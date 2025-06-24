package analysis1

import scala.collection.mutable
import java.{util => ju}

class ScopeManager:
  private val scopes: mutable.Map[ScopeId, Scope] = mutable.Map()
  private var nextId: Int = 0

  def createScope(name: String, parent: Option[ScopeId] = None, ordered: Boolean = false): ScopeId =
    val id = ScopeId(nextId)
    nextId += 1
    val scope = Scope(name, parent, ordered)
    scopes(id) = scope
    parent.foreach(addChild(_, id))
    id

  def getScope(id: ScopeId): Option[Scope] = scopes.get(id)
  def setHir(scopeId: ScopeId, hir: Hir): Unit = scopes.get(scopeId) match
    case Some(scope) => scope.hir = Some(hir)
    case None => throw ju.NoSuchElementException(s"Scope $scopeId does not exist")

  def addChild(parentId: ScopeId, childId: ScopeId): Unit = scopes.get(parentId) match
    case Some(parent) if parent.ordered => parent.children += childId
    case Some(parent) =>
      // 如果不是有序的，检查唯一性
      if !parent.children.contains(childId) then parent.children += childId
    case None => throw ju.NoSuchElementException(s"Scope $parentId does not exist")

  def addSymbol(scopeId: ScopeId, symbol: Symbol): Unit = scopes.get(scopeId) match
    case Some(scope) if scope.ordered =>
      // 有序作用域，直接添加到末尾
      scope.orderedSymbols += symbol
    case Some(scope) =>
      // 非有序作用域，检查唯一性
      if !scope.symbolMap.contains(symbol.name) then scope.symbolMap(symbol.name) = symbol
      else throw IllegalArgumentException(s"Symbol ${symbol.name} already exists in scope ${scope.name}")
    case None => throw ju.NoSuchElementException(s"Scope $scopeId does not exist")

  def updateSymbol(scopeId: ScopeId, symbol: Symbol): Unit = scopes.get(scopeId) match
    case Some(scope) if scope.ordered =>
      // 有序作用域，从末尾往前查找并替换
      val index = scope.orderedSymbols.lastIndexWhere(_.name == symbol.name)
      if index >= 0 then
        // 保留原有的AST信息
        val originalSymbol = scope.orderedSymbols(index)
        val updatedSymbol = symbol.copy(hir = symbol.hir.withAst(originalSymbol.hir.ast))
        scope.orderedSymbols(index) = updatedSymbol
      else
        // symbol没找到，尝试在children中找scope并更新
        val childOpt = scope.children.iterator.flatMap(childId => scopes.get(childId)).find(_.name == symbol.name)
        childOpt match
          case Some(childScope) =>
            // 保留原有的AST信息
            val originalAst = childScope.hir.flatMap(_.ast)
            childScope.hir = Some(symbol.hir.withAst(originalAst))
          case None =>
            throw ju.NoSuchElementException(s"Symbol or scope ${symbol.name} does not exist in scope ${scope.name}")
    case Some(scope) =>
      // 非有序作用域，检查存在性并更新
      if scope.symbolMap.contains(symbol.name) then
        // 保留原有的AST信息
        val originalSymbol = scope.symbolMap(symbol.name)
        val updatedSymbol = symbol.copy(hir = symbol.hir.withAst(originalSymbol.hir.ast))
        scope.symbolMap(symbol.name) = updatedSymbol
      else
        // symbol没找到，尝试在children中找scope并更新
        val childOpt = scope.children.iterator.flatMap(childId => scopes.get(childId)).find(_.name == symbol.name)
        childOpt match
          case Some(childScope) =>
            // 保留原有的AST信息
            val originalAst = childScope.hir.flatMap(_.ast)
            childScope.hir = Some(symbol.hir.withAst(originalAst))
          case None =>
            throw ju.NoSuchElementException(s"Symbol or scope ${symbol.name} does not exist in scope ${scope.name}")
    case None => throw ju.NoSuchElementException(s"Scope $scopeId does not exist")

  def resolve(name: String, scopeId: ScopeId): Option[(Symbol, Option[ScopeId])] = scopes.get(scopeId) match
    // 从后往前找symbol
    case Some(scope) => lookup(name, scopeId) match
        case Some((symbol, foundScope)) => Some((symbol, foundScope))
        // 如果在当前作用域没有找到symbol，检查uses
        case None => scope.uses.lookup(name, (namespace, name) => lookup(name, namespace).map(_._1)) match
            case Some(symbol) => Some((symbol, None)) // uses 导入的符号视为当前作用域
            // 如果没有在当前作用域找到symbol，递归查找父作用域
            case None => scope.parent match
                case Some(parentId) => resolve(name, parentId)
                case None => None
    case None => None

  def lookup(name: String, scopeId: ScopeId): Option[(Symbol, Option[ScopeId])] = scopes.get(scopeId) match
    case Some(scope) if scope.ordered =>
      // 首先在当前作用域查找symbol
      scope.orderedSymbols.reverseIterator.find(_.name == name) match
        case Some(symbol) => Some((symbol, None)) // 在当前作用域找到
        case None =>
          // 查找子作用域作为symbol
          scope.children.iterator.flatMap(childId =>
            scopes.get(childId).filter(_.name == name).map { childScope =>
              // 将子作用域包装成Symbol返回
              val scopeSymbol = Symbol(name, childScope.hir.getOrElse(Hir.Invalid))
              (scopeSymbol, Some(childId))
            }
          ).nextOption()

    case Some(scope) =>
      // 首先在当前作用域查找symbol
      scope.symbolMap.get(name) match
        case Some(symbol) => Some((symbol, None)) // 在当前作用域找到
        case None =>
          // 查找子作用域作为symbol
          scope.children.iterator.flatMap(childId =>
            scopes.get(childId).filter(_.name == name).map { childScope =>
              // 将子作用域包装成Symbol返回
              val scopeSymbol = Symbol(name, childScope.hir.getOrElse(Hir.Invalid))
              (scopeSymbol, Some(childId))
            }
          ).nextOption()
    case None => None

case class ScopeId(id: Int):
  override def toString: String = s"(ScopeId $id)"

class Scope(val name: String, val parent: Option[ScopeId] = None, val ordered: Boolean = false):
  val children: mutable.ListBuffer[ScopeId] = mutable.ListBuffer()
  val uses: mutable.ListBuffer[UseImport] = mutable.ListBuffer()
  var hir: Option[Hir] = None

  // 根据ordered属性使用不同的数据结构
  val orderedSymbols: mutable.ListBuffer[Symbol] = if ordered then mutable.ListBuffer() else null
  val symbolMap: mutable.Map[String, Symbol] = if !ordered then mutable.Map() else null

  def getAllSymbols: Iterable[Symbol] = if ordered then orderedSymbols else symbolMap.values

  override def toString: String =
    val symbolsStr = if ordered then orderedSymbols.mkString(", ") else symbolMap.values.mkString(", ")
    s"(Scope $name ${parent.map(_.toString).getOrElse("None")} (children ${children.mkString(", ")}) (symbols $symbolsStr))"

case class Symbol(name: String, var hir: Hir):
  override def toString: String = s"(Symbol $name $hir)"

enum UseImport:
  case UseAll(namespace: ScopeId)
  case UseAllExcept(namespace: ScopeId, except: List[String])
  case UseOnly(namespace: ScopeId, only: List[String])
  case UseThis(namespace: ScopeId, thisName: String, asName: String)

extension (uses: mutable.ListBuffer[UseImport])
  def lookup(name: String, namespaceLookup: (ScopeId, String) => Option[Symbol]): Option[Symbol] = uses.iterator
    .flatMap:
      case UseImport.UseAll(namespace) => namespaceLookup(namespace, name)
      case UseImport.UseAllExcept(namespace, except) if !except.contains(name) => namespaceLookup(namespace, name)
      case UseImport.UseOnly(namespace, only) if only.contains(name) => namespaceLookup(namespace, name)
      case UseImport.UseThis(namespace, thisName, asName) if asName == name => namespaceLookup(namespace, thisName)
      case _ => Iterator.empty
    .nextOption()
