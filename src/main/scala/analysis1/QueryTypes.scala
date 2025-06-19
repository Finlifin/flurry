package analysis1

import scala.collection.mutable
import vfs.VfsNode
import parse.Ast
import errors.*

// 查询键的基础trait
sealed trait QueryKey:
  def location: Option[AstLocation] = None

// 查询结果的基础trait
sealed trait QueryResult

// 查询状态枚举
enum QueryStatus:
  case NotStarted
  case InProgress
  case Completed
  case Failed(error: FlurryError)

// 编译错误类型 - 现在继承FlurryError
case class NameResolutionError(message: String, location: Option[AstLocation] = None, override val code: Int = 3001)
    extends FlurryError:
  override def errorMessage: String = message
  override def severity: Severity = Severity.Error

case class CyclicDependencyError(cycle: List[QueryKey], override val code: Int = 3002) extends FlurryError:
  override def errorMessage: String = s"Cyclic dependency detected: ${cycle.mkString(" -> ")}"
  override def severity: Severity = Severity.Error

case class ComptimeError(message: String, location: Option[AstLocation] = None, override val code: Int = 3003)
    extends FlurryError:
  override def errorMessage: String = message
  override def severity: Severity = Severity.Error

case class TypeResolutionError(message: String, location: Option[AstLocation] = None, override val code: Int = 3004)
    extends FlurryError:
  override def errorMessage: String = message
  override def severity: Severity = Severity.Error

case class TraitResolutionError(message: String, location: Option[AstLocation] = None, override val code: Int = 3005)
    extends FlurryError:
  override def errorMessage: String = message
  override def severity: Severity = Severity.Error

// 具体的查询类型
object QueryTypes:
  // Analysis Queries
  case class AnalysisQuery(ast: AstLocation, scopeId: ScopeId) extends QueryKey
  case class AnalysisResult(hir: Hir) extends QueryResult

  // Name Resolution Queries
  case class ResolveNameQuery(name: String, scope: ScopeId) extends QueryKey
  case class ResolvePathQuery(path: List[String], scope: ScopeId) extends QueryKey
  case class NameResolutionResult(symbol: Symbol, scope: Option[ScopeId]) extends QueryResult

  case class ResolveScopeQuery(ast: Ast, file: VfsNode) extends QueryKey
  case class ScopeResult(scope: ScopeId) extends QueryResult

  case class BuildScopeTreeQuery(module: Ast, file: VfsNode) extends QueryKey

  // Type Resolution Queries
  case class TypingQuery(expr: AstLocation, scope: ScopeId, expected: Option[Hir] = None) extends QueryKey
  // 第一个Hir是类型，第二个Hir是类型化后的表达式
  case class TypeResult(hir: (Hir, Hir)) extends QueryResult

  case class ResolveTypeQuery(typeAst: AstLocation, scope: ScopeId) extends QueryKey

  case class SubtypeQuery(subtype: Hir, supertype: Hir) extends QueryKey
  case class SubtypeResult(isSubtype: Boolean) extends QueryResult

  case class UnifyTypesQuery(type1: Hir, type2: Hir) extends QueryKey
  case class UnificationResult(unifiedType: Hir, substitutions: Map[String, Hir]) extends QueryResult

  // Trait Resolution Queries
  case class ResolveTraitQuery(traitName: String, scope: ScopeId) extends QueryKey

  case class CheckTraitBoundQuery(typ: Hir, traitBound: Hir) extends QueryKey

  case class FindTraitImplementationQuery(traitType: Hir, forType: Hir) extends QueryKey
  case class TraitImplementationResult(implementation: Option[Hir]) extends QueryResult

  // HIR Construction Queries
  case class AstToHirQuery(ast: Ast, context: TypeContext, file: VfsNode) extends QueryKey
  case class HirResult(hir: Hir) extends QueryResult

  case class ProcessDefinitionQuery(defAst: Ast, scope: ScopeId, file: VfsNode) extends QueryKey

  // Compile-time Computation Queries
  // case class EvaluateComptimeQuery(hir: Hir, context: ComptimeContext) extends QueryKey
  // case class ComptimeTypeQuery(hir: Hir, context: ComptimeContext) extends QueryKey
  case class ComptimeResult(value: Hir) extends QueryResult

  // VFS Analysis Queries - 新增VFS分析查询
  case class BuildModuleStructureQuery(vfsInstance: vfs.Vfs) extends QueryKey
  case class ModuleStructureResult(rootScope: ScopeId, projectHir: Hir.ProjectDef) extends QueryResult

  case class ProcessVfsNodeQuery(node: vfs.VfsNode, parentScope: ScopeId) extends QueryKey
  case class VfsNodeResult(scope: ScopeId, moduleHir: Hir) extends QueryResult

  case class AnalyzeFileQuery(file: vfs.VfsNode, scope: ScopeId) extends QueryKey
  case class FileAnalysisResult(definitions: List[Hir]) extends QueryResult

  case class ExtractFileDefinitionsQuery(file: vfs.VfsNode, scope: ScopeId) extends QueryKey

  case class BuildNamespaceQuery(items: List[Ast], scope: ScopeId, file: vfs.VfsNode) extends QueryKey
  case class NamespaceResult(definitions: Map[String, Hir]) extends QueryResult

// 类型推导上下文
case class TypeContext(
    expected: Option[Hir] = None,
    scope: ScopeId,
    enclosingFunction: Option[Hir.FunctionDef] = None,
    typeVars: Map[String, Hir] = Map(),
    constraints: List[TypeConstraint] = Nil
):
  def withExpected(ty: Hir): TypeContext = copy(expected = Some(ty))
  def withScope(newScope: ScopeId): TypeContext = copy(scope = newScope)
  def withTypeVar(name: String, ty: Hir): TypeContext = copy(typeVars = typeVars + (name -> ty))
  def addConstraint(constraint: TypeConstraint): TypeContext = copy(constraints = constraints :+ constraint)

// 类型约束
sealed trait TypeConstraint
case class EqualityConstraint(type1: Hir, type2: Hir) extends TypeConstraint
case class SubtypeConstraint(subtype: Hir, supertype: Hir) extends TypeConstraint
case class TraitBoundConstraint(typ: Hir, traitBound: Hir) extends TypeConstraint

// // 编译时计算上下文
// case class ComptimeContext(scope: ScopeId, values: Map[String, Hir] = Map(), types: Map[String, Hir] = Map()):
//   def withValue(name: String, value: Hir): ComptimeContext = copy(values = values + (name -> value))
//   def withType(name: String, typ: Hir): ComptimeContext = copy(types = types + (name -> typ))

// 查询缓存和依赖跟踪
class QueryCache:
  private val cache = mutable.Map[QueryKey, QueryResult]()
  private val status = mutable.Map[QueryKey, QueryStatus]()
  private val dependencies = mutable.Map[QueryKey, mutable.Set[QueryKey]]()
  private val dependents = mutable.Map[QueryKey, mutable.Set[QueryKey]]()
  private val queryStack = mutable.Stack[QueryKey]()

  def get(key: QueryKey): Option[QueryResult] = cache.get(key)

  def put(key: QueryKey, result: QueryResult): Unit =
    cache(key) = result
    status(key) = QueryStatus.Completed

  def setStatus(key: QueryKey, newStatus: QueryStatus): Unit = status(key) = newStatus

  def getStatus(key: QueryKey): QueryStatus = status.getOrElse(key, QueryStatus.NotStarted)

  def addDependency(dependent: QueryKey, dependency: QueryKey): Unit =
    dependencies.getOrElseUpdate(dependent, mutable.Set()) += dependency
    dependents.getOrElseUpdate(dependency, mutable.Set()) += dependent

  def checkCycle(key: QueryKey): Option[List[QueryKey]] =
    if queryStack.contains(key) then
      val cycleStart = queryStack.indexOf(key)
      Some(queryStack.drop(cycleStart).toList :+ key)
    else None

  def pushQuery(key: QueryKey): Unit = queryStack.push(key)

  def popQuery(): Unit = if queryStack.nonEmpty then queryStack.pop()

  def invalidate(key: QueryKey): Unit =
    cache.remove(key)
    status.remove(key)
    // 递归invalidate所有依赖这个查询的其他查询
    dependents.get(key).foreach(deps => deps.foreach(invalidate))
