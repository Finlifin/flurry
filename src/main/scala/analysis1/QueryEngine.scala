package analysis1

import parse.Ast
import vfs.VfsNode
import errors.*
import QueryTypes.*
import scala.concurrent.Future
import scala.util.chaining.*
import vfs.Vfs
import scala.annotation.init

// 查询执行结果
enum QueryExecutionResult[+T <: QueryResult]:
  case Success(result: T)
  case Failure(error: FlurryError)
  case InProgress
  case NotStarted

// 查询引擎 - 使用现代Scala 3特性
class QueryEngine(var vfsInstance: Vfs):
  private val cache = QueryCache()
  var scopeManager: ScopeManager = ScopeManager()
  val projectScope: ScopeId = scopeManager.createScope("project")
  var mainPackageScope: ScopeId = null
  // 创建VFS分析器实例 - 延迟初始化以避免循环依赖
  private lazy val vfsAnalyzer = VfsAnalyzer(this)

  // 主查询执行方法 - 使用上下文函数和现代错误处理
  def execute[T <: QueryResult](query: QueryKey): Either[FlurryError, T] = cache.checkCycle(query) match
    case Some(cycle) => Left(CyclicDependencyError(cycle))
    case None => cache.get(query) match
        case Some(result) => Right(result.asInstanceOf[T])
        case None => executeQuery(query).map(_.asInstanceOf[T])

  // 异步执行查询（为未来扩展准备）
  def executeAsync[T <: QueryResult](query: QueryKey): Future[Either[FlurryError, T]] = Future
    .successful(execute(query))

  // 批量执行查询
  def executeBatch[T <: QueryResult](queries: List[QueryKey]): Either[FlurryError, List[T]] = queries
    .foldLeft(Right(List.empty[T]): Either[FlurryError, List[T]]) { (acc, query) =>
      for
        results <- acc
        result <- execute[T](query)
      yield results :+ result
    }

  private def executeQuery(query: QueryKey): Either[FlurryError, QueryResult] =
    cache.setStatus(query, QueryStatus.InProgress)
    cache.pushQuery(query)

    val result =
      try
        query match
          case q: AnalysisQuery => analyzeQuery(q)

          // AST to HIR Translation
          // case q: AstToHirQuery => astToHirQuery(q)

          // Name Resolution - 使用现代模式匹配
          case q: ResolveNameQuery => resolveNameQuery(q)
          case q: ResolvePathQuery => resolvePathQuery(q)
          case q: ResolveScopeQuery => resolveScopeQuery(q)
          case q: BuildScopeTreeQuery => buildScopeTreeQuery(q)

          // Type Resolution - 使用上下文参数传递
          case q: TypingQuery => typingQuery(q)
          case q: ResolveTypeQuery => resolveTypeQuery(q)
          case q: SubtypeQuery => subtypeQuery(q)
          case q: UnifyTypesQuery => unifyTypesQuery(q)

          // Trait Resolution
          case q: ResolveTraitQuery => resolveTraitQuery(q)
          case q: CheckTraitBoundQuery => checkTraitBoundQuery(q)
          case q: FindTraitImplementationQuery => findTraitImplementationQuery(q)

          // HIR Construction
          // case q: AstToHirQuery => astToHirQuery(q)
          case q: ProcessDefinitionQuery => processDefinitionQuery(q)

          // Compile-time Computation
          // case q: EvaluateComptimeQuery => evaluateComptimeQuery(q)
          // case q: ComptimeTypeQuery => comptimeTypeQuery(q)

          case q: InitilizeBuiltinsQuery => initBuiltinScopes(q)
          case q: BuildModuleStructureQuery => buildModuleStructureQuery(q)
          case q: ProcessVfsNodeQuery => processVfsNodeQuery(q)
          case q: AnalyzeFileQuery => analyzeFileQuery(q)
          case q: ExtractFileDefinitionsQuery => extractFileDefinitionsQuery(q)
          case q: BuildNamespaceQuery => buildNamespaceQuery(q)
      catch
        case e: FlurryError =>
          cache.setStatus(query, QueryStatus.Failed(e))
          Left(e)
        case e: Exception =>
          val flurryError = ComptimeError(s"Unexpected error: ${e.getMessage}")
          cache.setStatus(query, QueryStatus.Failed(flurryError))
          e.printStackTrace()
          Left(flurryError)

    cache.popQuery()

    result.tap {
      case Right(queryResult) => cache.put(query, queryResult)
      case Left(_) => // Error already handled above
    }

  // 初始化math, builtin, meta, build, std, attr, main
  def initBuiltinScopes(query: InitilizeBuiltinsQuery): Either[FlurryError, BuiltinsResult] =
    // 初始化内置作用域
    val builtin = scopeManager.createScope("builtin", Some(projectScope))
    mainPackageScope = scopeManager.createScope(query.mainPackageName, Some(projectScope))
    scopeManager.createScope("math", Some(projectScope))
    scopeManager.createScope("meta", Some(projectScope))
    scopeManager.createScope("build", Some(projectScope))
    scopeManager.createScope("std", Some(projectScope))
    scopeManager.createScope("attr", Some(projectScope))

    scopeManager.setHir(scopeManager.createScope("Integer", Some(builtin)), Hir.TypeInteger)
    scopeManager.setHir(scopeManager.createScope("Real", Some(builtin)), Hir.TypeReal)
    scopeManager.setHir(scopeManager.createScope("i8", Some(builtin)), Hir.TypeInt(8, true))
    scopeManager.setHir(scopeManager.createScope("i16", Some(builtin)), Hir.TypeInt(16, true))
    scopeManager.setHir(scopeManager.createScope("i32", Some(builtin)), Hir.TypeInt(32, true))
    scopeManager.setHir(scopeManager.createScope("i64", Some(builtin)), Hir.TypeInt(64, true))
    scopeManager.setHir(scopeManager.createScope("isize", Some(builtin)), Hir.TypeInt(64, true))
    scopeManager.setHir(scopeManager.createScope("i128", Some(builtin)), Hir.TypeInt(128, true))
    scopeManager.setHir(scopeManager.createScope("u8", Some(builtin)), Hir.TypeInt(8, false))
    scopeManager.setHir(scopeManager.createScope("u16", Some(builtin)), Hir.TypeInt(16, false))
    scopeManager.setHir(scopeManager.createScope("u32", Some(builtin)), Hir.TypeInt(32, false))
    scopeManager.setHir(scopeManager.createScope("u64", Some(builtin)), Hir.TypeInt(64, false))
    scopeManager.setHir(scopeManager.createScope("usize", Some(builtin)), Hir.TypeInt(64, false))
    scopeManager.setHir(scopeManager.createScope("u128", Some(builtin)), Hir.TypeInt(128, false))
    scopeManager.setHir(scopeManager.createScope("f32", Some(builtin)), Hir.TypeFloat(32))
    scopeManager.setHir(scopeManager.createScope("f64", Some(builtin)), Hir.TypeFloat(64))
    scopeManager.setHir(scopeManager.createScope("f128", Some(builtin)), Hir.TypeFloat(128))
    scopeManager.setHir(scopeManager.createScope("bool", Some(builtin)), Hir.TypeBool)
    scopeManager.setHir(scopeManager.createScope("str", Some(builtin)), Hir.TypeStr)
    scopeManager.setHir(scopeManager.createScope("char", Some(builtin)), Hir.TypeChar)

    scopeManager.getScope(mainPackageScope) match
      case Some(mainScope) =>
        // 添加use builtin.*
        mainScope.uses.addOne(UseImport.UseAll(builtin))
      case None => throw new RuntimeException(s"Main scope $mainPackageScope not found")

    Right(BuiltinsResult())

  private def analyzeQuery(query: AnalysisQuery): Either[FlurryError, AnalysisResult] =
    Analyzer(query.ast, scopeId = query.scopeId, engine = this).analyze()

  private def resolveNameQuery(query: ResolveNameQuery): Either[FlurryError, NameResolutionResult] =
    // 处理特殊id
    query.name match
      case "null" => Right(NameResolutionResult(Symbol("null", Hir.NullVal), None))
      case "undefined" => Right(NameResolutionResult(Symbol("undefined", Hir.UndefinedVal), None))
      case _ =>
        println(s"Resolving name: ${query.name} in scope: ${query.scope}")
        scopeManager.resolve(query.name, query.scope) match
          case Some((symbol, foundScope)) if symbol.hir == Hir.AwaitingAnalysis =>
            // 如果是未解析的符号，自动执行该符号的解析逻辑
            println(s"Found awaiting analysis symbol: ${symbol.name}, starting analysis...")
            execute[AnalysisResult](AnalysisQuery(symbol.hir.ast.get, query.scope)) match
              case Right(AnalysisResult(hir)) =>
                // 保留原有的AST信息
                val originalAst = symbol.hir.ast
                val updatedHir = hir.withAst(originalAst)
                println(
                  s"Resolved awaiting analysis symbol: ${symbol.name} in scope: ${query.scope}, HIR: ${dumpHir(updatedHir)}, AST: ${originalAst
                      .getOrElse("None")}"
                )
                scopeManager.updateSymbol(query.scope, symbol.copy(hir = updatedHir))
                Right(NameResolutionResult(symbol.copy(hir = updatedHir), foundScope))
              case Left(error) =>
                println(s"Analysis failed for symbol ${symbol.name}: $error")
                Left(error)
          case Some((symbol, foundScope)) =>
            println(s"Found symbol: ${symbol.name} = ${dumpHir(symbol.hir)} in scope: $foundScope")
            Right(NameResolutionResult(symbol, foundScope))
          case None =>
            println(s"Failed to resolve name: ${query.name} in scope: ${query.scope}")
            Left(NameResolutionError(s"Undefined name: ${query.name}", query.location))

  // 路径解析查询
  // 从第一个id开始，逐级执行name resolve query
  private def resolvePathQuery(query: ResolvePathQuery): Either[FlurryError, NameResolutionResult] = query.path match
    case Nil => Left(NameResolutionError("Empty path in ResolvePathQuery"))
    case head :: tail =>
      // 使用循环逐级解析路径
      var currentResult = execute[NameResolutionResult](ResolveNameQuery(head, query.scope))
      var remainingPath = tail

      while currentResult.isRight && remainingPath.nonEmpty do
        val name = remainingPath.head
        remainingPath = remainingPath.tail
        currentResult = currentResult.flatMap { res =>
          // 如果上一步找到的是子作用域的符号，则在该子作用域中继续查找
          // TODO
          val nextScope = res.scope.getOrElse(query.scope)
          // println(s"query.scope: ${query.scope}, nextScope: $nextScope")
          execute[NameResolutionResult](ResolveNameQuery(name, nextScope))
        }

      currentResult

  private def resolveScopeQuery(query: ResolveScopeQuery): Either[FlurryError, ScopeResult] =
    // 根据AST节点确定其所属的作用域
    query.ast match
      case _: Ast.Block =>
        // 为 block 创建新作用域的逻辑
        Left(NameResolutionError("Scope resolution not implemented yet"))
      case _: Ast.FunctionDef =>
        // 为函数创建新作用域的逻辑
        Left(NameResolutionError("Function scope resolution not implemented yet"))
      case _ => Left(NameResolutionError("Scope resolution not implemented for this AST node"))

  private def buildScopeTreeQuery(query: BuildScopeTreeQuery): Either[FlurryError, ScopeResult] =
    // 为整个模块构建作用域树
    query.module match
      case Ast.ModuleDef(name, clauses, items) =>
        // 构建模块作用域树的逻辑
        Left(NameResolutionError("Module scope tree building not implemented yet"))
      case Ast.FileScope(items) =>
        // 构建文件作用域树的逻辑
        Left(NameResolutionError("File scope tree building not implemented yet"))
      case _ => Left(NameResolutionError("Invalid module AST for scope tree building"))

  private def typingQuery(query: TypingQuery): Either[FlurryError, TypeResult] =
    // 首先需要将AST翻译为HIR
    val analyzer = Analyzer(query.expr, query.scope, this)

    analyzer.translate() match
      case Right(hir) =>
        val result = query.expected match
          case Some(expectedType) => ContextualTyping
              .checkTypeOnHirWithExpectation(hir, expectedType, this, query.scope)
          case None => ContextualTyping.inferTypeOnHir(hir, this, query.scope)

        result match
          case Right(checkedHir) => Right(TypeResult((checkedHir, checkedHir))) // 类型和检查后的HIR
          case Left(error) => Left(error)
      case Left(error) => Left(error)

  private def resolveTypeQuery(query: ResolveTypeQuery): Either[FlurryError, TypeResult] =
    // TODO: 实现类型解析 - 使用独立的类型解析组件
    Left(TypeResolutionError("Type resolution not implemented yet"))

  private def subtypeQuery(query: SubtypeQuery): Either[FlurryError, SubtypeResult] =
    // TODO: 实现子类型检查 - 使用独立的子类型检查组件
    Right(SubtypeResult(false))

  private def unifyTypesQuery(query: UnifyTypesQuery): Either[FlurryError, UnificationResult] =
    // TODO: 实现类型统一 - 使用独立的类型统一组件
    Left(TypeResolutionError("Type unification not implemented yet"))

  private def resolveTraitQuery(query: ResolveTraitQuery): Either[FlurryError, NameResolutionResult] =
    scopeManager.resolve(query.traitName, query.scope) match
      case Some((symbol, foundScope)) => symbol.hir match
          case _: Hir.TraitDef => Right(NameResolutionResult(symbol, foundScope))
          case _ => Left(TraitResolutionError(s"${query.traitName} is not a trait"))
      case None => Left(TraitResolutionError(s"Trait not found: ${query.traitName}"))

  private def checkTraitBoundQuery(query: CheckTraitBoundQuery): Either[FlurryError, SubtypeResult] =
    // TODO: 实现trait约束检查 - 使用独立的trait检查组件
    Right(SubtypeResult(false))

  private def findTraitImplementationQuery(
      query: FindTraitImplementationQuery
  ): Either[FlurryError, TraitImplementationResult] =
    // TODO: 实现trait实现查找 - 使用独立的trait解析组件
    Right(TraitImplementationResult(None))

  private def processDefinitionQuery(query: ProcessDefinitionQuery): Either[FlurryError, HirResult] =
    given QueryEngine = this
    Left(NameResolutionError("Definition processing not implemented yet"))
    // DefinitionProcessor().processDefinition(query.defAst, query.scope, query.file).map(HirResult.apply)

  // // Compile-time Computation Implementation - 使用 ComptimeComputation.scala 中的实现
  // private def evaluateComptimeQuery(query: EvaluateComptimeQuery): Either[FlurryError, ComptimeResult] =
  //   // TODO: 实现编译时求值 - 使用独立的编译时计算组件
  //   Left(ComptimeError("Comptime evaluation not implemented yet"))

  // private def comptimeTypeQuery(query: ComptimeTypeQuery): Either[FlurryError, TypeResult] =
  //   // TODO: 实现编译时类型求值 - 使用独立的编译时计算组件
  //   Left(ComptimeError("Comptime type evaluation not implemented yet"))

  // VFS Analysis Implementation - 使用VfsAnalyzer组件
  private def buildModuleStructureQuery(query: BuildModuleStructureQuery): Either[FlurryError, ModuleStructureResult] =
    vfsAnalyzer.buildModuleStructure()

  private def processVfsNodeQuery(query: ProcessVfsNodeQuery): Either[FlurryError, VfsNodeResult] = vfsAnalyzer
    .processVfsNode(query.node, query.parentScope)

  private def analyzeFileQuery(query: AnalyzeFileQuery): Either[FlurryError, FileAnalysisResult] = vfsAnalyzer
    .analyzeFile(query.file, query.scope)

  private def extractFileDefinitionsQuery(query: ExtractFileDefinitionsQuery): Either[FlurryError, FileAnalysisResult] =
    vfsAnalyzer.extractFileDefinitions(query.file, query.scope)

  private def buildNamespaceQuery(query: BuildNamespaceQuery): Either[FlurryError, NamespaceResult] = vfsAnalyzer
    .buildNamespace(query.items, query.scope, query.file)

  def dumpHir(hir: Hir): String = hir match
    case Hir.ProjectDef(name, scopeId, _) => s"(project $name ${dumpScope(scopeId)})"
    case Hir.PackageDef(name, scopeId, _) => s"(package $name ${dumpScope(scopeId)})"

    case _ => hir.toString

  def dumpScope(scopeId: ScopeId): String = scopeManager.getScope(scopeId) match
    case Some(scope) =>
      s"(scope ${scope.name} ${scope.parent.map(_.toString).getOrElse("None")} (symbols ${scope.symbolMap
          .map((name, s) => dumpHir(s.hir)).mkString(" ")}) (children ${scope.children.map(dumpScope(_)).mkString(" ")}))"
    case None => s"(scope $scopeId not found)"
