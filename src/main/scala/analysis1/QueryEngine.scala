package analysis1

import parse.Ast
import vfs.VfsNode
import errors.*
import QueryTypes.*
import scala.concurrent.Future
import scala.util.chaining.*
import vfs.Vfs

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

          // VFS Analysis - 新增VFS分析查询处理
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

  private def analyzeQuery(query: AnalysisQuery): Either[FlurryError, AnalysisResult] =
    Analyzer(query.ast, scopeId = query.scopeId, engine = this).analyze()

  private def resolveNameQuery(query: ResolveNameQuery): Either[FlurryError, NameResolutionResult] =
    // 处理特殊id
    query.name match
      case "null" => Right(NameResolutionResult(Symbol("null", Hir.NullVal), None))
      case "undefined" => Right(NameResolutionResult(Symbol("undefined", Hir.UndefinedVal), None))
      case _ => scopeManager.resolve(query.name, query.scope) match
          case Some((symbol, foundScope)) if symbol.hir == Hir.AwaitingAnalysis =>
            // 如果是未解析的符号，自动执行该符号的解析逻辑
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
              case Left(error) => Left(error)
          case Some((symbol, foundScope)) => Right(NameResolutionResult(symbol, foundScope))
          case None => Left(NameResolutionError(s"Undefined name: ${query.name}", query.location))

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
    val result = query.expected match
      case Some(expectedType) => ContextualTyping.checkType(query.expr, expectedType, this, query.scope)
      case None => ContextualTyping.inferType(query.expr, this, query.scope)

    result match
      case Right(hirType) => Right(TypeResult(hirType))
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

  // HIR Construction Implementation - 使用 AstToHirTranslator.scala 中的实现
  // private def astToHirQuery(query: AstToHirQuery): Either[FlurryError, HirResult] =
  //   given QueryEngine = this
  //   val translator = AstToHirTranslator()
  //   val context = TranslationContext(
  //     engine = this,
  //     scopeManager = scopeManager,
  //     currentScope = query.context.scope,
  //     currentFile = query.file,
  //     globalContext = GlobalCompilationContext(DiagnosticsReporter())
  //   )
  //   translator.translate(query.ast, context).map(HirResult.apply)

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
    vfsAnalyzer.buildModuleStructure(query.vfsInstance)

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
