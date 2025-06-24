package analysis1

import parse.Ast
import vfs.VfsNode
import errors.*
import QueryTypes.*

// VFS分析器 - 负责分析虚拟文件系统结构并构建模块层次
class VfsAnalyzer(engine: QueryEngine):
  val scopeManager: ScopeManager = engine.scopeManager

  def buildModuleStructure(): Either[FlurryError, ModuleStructureResult] =
    // 查找src目录
    val srcDirectory = engine.vfsInstance.root.children.flatMap(_.find(_.kind == vfs.VfsNodeKind.SrcDirectory)) match
      case Some(src) => src
      case None => return Left(NameResolutionError("No src directory found"))

    for
      srcResult <- engine.execute[VfsNodeResult](ProcessVfsNodeQuery(srcDirectory, engine.mainPackageScope))
      projectHir = Hir.ProjectDef(engine.vfsInstance.projectPath, engine.projectScope).asInstanceOf[Hir.ProjectDef]
    yield ModuleStructureResult(projectHir)

  def processVfsNode(node: VfsNode, parentScope: ScopeId): Either[FlurryError, VfsNodeResult] = node.kind match
    case vfs.VfsNodeKind.File => processFileNode(node, parentScope)

    case vfs.VfsNodeKind.SrcDirectory => processSrcDirectoryNode(node, parentScope)

    case vfs.VfsNodeKind.Directory => processDirectoryNode(node, parentScope)

    case vfs.VfsNodeKind.Root => processRootNode(node, parentScope)

  private def processFileNode(node: VfsNode, parentScope: ScopeId): Either[FlurryError, VfsNodeResult] =
    // 文件节点：创建模块作用域并分析文件
    val fileName = if node.name.endsWith(".fl") then node.name.dropRight(3) else node.name
    val moduleScope = scopeManager.createScope(fileName, Some(parentScope))

    for
      fileResult <- engine.execute[FileAnalysisResult](AnalyzeFileQuery(node, moduleScope))
      moduleHir = Hir.ModuleDef(fileName, moduleScope).asInstanceOf[Hir.ModuleDef]
    yield VfsNodeResult(moduleScope, moduleHir)

  private def processSrcDirectoryNode(node: VfsNode, parentScope: ScopeId): Either[FlurryError, VfsNodeResult] =
    // Src目录节点：查找 lib.fl 或 main.fl 作为包的主要内容
    // src目录本身不创建scope，内容直接属于parent scope（main包）
    val children = node.children.getOrElse(List.empty)

    // 查找包直属文件：lib.fl 或 main.fl
    val packageFile = children.find(child => child.name == "lib.fl" || child.name == "main.fl")

    // 获取包直属定义
    val packageDefinitionsResult = packageFile match
      case Some(file) =>
        // 有包直属文件，分析该文件获取定义
        engine.execute[FileAnalysisResult](AnalyzeFileQuery(file, parentScope)).map(_.definitions)
      case None =>
        // 没有包直属文件，使用空定义列表
        Right(List.empty[Hir])

    // 处理子模块（排除包直属文件）
    val childModules = children.filter(child =>
      packageFile.forall(_.name != child.name) && // 排除包直属文件
        (child.kind == vfs.VfsNodeKind.Directory || (child.kind == vfs.VfsNodeKind.File && child.name.endsWith(".fl")))
    )

    val childResults = childModules.foldLeft(Right(List.empty[Hir]): Either[FlurryError, List[Hir]]) { (acc, child) =>
      for
        results <- acc
        childResult <- engine.execute[VfsNodeResult](ProcessVfsNodeQuery(child, parentScope))
      yield results :+ childResult.moduleHir
    }

    for
      packageDefinitions <- packageDefinitionsResult
      childModuleHirs <- childResults
      // 合并包直属定义和子模块
      allDefinitions = packageDefinitions ++ childModuleHirs
      // TODO: 需要一个更严谨的方式来定义main package的名字
      moduleHir = Hir.PackageDef(node.name, parentScope)
    yield VfsNodeResult(parentScope, moduleHir)

  private def processDirectoryNode(node: VfsNode, parentScope: ScopeId): Either[FlurryError, VfsNodeResult] =
    // 普通目录节点：查找 mod.fl 作为模块的主要内容
    val moduleScope = scopeManager.createScope(node.name, Some(parentScope))
    val children = node.children.getOrElse(List.empty)

    // 查找模块直属文件：mod.fl
    val moduleFile = children.find(_.name == "mod.fl")

    // 获取模块直属定义
    val moduleDefinitionsResult = moduleFile match
      case Some(file) =>
        // 有模块直属文件，分析该文件获取定义
        engine.execute[FileAnalysisResult](AnalyzeFileQuery(file, moduleScope)).map(_.definitions)
      case None =>
        // 没有模块直属文件，使用空定义列表
        Right(List.empty[Hir])

    // 处理子模块（排除模块直属文件）
    val childModules = children.filter(child =>
      moduleFile.forall(_.name != child.name) && // 排除模块直属文件
        (child.kind == vfs.VfsNodeKind.Directory || (child.kind == vfs.VfsNodeKind.File && child.name.endsWith(".fl")))
    )

    val childResults = childModules.foldLeft(Right(List.empty[Hir]): Either[FlurryError, List[Hir]]) { (acc, child) =>
      for
        results <- acc
        childResult <- engine.execute[VfsNodeResult](ProcessVfsNodeQuery(child, moduleScope))
      yield results :+ childResult.moduleHir
    }

    for
      moduleDefinitions <- moduleDefinitionsResult
      childModuleHirs <- childResults
      // 合并模块直属定义和子模块
      allDefinitions = moduleDefinitions ++ childModuleHirs
      moduleHir = Hir.ModuleDef(node.name, moduleScope, allDefinitions)
    yield VfsNodeResult(moduleScope, moduleHir)

  private def processRootNode(node: VfsNode, parentScope: ScopeId): Either[FlurryError, VfsNodeResult] =
    // 根节点：直接处理子节点
    val childResults = node.children.getOrElse(List.empty)
      .foldLeft(Right(List.empty[Hir]): Either[FlurryError, List[Hir]]) { (acc, child) =>
        for
          results <- acc
          childResult <- engine.execute[VfsNodeResult](ProcessVfsNodeQuery(child, parentScope))
        yield results :+ childResult.moduleHir
      }

    childResults.map(modules => VfsNodeResult(parentScope, Hir.ModuleDef("root", parentScope, modules)))

  def analyzeFile(file: VfsNode, scope: ScopeId): Either[FlurryError, FileAnalysisResult] =
    // 确保文件有AST
    file.ensureAst() match
      case Left(e) => return Left(NameResolutionError(s"Failed to parse file ${file.absolutePath()}: $e"))
      case Right(_) => // continue
    file.ast match
      case Some(ast) =>
        for
          definitionsResult <- engine.execute[NamespaceResult](BuildNamespaceQuery(List(ast), scope, file))
          definitions = definitionsResult.definitions.values.toList
        yield FileAnalysisResult(definitions)

      case None => Left(NameResolutionError(s"No AST found for file ${file.absolutePath()}"))

  def extractFileDefinitions(file: VfsNode, scope: ScopeId): Either[FlurryError, FileAnalysisResult] =
    // 这个方法专门用于提取文件中的顶层定义，不进行完整分析
    file.ensureAst() match
      case Left(e) => return Left(NameResolutionError(s"Failed to parse file: $e"))
      case Right(_) => // continue
    file.ast match
      case Some(ast) =>
        val definitions = extractTopLevelDefinitions(ast, scope, file)
        Right(FileAnalysisResult(definitions))
      case None => Left(NameResolutionError("No AST found"))

  def buildNamespace(items: List[Ast], scope: ScopeId, file: VfsNode): Either[FlurryError, NamespaceResult] =
    val definitions = items
      .foldLeft(Right(Map.empty[String, Hir]): Either[FlurryError, Map[String, Hir]]) { (acc, item) =>
        for
          defsMap <- acc
          newDefs <- processNamespaceItem(item, scope, file)
        yield defsMap ++ newDefs
      }

    definitions.map(NamespaceResult.apply)

  private def extractTopLevelDefinitions(ast: Ast, scope: ScopeId, file: VfsNode): List[Hir] = ast match
    case Ast.FileScope(items) => items.toList.flatMap(extractTopLevelDefinitions(_, scope, file))
    case Ast.ModuleDef(id, clauses, items) =>
      val moduleScope = scopeManager.createScope(id, Some(scope))
      val childDefinitions = items.toList.flatMap(extractTopLevelDefinitions(_, moduleScope, file))
      List(Hir.ModuleDef(id, moduleScope, childDefinitions))
    case Ast.StructDef(id, clauses, body) =>
      val structScope = scopeManager.createScope(id, Some(scope))
      val _ = body.toList.flatMap(extractTopLevelDefinitions(_, structScope, file))
      List(Hir.StructDef(id, structScope))
    case Ast.EnumDef(id, clauses, body) =>
      val enumScope = scopeManager.createScope(id, Some(scope))
      val childDefinitions = body.toList.flatMap(extractTopLevelDefinitions(_, enumScope, file))
      List(Hir.EnumDef(id, enumScope, childDefinitions))
    case Ast.UnionDef(id, clauses, body) =>
      val unionScope = scopeManager.createScope(id, Some(scope))
      val childDefinitions = body.toList.flatMap(extractTopLevelDefinitions(_, unionScope, file))
      List(Hir.UnionDef(id, unionScope))
    case Ast.TraitDef(id, clauses, body) =>
      val traitScope = scopeManager.createScope(id, Some(scope))
      val childDefinitions = body.toList.flatMap(extractTopLevelDefinitions(_, traitScope, file))
      List(Hir.TraitDef(id, traitScope))
    case Ast.FunctionDef(id, params, returnType, clauses, body) =>
      val funcScope = scopeManager.createScope(id, Some(scope))
      List(Hir.FunctionDef(id))
    case _ => List.empty

  private def processNamespaceItem(item: Ast, scope: ScopeId, file: VfsNode): Either[FlurryError, Map[String, Hir]] =
    val awaitToAnalyze = Hir.AwaitingAnalysis.withAst(file, item)
    println(s"Processing namespace item: $item in scope: $scope")
    item match
      case Ast.ModuleDef(id, clauses, items) =>
        val moduleScope = scopeManager.createScope(id, Some(scope))
        val childDefinitions = items.toList.flatMap(extractTopLevelDefinitions(_, moduleScope, file))

        // 在作用域中注册模块
        scopeManager.addSymbol(scope, Symbol(id, awaitToAnalyze))
        Right(Map(id -> awaitToAnalyze))

      case Ast.StructDef(id, clauses, body) =>
        val structScope = scopeManager.createScope(id, Some(scope))
        val childDefinitions = body.toList.flatMap(extractTopLevelDefinitions(_, structScope, file))

        scopeManager.addSymbol(scope, Symbol(id, awaitToAnalyze))
        Right(Map(id -> awaitToAnalyze))

      case Ast.EnumDef(id, clauses, body) =>
        val enumScope = scopeManager.createScope(id, Some(scope))
        val childDefinitions = body.toList.flatMap(extractTopLevelDefinitions(_, enumScope, file))

        scopeManager.addSymbol(scope, Symbol(id, awaitToAnalyze))
        Right(Map(id -> awaitToAnalyze))

      case Ast.UnionDef(id, clauses, body) =>
        val unionScope = scopeManager.createScope(id, Some(scope))
        val childDefinitions = body.toList.flatMap(extractTopLevelDefinitions(_, unionScope, file))

        scopeManager.addSymbol(scope, Symbol(id, awaitToAnalyze))
        Right(Map(id -> awaitToAnalyze))

      case Ast.TraitDef(id, clauses, body) =>
        val traitScope = scopeManager.createScope(id, Some(scope))
        val childDefinitions = body.toList.flatMap(extractTopLevelDefinitions(_, traitScope, file))

        scopeManager.addSymbol(scope, Symbol(id, awaitToAnalyze))
        Right(Map(id -> awaitToAnalyze))

      case Ast.FunctionDef(id, params, returnType, clauses, body) =>
        scopeManager.addSymbol(scope, Symbol(id, awaitToAnalyze))
        Right(Map(id -> awaitToAnalyze))

      case Ast.FileScope(items) => items.toList
          .foldLeft(Right(Map.empty[String, Hir]): Either[FlurryError, Map[String, Hir]]) { (acc, subItem) =>
            for
              defsMap <- acc
              newDefs <- processNamespaceItem(subItem, scope, file)
            yield defsMap ++ newDefs
          }

      case _ => Right(Map.empty)
