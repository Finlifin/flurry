import parse.*
import vfs.*
import analysis1.ScopeManager
import analysis1.QueryEngine
// import analysis1.Analyzer
// import comptime.{Definition, PackageDefinition, ProjectDefinition}
// import comptime.toSExpr

@main
def main(): Unit =
  val vfs = Vfs("test_project")
  var vfs_buffer = java.io.PrintWriter("vfs.lisp")
  var ast_buffer = java.io.PrintWriter("ast.lisp")
  var hir_buffer = java.io.PrintWriter("hir.lisp")
  var fn_main_hir_buffer = java.io.PrintWriter("fn_main_hir.lisp")

  println("=== Testing VFS ===")
  vfs.dumpSExpr(vfs_buffer)
  vfs_buffer.flush()
  vfs_buffer.close()
  println("VFS dumped to vfs.lisp")

  println("\n=== Testing AST Parsing ===")
  val node = vfs.findNodeByPath("test_project/src/main.fl").get
  node.ensureAst()
  val sExpr = node.ast.get.toString()
  ast_buffer.println(sExpr)
  ast_buffer.flush()
  ast_buffer.close()
  println("AST dumped to ast.lisp")

  println("\n=== Testing New Query System ===")
  try
    // 创建查询引擎
    val queryEngine = QueryEngine(vfs)

    // 使用查询系统构建模块结构
    import analysis1.QueryTypes.*
    val moduleStructureQuery = BuildModuleStructureQuery(vfs)

    for
      initBuiltinsResult <- queryEngine.execute[BuiltinsResult](InitilizeBuiltinsQuery())
      result <- queryEngine.execute[ModuleStructureResult](moduleStructureQuery)
      _ = {
        println("Module structure built successfully")
        // 输出结果到HIR文件
        hir_buffer.println(queryEngine.dumpHir(result.projectHir))
        hir_buffer.flush()
        hir_buffer.close()
        println("HIR dumped to hir.lisp")
      }
      nameResolutionResult <- queryEngine
        .execute[NameResolutionResult](ResolvePathQuery(List("main", "damn"), queryEngine.projectScope))
      _ = {
        val scopeInfo = nameResolutionResult.scope.map(_.toString).getOrElse("current scope")
        println(s"Resolved 'main.damn' to symbol: ${nameResolutionResult.symbol.name} in scope: $scopeInfo")
        // 输出fn_main的HIR到单独文件
        val fnMainHir = queryEngine.dumpHir(nameResolutionResult.symbol.hir)
        fn_main_hir_buffer.println(fnMainHir)
        fn_main_hir_buffer.flush()
        fn_main_hir_buffer.close()
        println("Function main HIR dumped to fn_main_hir.lisp")
      }
    yield ()
  catch
    case e: Exception =>
      println(s"Error during analysis: ${e.getMessage}")
      e.printStackTrace()

  // println("\n=== Testing Analyzer ===")
  // try {
  //   var analyzer = Analyzer()

  //   // 初始化内置类型和包
  //   analyzer.initBuiltins()
  //   println("Analyzer builtins initialized")

  //   // 只执行 symbol allocation，不执行完整分析
  //   val project = analyzer.project
  //   val srcDirectory = vfs.findNodeByPath("test_project/src").get

  //   analyzer.analyzeAllFiles(vfs)

  //   // 输出分析结果
  //   project.dumpNamespace(hir_buffer)
  //   hir_buffer.flush()
  //   hir_buffer.close()

  //   println("HIR dumped to hir.lisp")

  //   // 打印项目结构概览
  //   println("\n=== Project Structure After Symbol Allocation ===")
  //   println(project.toSExpr)

  // } catch {
  //   case e: Exception =>
  //     println(s"Error during analysis: ${e.getMessage}")
  //     e.printStackTrace()
  // }
