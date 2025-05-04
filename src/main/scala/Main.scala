import parse.*
import vfs.*
import analysis1.Analyzer
import comptime.PackageDefinition

@main
def main(): Unit =
  val vfs = Vfs("test_project")
  var vfs_buffter = java.io.PrintWriter("vfs.lisp")
  var ast_buffer = java.io.PrintWriter("ast.lisp")
  var hir_buffer = java.io.PrintWriter("hir.lisp")

  vfs.dumpSExpr(vfs_buffter)
  vfs_buffter.flush()
  vfs_buffter.close()
  val node = vfs.findNodeByPath("test_project/src/main.fl").get
  node.ensureAst()
  node.ast.get.root.dumpSExpr(ast_buffer)
  ast_buffer.flush()
  ast_buffer.close()

  var analyzer = Analyzer()
  analyzer.initBuiltins()
  analyzer.fsSymbolAllocation(vfs)
  analyzer.resolveFileScope(node, analyzer.project.lookup("main").get.asInstanceOf[PackageDefinition])
  println(analyzer.project.dumpNamespace(hir_buffer))
  hir_buffer.flush()
  hir_buffer.close()
