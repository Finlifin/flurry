import parse.*
import vfs.*

@main
def main(): Unit =
  val vfs = Vfs("test_project")
  var vfs_buffter = java.io.PrintWriter("vfs.lisp")
  var ast_buffer = java.io.PrintWriter("ast.lisp")

  vfs.dumpSExpr(vfs_buffter)
  vfs_buffter.flush()
  vfs_buffter.close()
  val node = vfs.findNodeByPath("test_project/src/main.fl").get
  node.ensureAst()
  node.ast.get.root.dumpSExpr(ast_buffer)
  ast_buffer.flush()
  ast_buffer.close()
