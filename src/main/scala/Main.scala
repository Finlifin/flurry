import parse.*
import vfs.*

@main def main(): Unit =
  val vfs = Vfs(".")
  var writer = java.io.PrintWriter("vfs.lisp")
  vfs.dumpSExpr(writer)
  writer.flush()
  writer.close()
