package vfs

import parse.*
import lex.{lex, LexError, Token}
import java.nio.file.FileSystemException
import scala.collection.mutable.ListBuffer

class Vfs {
  var project_path: String = null
  var root: VfsNode = null

  // build from file system
  // project_path
  //  - src
  //    - main.fl
  //    - subdir0
  //      - mod.fl
  //    - subdir1
  //      - mod.fl
  //  - package.fl
  //  - ...
  def this(project_path: String) = {
    this()
    this.project_path = project_path
    this.root = VfsNode(VfsNodeKind.Root, project_path)
    import java.io.File
    import scala.collection.mutable.ListBuffer

    // 递归遍历目录，构建VFS节点树
    def traverseDirectory(dir: File, parentNode: VfsNode): Unit = {
      val files = dir.listFiles()
      if (files != null) {
        val childrenBuffer = ListBuffer[VfsNode]()

        for (file <- files) {
          val nodeName = file.getName
          // 忽略指定的文件或目录
          if (!ignores.contains(nodeName)) {
            val nodeKind = if (file.isDirectory()) {
              if (nodeName == "src") VfsNodeKind.Src else VfsNodeKind.Directory
            } else {
              VfsNodeKind.File
            }

            val childNode = new VfsNode(nodeKind, nodeName, Some(parentNode))
            childrenBuffer += childNode

            if (file.isDirectory()) {
              traverseDirectory(file, childNode)
            }
          }
        }

        parentNode.children = Some(childrenBuffer.toList)
      }
    }

    // 从项目根目录开始遍历
    val rootDir = new File(project_path)
    if (rootDir.exists() && rootDir.isDirectory()) {
      traverseDirectory(rootDir, root)
    } else {
      throw new FileSystemException(s"Invalid project path: $project_path")
    }
  }

  def dumpSExpr(writer: java.io.PrintWriter): Unit = {
    writer.println("(vfs")
    root.dumpSExpr(writer)
    writer.println(")")
  }
}

enum VfsNodeKind {
  case File
  case Directory
  case Root
  case Src
}

// 定义错误报告辅助类
case class Line(from: Int, to: Int)
case class LinesSlice(lines: Seq[Line], startLine: Int)

enum ErrorKind {
  case Error, Warning, Info, Help, Log
}

class VfsNode(
    kind: VfsNodeKind,
    // if kind is root, name is the absolute path of the root
    name: String,
    parent: Option[VfsNode] = None,
    var src: Option[String] = None,
    var tokens: Option[ListBuffer[Token]] = None,
    var ast: Option[Ast] = None,
    var children: Option[List[VfsNode]] = None,
    var lines: Option[Seq[Line]] = None
) {
  def this(kind: VfsNodeKind, name: String) =
    this(kind, name, None, None, None, None, None, None)

  def absolutePath(): String = {
    parent.map(_.absolutePath() + "/" + name) match
      case None        => name
      case Some(value) => value
  }

  def modFilePath(): String = {
    kind match
      case VfsNodeKind.Src       => absolutePath() + "/main.fl"
      case VfsNodeKind.Directory => absolutePath() + "/mod.fl"
      case _                     => absolutePath()
  }

  def ensureSrc(): Either[FileSystemException, Unit] = {
    src match
      case None => {
        // open file and read to string
        val file = new java.io.File(modFilePath())
        if (!file.exists()) {
          Left(new FileSystemException("File does not exist"))
        } else {
          val src = scala.io.Source.fromFile(file)
          val lines =
            try src.mkString
            finally src.close()
          this.src = Some(lines)
          Right(())
        }
      }
      case Some(_) => Right(())
  }

  def ensureTokens(): Either[LexError, Unit] = {
    this.tokens match
      case None => {
        ensureSrc() match
          case Left(e) => throw new Exception("File system error: " + e)
          case _       => ()

        // 处理词法分析错误
        lex(src.get)
        Right(())
      }
      case Some(_) => Right(())
  }

  def ensureAst(): Either[ParseError, Unit] = {
    this.ast match
      case None => {
        ensureTokens() match
          case Left(e) => throw new Exception("Lexing error: " + e)
          case _       => ()

        parse(this)
        Right(())
      }
      case Some(_) => Right(())
  }

  // 处理行信息以供错误报告
  def processLines(): Unit = {
    if (lines.isEmpty && src.isDefined) {
      val content = src.get
      var linesList = ListBuffer[Line]()
      var lineStart = 0
      var i = 0

      while (i < content.length) {
        if (content(i) == '\n') {
          linesList += Line(lineStart, i)
          lineStart = i + 1
        }
        i += 1
      }
      linesList += Line(lineStart, content.length)

      this.lines = Some(linesList.toSeq)
    }
  }

  // 二分查找定位行号
  def locate(pos: Int): Int = {
    lines match {
      case Some(lns) =>
        var lo = 0
        var hi = lns.length
        while (lo < hi) {
          val mid = lo + (hi - lo) / 2
          val line = lns(mid)
          if (pos < line.from) {
            hi = mid
          } else if (pos > line.to) {
            lo = mid + 1
          } else {
            return mid
          }
        }
        lo
      case None => 0
    }
  }

  // 定位单个token的行信息
  def locateToken(token: Token, extraLines: Int): LinesSlice = {
    lines match {
      case Some(lns) =>
        var from = locate(token.from)
        from = if (from < extraLines) 0 else from - extraLines
        var to = locate(token.to)
        to =
          if (to + extraLines >= lns.length) lns.length - 1 else to + extraLines
        LinesSlice(lns.slice(from, to + 1), from)
      case None => LinesSlice(Seq.empty, 0)
    }
  }

  // 定位跨多个token的行信息
  def locateTokens(
      tokenBegin: Token,
      tokenEnd: Token,
      extraLines: Int
  ): LinesSlice = {
    lines match {
      case Some(lns) =>
        var from = locate(tokenBegin.from)
        from = if (from < extraLines) 0 else from - extraLines
        var to = locate(tokenEnd.to)
        to =
          if (to + extraLines >= lns.length) lns.length - 1 else to + extraLines
        LinesSlice(lns.slice(from, to + 1), from)
      case None => LinesSlice(Seq.empty, 0)
    }
  }

  // 报告单个token的错误
  def report(
      token: Token,
      kind: ErrorKind,
      code: Int,
      labelInfo: String,
      extraLines: Int = 2
  ): Unit = {
    processLines()

    val path = absolutePath()
    val linesSlice = locateToken(token, extraLines)
    val content = src.getOrElse("")

    val (kindStr, kindColor) = kind match {
      case ErrorKind.Error   => ("error", Console.RED)
      case ErrorKind.Warning => ("warning", Console.YELLOW)
      case ErrorKind.Info    => ("info", Console.CYAN)
      case ErrorKind.Help    => ("help", Console.GREEN)
      case ErrorKind.Log     => ("log", Console.BLUE)
    }

    val reset = Console.RESET
    val bold = Console.BOLD

    println(s"${bold}${kindColor}${kindStr}[${code}]: ${code}${reset}")
    println(s"${path}:")

    linesSlice.lines.zipWithIndex.foreach { case (line, idx) =>
      val lineIdx = linesSlice.startLine + idx
      val lineContent = content.substring(line.from, line.to)
      println(f"${lineIdx + 1}%6d | ${lineContent}")

      if (token.from >= line.from && token.to <= line.to) {
        val column = token.from - line.from
        print(" " * (column + 9))
        print(kindColor)
        print("^" * (token.to - token.from))
        println(s" ${labelInfo}${reset}")
      }
    }
  }

  // 报告跨多个token的错误
  def reportSpan(
      fromToken: Token,
      toToken: Token,
      kind: ErrorKind,
      code: Int,
      labelInfo: String,
      extraLines: Int = 2
  ): Unit = {
    processLines()

    val path = absolutePath()
    val linesSlice = locateTokens(fromToken, toToken, extraLines)
    val content = src.getOrElse("")

    val (kindStr, kindColor) = kind match {
      case ErrorKind.Error   => ("error", Console.RED)
      case ErrorKind.Warning => ("warning", Console.YELLOW)
      case ErrorKind.Info    => ("info", Console.CYAN)
      case ErrorKind.Help    => ("help", Console.GREEN)
      case ErrorKind.Log     => ("log", Console.BLUE)
    }

    val reset = Console.RESET
    val bold = Console.BOLD

    println(s"${bold}${kindColor}${kindStr}[${code}]: ${code}${reset}")
    println(s"${path}:")

    linesSlice.lines.zipWithIndex.foreach { case (line, idx) =>
      val lineIdx = linesSlice.startLine + idx
      val lineContent = content.substring(line.from, line.to)
      println(f"${lineIdx + 1}%6d | ${lineContent}")

      // 处理第一行
      if (fromToken.from >= line.from && fromToken.to <= line.to) {
        val column = fromToken.from - line.from
        print(" " * (column + 9))
        print(kindColor)
        val min = Math.min(toToken.to, line.to)
        print("^" * (min - fromToken.from))

        if (toToken.to <= line.to)
          println(s" ${labelInfo}${reset}")
        else
          println(reset)
      }
      // 处理中间行
      else if (fromToken.from < line.from && toToken.to > line.to) {
        print(" " * 9)
        print(kindColor)
        print("^" * (line.to - line.from))
        println(reset)
      }
      // 处理最后一行
      else if (toToken.from >= line.from && toToken.to <= line.to) {
        print(" " * 9)
        print(kindColor)
        print("^" * (toToken.to - line.from))
        println(s" ${labelInfo}${reset}")
      }
    }
  }

  def dumpSExpr(writer: java.io.PrintWriter): Unit = {
    writer.print("(" + kind.toString + " " + name)
    children match {
      case Some(children) =>
        for (child <- children) {
          writer.print(" ")
          child.dumpSExpr(writer)
        }
      case None => ()
    }
    writer.println(")")
  }
}

val ignores = List(
  ".git",
  ".bloop",
  ".metals",
  ".vscode",
  "target"
)
