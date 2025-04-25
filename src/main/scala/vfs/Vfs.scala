package vfs

import parse.*
import lex.{dumpTokens, lex, LexError, Token}
import java.nio.file.FileSystemException
import scala.collection.mutable.ListBuffer
import java.io.File
import errors.{ErrorNote, ErrorReporter, FlurryError, Severity, SourceLocation, TerminalErrorReporter}

class Vfs {
  // 默认的终端错误报告器
  val defaultReporter: ErrorReporter = new TerminalErrorReporter()

  var project_path: String = null
  var root: VfsNode = null

  var id_generator: Int = 0
  def nextId(): Int = {
    id_generator += 1
    id_generator
  }

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
    this.project_path = File(project_path).getAbsolutePath()
    this.root = VfsNode(VfsNodeKind.Root, project_path, nextId())
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
            val nodeKind =
              if (file.isDirectory()) { if (nodeName == "src") VfsNodeKind.Src else VfsNodeKind.Directory }
              else { VfsNodeKind.File }

            val childNode = new VfsNode(nodeKind, nodeName, nextId(), Some(parentNode))
            childrenBuffer += childNode

            if (file.isDirectory()) { traverseDirectory(file, childNode) }
          }
        }

        parentNode.children = Some(childrenBuffer.toList)
      }
    }

    // 从项目根目录开始遍历
    val rootDir = new File(project_path)
    if (rootDir.exists() && rootDir.isDirectory()) { traverseDirectory(rootDir, root) }
    else { throw new FileSystemException(s"Invalid project path: $project_path") }
  }

  // resolve the path to a file
  def resolve(path: String): Option[VfsNode] = {
    val parts = path.split("/").toList
    var currentNode: Option[VfsNode] = Some(root)

    // 根节点为一个绝对路径

    currentNode
  }

  def findNodeById(id: Int): Option[VfsNode] = {
    import scala.util.boundary

    boundary {
      def findInNode(node: VfsNode): Option[VfsNode] =
        if (node.getId() == id) { Some(node) }
        else {
          node.children match {
            case Some(children) =>
              for (child <- children) {
                val result = findInNode(child)
                if (result.isDefined) boundary.break(result)
              }
              None
            case None => None
          }
        }

      findInNode(root)
    }
  }

  def findNodeByPath(path: String): Option[VfsNode] = {
    // 先去掉vfs绝对路径
    val absolutePath = File(path).getAbsolutePath();
    val relativePath = absolutePath.substring(project_path.length + 1)
    println(relativePath)
    val parts = relativePath.split("/").toList
    var currentNode: Option[VfsNode] = Some(root)
    for (part <- parts) currentNode match {
      case Some(node) => node.children match {
          case Some(children) => currentNode = children.find(_.getName() == part)
          case None => currentNode = None
        }
      case None => currentNode = None
    }
    currentNode
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

  // 映射到Severity
  def toSeverity: errors.Severity = this match {
    case ErrorKind.Error => errors.Severity.Error
    case ErrorKind.Warning => errors.Severity.Warning
    case ErrorKind.Info | ErrorKind.Log => errors.Severity.Info
    case ErrorKind.Help => errors.Severity.Hint
  }
}

class VfsNode(
    var kind: VfsNodeKind,
    // if kind is root, name is the absolute path of the root
    var name: String,
    var id: Int,
    var parent: Option[VfsNode] = None,
    var src: Option[String] = None,
    var tokens: Option[ListBuffer[Token]] = None,
    var ast: Option[Ast] = None,
    var children: Option[List[VfsNode]] = None,
    var lines: Option[Seq[Line]] = None
) {
  def this(kind: VfsNodeKind, name: String, id: Int) = this(kind, name, id, None, None, None, None, None, None)

  def getName(): String = name
  def getId(): Int = id

  def absolutePath(): String = parent match
    case Some(p) => p.absolutePath() + "/" + name
    case None => name

  def modFilePath(): String = kind match
    case VfsNodeKind.Src => absolutePath() + "/main.fl"
    case VfsNodeKind.Directory => absolutePath() + "/mod.fl"
    case _ => absolutePath()

  def ensureSrc(): Either[FileSystemException, Unit] = src match
    case None =>
      try {
        val file = new java.io.File(modFilePath())
        if (!file.exists()) { Left(new FileSystemException("File does not exist: " + modFilePath())) }
        else if (!file.isFile()) { Left(new FileSystemException("Path is not a file: " + modFilePath())) }
        else if (!file.canRead()) { Left(new FileSystemException("Cannot read file: " + modFilePath())) }
        else {
          import scala.io.Source
          val source = Source.fromFile(file, "UTF-8")
          try {
            this.src = Some(source.mkString)
            Right(())
          } catch {
            case e: java.io.IOException => Left(new FileSystemException("Error reading file: " + e.getMessage))
          } finally source.close()
        }
      } catch { case e: Exception => Left(new FileSystemException("Unexpected error: " + e.getMessage)) }
    case Some(_) => Right(())

  def ensureTokens(): Either[LexError, Unit] = this.tokens match
    case None =>
      ensureSrc() match
        case Left(e) => throw new Exception("File system error: " + e)
        case _ => ()

      // 处理词法分析错误
      tokens = Some(lex(src.get))
      Right(())
    case Some(_) => Right(())

  def ensureAst(): Either[ParseError, Unit] = this.ast match
    case None =>
      ensureTokens() match
        case Left(e) => throw new Exception("Lexing error: " + e)
        case _ => ()

      // dumpTokens(tokens.get, src.get)
      this.ast = Some(parse(this))
      Right(())
    case Some(_) => Right(())

  // 处理行信息以供错误报告
  def processLines(): Unit = if (lines.isEmpty && src.isDefined) {
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

  // 二分查找定位行号
  def locate(pos: Int): Int = lines match {
    case Some(lns) =>
      var lo = 0
      var hi = lns.length
      while (lo < hi) {
        val mid = lo + (hi - lo) / 2
        val line = lns(mid)
        if (pos < line.from) { hi = mid }
        else if (pos > line.to) { lo = mid + 1 }
        else { return mid }
      }
      lo
    case None => 0
  }

  // 定位单个token的行信息
  def locateToken(token: Token, extraLines: Int): LinesSlice = lines match {
    case Some(lns) =>
      var from = locate(token.from)
      from = if (from < extraLines) 0 else from - extraLines
      var to = locate(token.to)
      to = if (to + extraLines >= lns.length) lns.length - 1 else to + extraLines
      LinesSlice(lns.slice(from, to + 1), from)
    case None => LinesSlice(Seq.empty, 0)
  }

  // 定位跨多个token的行信息
  def locateTokens(tokenBegin: Token, tokenEnd: Token, extraLines: Int): LinesSlice = lines match {
    case Some(lns) =>
      var from = locate(tokenBegin.from)
      from = if (from < extraLines) 0 else from - extraLines
      var to = locate(tokenEnd.to)
      to = if (to + extraLines >= lns.length) lns.length - 1 else to + extraLines
      LinesSlice(lns.slice(from, to + 1), from)
    case None => LinesSlice(Seq.empty, 0)
  }

  // 报告单个token的错误
  def report[Error <: errors.FlurryError](
      token: Token,
      kind: ErrorKind,
      error: Error,
      labelInfo: String,
      extraLines: Int = 2,
      writer: java.io.PrintWriter = java.io.PrintWriter(System.out)
  ): Unit = {
    processLines()

    val code = error.code
    val errorMessage = error.errorMessage
    val path = absolutePath()
    val linesSlice = locateToken(token, extraLines)
    val content = src.getOrElse("")

    val (kindStr, kindColor) = kind match {
      case ErrorKind.Error => ("error", Console.RED)
      case ErrorKind.Warning => ("warning", Console.YELLOW)
      case ErrorKind.Info => ("info", Console.CYAN)
      case ErrorKind.Help => ("help", Console.GREEN)
      case ErrorKind.Log => ("log", Console.BLUE)
    }

    val reset = Console.RESET
    val bold = Console.BOLD

    writer.println(s"$bold$kindColor$kindStr[$code]: $errorMessage$reset")
    writer.println(s"$path:")

    linesSlice.lines.zipWithIndex.foreach { case (line, idx) =>
      val lineIdx = linesSlice.startLine + idx
      val lineContent = content.substring(line.from, line.to)
      writer.println(f"${lineIdx + 1}%6d | $lineContent")

      if (token.from >= line.from && token.to <= line.to) {
        val column = token.from - line.from
        writer.print(" " * (column + 9))
        writer.print(kindColor)
        writer.print("^" * (token.to - token.from))
        writer.println(s" $labelInfo$reset")
      }
    }

    writer.flush()
  }

  // 使用新的错误报告系统报告单个token的错误
  def reportWithNewSystem[Error <: errors.FlurryError](
      token: Token,
      kind: ErrorKind,
      error: Error,
      labelInfo: String,
      extraLines: Int = 2,
      reporter: errors.ErrorReporter = null
  ): Unit = {
    processLines()

    val path = absolutePath()
    val content = src.getOrElse("")
    val actualReporter =
      if (reporter == null) parent.map(_.parent) match {
        // case Some(Some(value)) =>
        case _ => new errors.TerminalErrorReporter()
      }
      else reporter

    // 错误严重程度映射
    val severity = kind.toSeverity

    // 创建位置信息
    val lineIdx = locate(token.from)
    val columnIdx = token.from - lines.get(lineIdx).from + 1 // 1-based
    val location = errors.SourceLocation(path, lineIdx + 1, columnIdx)

    // 创建错误消息
    val notes = if (labelInfo.nonEmpty) List(errors.ErrorNote(errors.Severity.Hint, labelInfo)) else List.empty

    // 报告错误
    actualReporter.report(errors.FlurryMessage(
      severity,
      error.code,
      error.errorMessage,
      path,
      Some(location),
      Some((token.from, token.to)),
      notes,
      Some(content),
      extraLines
    ))
  }

  // 使用新的错误报告系统报告跨多个token的错误
  def reportSpanWithNewSystem[Error <: errors.FlurryError](
      fromToken: Token,
      toToken: Token,
      kind: ErrorKind,
      error: Error,
      labelInfo: String,
      extraLines: Int = 2,
      reporter: errors.ErrorReporter = null
  ): Unit = {
    processLines()

    val path = absolutePath()
    val content = src.getOrElse("")
    val actualReporter =
      if (reporter == null) parent.map(_.parent) match {
        // case Some(Some(vfs: Vfs)) => vfs.defaultReporter
        case _ => new errors.TerminalErrorReporter()
      }
      else reporter

    // 错误严重程度映射
    val severity = kind.toSeverity

    // 创建位置信息 - 从第一个token开始
    val fromLineIdx = locate(fromToken.from)
    val fromColumnIdx = fromToken.from - lines.get(fromLineIdx).from + 1 // 1-based
    val location = errors.SourceLocation(path, fromLineIdx + 1, fromColumnIdx)

    // 创建错误消息
    val notes = if (labelInfo.nonEmpty) List(errors.ErrorNote(errors.Severity.Hint, labelInfo)) else List.empty

    // 报告错误
    actualReporter.report(errors.FlurryMessage(
      severity,
      error.code,
      error.errorMessage,
      path,
      Some(location),
      Some((fromToken.from, toToken.to)),
      notes,
      Some(content)
    ))
  }

  def reportSpanT[Error <: errors.FlurryError](
      span: Span,
      kind: ErrorKind,
      error: Error,
      labelInfo: String,
      extraLines: Int = 2,
      writer: java.io.PrintWriter = java.io.PrintWriter(System.out),
      isStdOut: Boolean = true
  ): Unit =
    reportSpan(tokens.get(span.start), tokens.get(span.end), kind, error, labelInfo, extraLines, writer, isStdOut)

  // 报告跨多个token的错误
  // BUG: 我不知道为什么从第二行开始都会少6个字符的偏移😭
  def reportSpan[Error <: errors.FlurryError](
      fromToken: Token,
      toToken: Token,
      kind: ErrorKind,
      error: Error,
      labelInfo: String,
      extraLines: Int = 2,
      writer: java.io.PrintWriter = java.io.PrintWriter(System.out),
      isStdOut: Boolean = true
  ): Unit = {
    processLines()

    val code = error.code
    val errorMessage = error.errorMessage
    val path = absolutePath()
    val linesSlice = locateTokens(fromToken, toToken, extraLines)
    val content = src.getOrElse("")

    // 定位错误所在的行号范围
    val fromLineIdx = locate(fromToken.from)
    val toLineIdx = locate(toToken.to)

    val (kindStr, kindColor) = kind match {
      case ErrorKind.Error => ("error", if (isStdOut) Console.RED else "")
      case ErrorKind.Warning => ("warning", if (isStdOut) Console.YELLOW else "")
      case ErrorKind.Info => ("info", if (isStdOut) Console.CYAN else "")
      case ErrorKind.Help => ("help", if (isStdOut) Console.GREEN else "")
      case ErrorKind.Log => ("log", if (isStdOut) Console.BLUE else "")
    }

    val reset = if (isStdOut) Console.RESET else ""
    val bold = if (isStdOut) Console.BOLD else ""

    writer.println(s"$bold$kindColor$kindStr[$code]: $errorMessage$reset")
    writer.println(s"$path:")

    // 逐行处理并打印
    linesSlice.lines.zipWithIndex.foreach { case (line, idx) =>
      val absoluteLineIdx = linesSlice.startLine + idx // 在文件中的绝对行号
      val lineContent = content.substring(line.from, line.to)

      writer.println(f"${absoluteLineIdx + 1}%6d | $lineContent")

      // 确定当前行是否在错误范围内
      if (fromToken.from <= line.to && toToken.to >= line.from) {
        // 判断当前行在错误中的位置
        val isFirstErrorLine = absoluteLineIdx == fromLineIdx
        val isLastErrorLine = absoluteLineIdx == toLineIdx

        // 计算当前行中错误的开始和结束位置
        var errorStartInLine = 0
        if (isFirstErrorLine) {
          // 第一行：使用 fromToken.from 相对于行开始的偏移
          errorStartInLine = fromToken.from - line.from
        } else {
          // 非第一行：错误从缩进后开始，需要跳过前导空白
          // 查找行内第一个非空白字符
          var i = 0
          while (i < lineContent.length && lineContent(i).isWhitespace) i += 1
          errorStartInLine = i
        }

        // 计算当前行中错误结束的位置
        val errorEndInLine =
          if (isLastErrorLine) {
            // 最后一行：使用 toToken.to 相对于行开始的偏移
            toToken.to - line.from
          } else {
            // 非最后行：错误到行尾
            line.to - line.from
          }

        // 计算标记长度
        val markerLength = errorEndInLine - errorStartInLine

        // 打印标记
        if (markerLength > 0) {
          writer.print(" " * (errorStartInLine + 9))
          writer.print(kindColor)
          writer.print("^" * markerLength)

          if (isLastErrorLine) writer.println(s" $labelInfo$reset") else writer.println(reset)
        }
      }
    }
    writer.flush()
  }

  def dumpSExpr(writer: java.io.PrintWriter): Unit = {
    writer.print("(" + kind.toString + " " + name)
    children match {
      case Some(children) => for (child <- children) {
          writer.print(" ")
          child.dumpSExpr(writer)
        }
      case None => ()
    }
    writer.println(")")
  }
}

val ignores = List(".git", ".bloop", ".metals", ".vscode", "target")
