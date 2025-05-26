package vfs

import parse.*
import lex.{dumpTokens, lex, LexError, Token}
import java.nio.file.{FileSystemException, Paths}
import scala.collection.mutable.ListBuffer
import java.io.{File, PrintWriter}
import errors.{ErrorNote, ErrorReporter, FlurryError, FlurryMessage, Severity, SourceLocation, TerminalErrorReporter}
import scala.util.Using

enum VfsNodeKind {
  case File
  case Directory
  case Root
  case SrcDirectory
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

  // 优化路径处理，避免拼接导致的重复斜杠问题
  def absolutePath(): String = parent match
    case Some(p) => Paths.get(p.absolutePath(), name).toString
    case None => name

  def modFilePath(): String = kind match
    case VfsNodeKind.SrcDirectory => Paths.get(absolutePath(), "main.fl").toString
    case VfsNodeKind.Directory => Paths.get(absolutePath(), "mod.fl").toString
    case _ => absolutePath()

  // 优化错误处理和资源管理
  def ensureSrc(): Either[FileSystemException, Unit] = src match
    case None =>
      try {
        val file = new File(modFilePath())
        if (!file.exists()) Left(new FileSystemException("File does not exist: " + modFilePath()))
        else if (!file.isFile()) Left(new FileSystemException("Path is not a file: " + modFilePath()))
        else if (!file.canRead()) Left(new FileSystemException("Cannot read file: " + modFilePath()))
        else {
          Using(scala.io.Source.fromFile(file, "UTF-8")) { source =>
            this.src = Some(source.mkString)
            Right(())
          }.getOrElse(Left(new FileSystemException("Error reading file")))
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

      parse(this) match
        case Right(Some(ast)) =>
          this.ast = Some(ast)
          Right(())
        case Left(e) => throw new Exception("Parsing error: " + e)
        case _ => Right(())
    case Some(_) => Right(())

  // 优化行处理方法，使用更高效的函数式方法
  def processLines(): Unit = if (lines.isEmpty && src.isDefined) {
    val content = src.get
    val linesList = ListBuffer[Line]()
    var lineStart = 0

    for (i <- 0 until content.length) if (content(i) == '\n') {
      linesList += Line(lineStart, i)
      lineStart = i + 1
    }
    linesList += Line(lineStart, content.length)

    this.lines = Some(linesList.toSeq)
  }

  // 优化二分查找算法
  def locate(pos: Int): Int = lines match {
    case Some(lns) =>
      // 如果超出范围，返回最近的行
      if (pos < 0) return 0
      if (pos >= lns.last.to) return lns.length - 1

      @scala.annotation.tailrec
      def binarySearch(lo: Int, hi: Int): Int =
        if (lo >= hi) lo
        else {
          val mid = lo + (hi - lo) / 2
          val line = lns(mid)
          if (pos < line.from) binarySearch(lo, mid) else if (pos > line.to) binarySearch(mid + 1, hi) else mid
        }

      binarySearch(0, lns.length)
    case None => 0
  }

  // 定位单个token的行信息
  def locateToken(token: Token, extraLines: Int): LinesSlice = lines match {
    case Some(lns) =>
      val from = Math.max(0, locate(token.from) - extraLines)
      val to = Math.min(lns.length - 1, locate(token.to) + extraLines)
      LinesSlice(lns.slice(from, to + 1), from)
    case None => LinesSlice(Seq.empty, 0)
  }

  // 定位跨多个token的行信息
  def locateTokens(tokenBegin: Token, tokenEnd: Token, extraLines: Int): LinesSlice = lines match {
    case Some(lns) =>
      val from = Math.max(0, locate(tokenBegin.from) - extraLines)
      val to = Math.min(lns.length - 1, locate(tokenEnd.to) + extraLines)
      LinesSlice(lns.slice(from, to + 1), from)
    case None => LinesSlice(Seq.empty, 0)
  }

  // 整合错误报告方法，减少代码重复
  private def getErrorDisplayInfo(kind: ErrorKind, isStdOut: Boolean = true): (String, String, String, String) = {
    val (kindStr, kindColor) = kind match {
      case ErrorKind.Error => ("error", if (isStdOut) Console.RED else "")
      case ErrorKind.Warning => ("warning", if (isStdOut) Console.YELLOW else "")
      case ErrorKind.Info => ("info", if (isStdOut) Console.CYAN else "")
      case ErrorKind.Help => ("help", if (isStdOut) Console.GREEN else "")
      case ErrorKind.Log => ("log", if (isStdOut) Console.BLUE else "")
    }

    val reset = if (isStdOut) Console.RESET else ""
    val bold = if (isStdOut) Console.BOLD else ""

    (kindStr, kindColor, reset, bold)
  }

  // 报告单个token的错误
  def report[Error <: errors.FlurryError](
      token: Token,
      kind: ErrorKind,
      error: Error,
      labelInfo: String,
      extraLines: Int = 2,
      writer: PrintWriter = new PrintWriter(System.out)
  ): Unit = {
    processLines()

    val (kindStr, kindColor, reset, bold) = getErrorDisplayInfo(kind)
    val content = src.getOrElse("")
    val path = absolutePath()
    val linesSlice = locateToken(token, extraLines)

    writer.println(s"$bold$kindColor$kindStr[${error.code}]: ${error.errorMessage}$reset")
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

  // 使用新的错误报告系统
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
      if (reporter == null) {
        parent.flatMap(_.parent) match {
          case Some(root: VfsNode) =>
            // 尝试获取根节点的默认报告器
            try {
              val vfsField = root.getClass.getDeclaredField("vfs")
              vfsField.setAccessible(true)
              val vfs = vfsField.get(root)
              val defaultReporterField = vfs.getClass.getDeclaredField("defaultReporter")
              defaultReporterField.setAccessible(true)
              defaultReporterField.get(vfs).asInstanceOf[ErrorReporter]
            } catch { case _: Exception => new TerminalErrorReporter() }
          case _ => new TerminalErrorReporter()
        }
      } else reporter

    // 创建位置信息
    val lineIdx = locate(token.from)
    val columnIdx = lines.fold(0)(lns => token.from - lns(lineIdx).from + 1) // 1-based
    val location = SourceLocation(path, lineIdx + 1, columnIdx)

    // 创建错误消息
    val notes = if (labelInfo.nonEmpty) List(ErrorNote(Severity.Hint, labelInfo)) else List.empty

    // 报告错误
    actualReporter.report(FlurryMessage(
      kind.toSeverity,
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
      if (reporter == null) {
        parent.flatMap(_.parent) match {
          case Some(root: VfsNode) =>
            // 同上，尝试获取默认报告器
            try {
              val vfsField = root.getClass.getDeclaredField("vfs")
              vfsField.setAccessible(true)
              val vfs = vfsField.get(root)
              val defaultReporterField = vfs.getClass.getDeclaredField("defaultReporter")
              defaultReporterField.setAccessible(true)
              defaultReporterField.get(vfs).asInstanceOf[ErrorReporter]
            } catch { case _: Exception => new TerminalErrorReporter() }
          case _ => new TerminalErrorReporter()
        }
      } else reporter

    // 创建位置信息 - 从第一个token开始
    val fromLineIdx = locate(fromToken.from)
    val fromColumnIdx = lines.fold(0)(lns => fromToken.from - lns(fromLineIdx).from + 1) // 1-based
    val location = SourceLocation(path, fromLineIdx + 1, fromColumnIdx)

    // 创建错误消息
    val notes = if (labelInfo.nonEmpty) List(ErrorNote(Severity.Hint, labelInfo)) else List.empty

    // 报告错误
    actualReporter.report(FlurryMessage(
      kind.toSeverity,
      error.code,
      error.errorMessage,
      path,
      Some(location),
      Some((fromToken.from, toToken.to)),
      notes,
      Some(content),
      extraLines
    ))
  }

  def reportSpanT[Error <: errors.FlurryError](
      span: Span,
      kind: ErrorKind,
      error: Error,
      labelInfo: String,
      extraLines: Int = 2,
      writer: PrintWriter = new PrintWriter(System.out),
      isStdOut: Boolean = true
  ): Unit =
    reportSpan(tokens.get(span.start), tokens.get(span.end), kind, error, labelInfo, extraLines, writer, isStdOut)

  // 报告跨多个token的错误 - 修复行偏移BUG
  def reportSpan[Error <: errors.FlurryError](
      fromToken: Token,
      toToken: Token,
      kind: ErrorKind,
      error: Error,
      labelInfo: String,
      extraLines: Int = 2,
      writer: PrintWriter = new PrintWriter(System.out),
      isStdOut: Boolean = true
  ): Unit = {
    processLines()

    val (kindStr, kindColor, reset, bold) = getErrorDisplayInfo(kind, isStdOut)
    val path = absolutePath()
    val linesSlice = locateTokens(fromToken, toToken, extraLines)
    val content = src.getOrElse("")

    // 定位错误所在的行号范围
    val fromLineIdx = locate(fromToken.from)
    val toLineIdx = locate(toToken.to)

    writer.println(s"$bold$kindColor$kindStr[${error.code}]: ${error.errorMessage}$reset")
    writer.println(s"$path:")

    // 逐行处理并打印
    linesSlice.lines.zipWithIndex.foreach { case (line, idx) =>
      val absoluteLineIdx = linesSlice.startLine + idx // 在文件中的绝对行号
      val lineContent = content.substring(line.from, line.to)

      // 修复行偏移问题 - 确保行号对齐
      writer.println(f"${absoluteLineIdx + 1}%6d | $lineContent")

      // 确定当前行是否在错误范围内
      if (fromToken.from <= line.to && toToken.to >= line.from) {
        // 计算当前行中错误的开始和结束位置
        val errorStartInLine =
          if (absoluteLineIdx == fromLineIdx) fromToken.from - line.from
          else lineContent.indexWhere(!_.isWhitespace, 0).max(0)

        val errorEndInLine = if (absoluteLineIdx == toLineIdx) toToken.to - line.from else line.to - line.from

        // 计算标记长度
        val markerLength = Math.max(1, errorEndInLine - errorStartInLine)

        // 打印标记 - 固定9个字符的偏移，包括行号和分隔符
        if (markerLength > 0) {
          writer.print(" " * (errorStartInLine + 9))
          writer.print(kindColor)
          writer.print("^" * markerLength)

          if (absoluteLineIdx == toLineIdx) writer.println(s" $labelInfo$reset") else writer.println(reset)
        }
      }
    }
    writer.flush()
  }

  def dumpSExpr(writer: PrintWriter): Unit = {
    writer.print("(" + kind.toString + " " + name)
    children.foreach { children =>
      children.foreach { child =>
        writer.print(" ")
        child.dumpSExpr(writer)
      }
    }
    writer.print(")")
  }
}

// 添加常量用于避免Magic数字
object VfsNode {
  val DEFAULT_EXTRA_LINES = 2
  val LINE_NUMBER_WIDTH = 6
  val LINE_PREFIX_LENGTH = LINE_NUMBER_WIDTH + 3 // 6位行号 + " | "
}

val ignores = List(".git", ".bloop", ".metals", ".vscode", "target")
