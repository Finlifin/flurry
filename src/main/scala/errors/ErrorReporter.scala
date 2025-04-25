package errors

import scala.collection.mutable.ListBuffer

// ANSI颜色工具
object AnsiColors {
  val RESET = "\u001B[0m"
  val RED = "\u001B[31m"
  val YELLOW = "\u001B[33m"
  val CYAN = "\u001B[36m"
  val BLUE = "\u001B[34m"
  val GREEN = "\u001B[32m"
  val BRIGHT_BLACK = "\u001B[90m"
  val BOLD = "\u001B[1m"
}

// 错误信息
case class FlurryMessage(
    severity: Severity,
    code: Int,
    errorMessage: String,
    filePath: String,
    location: Option[SourceLocation] = None,
    span: Option[(Int, Int)] = None, // 从Token位置转换而来(from, to)
    notes: List[ErrorNote] = List.empty,
    sourceContent: Option[String] = None,
    surroudingLines: Int = 0
)

// 错误报告器特质
trait ErrorReporter {
  def report(message: FlurryMessage): Unit

  // 方便方法
  def error(
      error: FlurryError,
      filePath: String,
      location: Option[SourceLocation] = None,
      span: Option[(Int, Int)] = None,
      notes: List[ErrorNote] = List.empty,
      sourceContent: Option[String] = None
  ): Unit = report(FlurryMessage(
    error.severity,
    error.code,
    error.errorMessage,
    filePath,
    location,
    span,
    notes ++ error.notes,
    sourceContent
  ))

  def warning(
      code: Int,
      message: String,
      filePath: String,
      location: Option[SourceLocation] = None,
      span: Option[(Int, Int)] = None,
      notes: List[ErrorNote] = List.empty,
      sourceContent: Option[String] = None
  ): Unit = report(FlurryMessage(Severity.Warning, code, message, filePath, location, span, notes, sourceContent))
}

// 收集错误报告器实现
class CollectingErrorReporter extends ErrorReporter {
  private val messagesBuffer = ListBuffer[FlurryMessage]()

  def report(message: FlurryMessage): Unit = messagesBuffer += message

  def getAllMessages(): List[FlurryMessage] = messagesBuffer.toList

  def hasErrors(): Boolean = messagesBuffer.exists(_.severity == Severity.Error)

  def clear(): Unit = messagesBuffer.clear()
}

// 终端格式化器
class TerminalFormatter {

  def colorize(text: String, color: String, isBold: Boolean = false): String = {
    val boldCode = if (isBold) AnsiColors.BOLD else ""
    s"$boldCode$color$text${AnsiColors.RESET}"
  }

  def getSeverityColor(severity: Severity): String = severity match {
    case Severity.Error => AnsiColors.RED
    case Severity.Warning => AnsiColors.YELLOW
    case Severity.Info => AnsiColors.CYAN
    case Severity.Hint => AnsiColors.GREEN
  }

  def getSeverityLabel(severity: Severity): String = severity match {
    case Severity.Error => "error"
    case Severity.Warning => "warning"
    case Severity.Info => "info"
    case Severity.Hint => "hint"
  }

  // 将错误信息格式化为终端输出
  def formatMessage(message: FlurryMessage): String = {
    val severityColor = getSeverityColor(message.severity)
    val severityLabel = getSeverityLabel(message.severity)
    val sb = new StringBuilder()

    // 头部信息：[severity][code]: message
    sb.append(colorize(s"$severityLabel[${message.code}]", severityColor, true))
    sb.append(s": ${message.errorMessage}\n")

    // 文件路径
    sb.append(s"${message.filePath}:\n")

    // 如果有源代码内容，添加相关代码段
    if (message.sourceContent.isDefined && message.span.isDefined) {
      val content = message.sourceContent.get
      val (from, to) = message.span.get

      // 找到包含错误的行
      val lines = content.split("\n")
      val errorLineIdx = content.substring(0, from).count(_ == '\n')
      val errorLine = lines(errorLineIdx)

      // 计算列位置
      val fromCol = from - content.substring(0, from).lastIndexOf('\n') - 1
      val toCol = to - content.substring(0, from).lastIndexOf('\n') - 1
      val lineNumber = errorLineIdx + 1

      // 计算要显示的行范围
      val surroundingLines = message.surroudingLines
      val startLineIdx = Math.max(0, errorLineIdx - surroundingLines)
      val endLineIdx = Math.min(lines.length - 1, errorLineIdx + surroundingLines)

      // 显示周围的代码行
      for (i <- startLineIdx to endLineIdx) {
        val currentLineNumber = i + 1
        val linePrefix = f"$currentLineNumber%6d |"
        sb.append(colorize(linePrefix, AnsiColors.BRIGHT_BLACK))
        sb.append(s" ${lines(i)}\n")

        // 在错误行下方添加错误标记
        if (i == errorLineIdx) {
          val markerPrefix = " " * 7 + "|"
          sb.append(colorize(markerPrefix, AnsiColors.BRIGHT_BLACK))
          sb.append(" " * (fromCol + 1))
          sb.append(colorize("^" * Math.max(1, toCol - fromCol), severityColor, true))
          sb.append("\n")
        }
      }
    }

    // 添加附加说明
    message.notes.foreach { note =>
      val noteColor = getSeverityColor(note.severity)
      val noteLabel = getSeverityLabel(note.severity)
      sb.append(colorize(s"= $noteLabel:", AnsiColors.BLUE, true))
      sb.append(s" ${note.message}\n")
    }

    sb.toString()
  }

  // 打印格式化后的错误信息
  def printMessage(message: FlurryMessage): Unit = println(formatMessage(message))
}

// 直接报告到终端的报告器
class TerminalErrorReporter(isStdOut: Boolean = true) extends ErrorReporter {
  private val formatter = new TerminalFormatter()

  def report(message: FlurryMessage): Unit =
    if (isStdOut) { println(formatter.formatMessage(message)) }
    else { System.err.println(formatter.formatMessage(message)) }
}
