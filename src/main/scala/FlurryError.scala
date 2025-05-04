package errors

// 定义严重级别
enum Severity {
  case Error, Warning, Info, Hint
}

// 源码位置信息
case class SourceLocation(
    filePath: String,
    line: Int, // 1-based
    column: Int // 1-based
)

// 源代码跨度
case class SourceSpan(start: SourceLocation, end: SourceLocation)

// 附加说明或建议
case class ErrorNote(
    severity: Severity,
    message: String,
    location: Option[SourceLocation] = None,
    span: Option[SourceSpan] = None
)

// 基本错误特质
trait FlurryError extends Throwable {
  def errorMessage: String = this.toString()
  def code: Int = 0
  def severity: Severity = Severity.Error
  def notes: List[ErrorNote] = List.empty
}

// 用于测试的简单错误实现
class TestingError extends FlurryError
val testingError = TestingError()
