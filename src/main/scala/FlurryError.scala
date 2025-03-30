package errors

trait FlurryError {
  def errorMessage: String = this.toString()
  def code: Int = 0
}

class TestingError extends FlurryError
val testingError = TestingError()