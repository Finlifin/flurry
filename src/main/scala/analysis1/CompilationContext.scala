// package analysis1

// import errors.{ErrorReporter, FlurryError, TerminalErrorReporter}

// // 诊断报告器 - 用于收集和报告编译诊断
// class DiagnosticsReporter(private val reporter: ErrorReporter = new TerminalErrorReporter()):
//   def report(error: FlurryError): Unit = reporter.error(error, "unknown", None, None)

// // 全局编译上下文 - 包含编译期间的全局信息
// case class GlobalCompilationContext(diagnostics: DiagnosticsReporter)
