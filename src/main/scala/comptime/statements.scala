package comptime

enum DeclarationKind:
  case Const
  case Let

case class Declaration(kind: DeclarationKind, pattern: Pattern, _type: Type, init: Option[Expr]) extends Statement, Expr

case class UseStatement(use: UseImport) extends Statement, Expr

case class ContinueStatement(label: Option[Statement]) extends Statement, Expr
case class BreakStatement(label: Option[Statement]) extends Statement, Expr
case class ReturnStatement(value: Option[Expr]) extends Statement, Expr
case class ResumeStatement(value: Option[Expr]) extends Statement, Expr
case class IfStatement(condition: Expr, thenBlock: Block, elseBlock: Option[Block]) extends Statement, Expr
case class MatchCase(pattern: Pattern, expr: Expr)
case class MatchStatement(value: Expr, cases: List[MatchCase]) extends Statement, Expr
case class LoopStatement(body: Block, condition: Option[Expr]) extends Statement, Expr
case class AssignStatement(location: Expr, value: Expr) extends Statement, Expr

case class Block(statements: List[Statement]) extends Statement, Expr {
  def resultType: Type = statements match {
    case Nil => VoidType()
    case _ => statements.last.getType
  }
}
