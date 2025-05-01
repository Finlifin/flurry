package comptime

trait Expr
case class Block(statements: List[Expr]) extends Expr
