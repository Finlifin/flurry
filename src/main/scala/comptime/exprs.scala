// package comptime

// enum BinaryOp:
//   case Add, Sub, Mul, Div, Mod
//   case Eq, Neq, Gt, Ge, Lt, Le

// case class BinaryApplication(op: BinaryOp, lhs: Expr, rhs: Expr) extends Expr {
//   def resultType: Type = op match {
//     case BinaryOp.Add | BinaryOp.Sub | BinaryOp.Mul | BinaryOp.Div | BinaryOp.Mod => IntegerType()
//     case BinaryOp.Eq | BinaryOp.Neq | BinaryOp.Gt | BinaryOp.Ge | BinaryOp.Lt | BinaryOp.Le => BoolType()
//   }
// }

// enum UnaryOp:
//   case Neg, Not

// case class UnaryApplication(op: UnaryOp, expr: Expr) extends Expr {
//   def resultType: Type = op match {
//     case UnaryOp.Neg => IntegerType()
//     case UnaryOp.Not => BoolType()
//   }
// }
