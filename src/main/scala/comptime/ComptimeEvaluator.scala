// package comptime

// // 编译时计算引擎，用于执行编译时表达式
// object ComptimeEvaluator {
//   // 表示编译时表达式的类型
//   sealed trait ComptimeExpr
//   case class Literal(value: Value) extends ComptimeExpr
//   case class BinaryOp(left: ComptimeExpr, right: ComptimeExpr, op: String) extends ComptimeExpr
//   case class UnaryOp(expr: ComptimeExpr, op: String) extends ComptimeExpr
//   case class VariableRef(name: String) extends ComptimeExpr
//   case class FunctionCall(fn: ComptimeExpr, args: List[ComptimeExpr]) extends ComptimeExpr
//   case class FieldAccess(expr: ComptimeExpr, fieldName: String) extends ComptimeExpr
//   case class IndexAccess(expr: ComptimeExpr, index: ComptimeExpr) extends ComptimeExpr

//   // 编译时环境，用于存储变量绑定
//   case class Environment(bindings: Map[String, Value]) {
//     def lookup(name: String): Option[Value] = bindings.get(name)
//     def extend(name: String, value: Value): Environment = Environment(bindings + (name -> value))
//     def extendAll(newBindings: Map[String, Value]): Environment = Environment(bindings ++ newBindings)
//   }

//   // 空环境
//   val emptyEnv = Environment(Map.empty)

//   // 主要的求值函数，对给定环境中的表达式进行求值
//   def evaluate(expr: ComptimeExpr, env: Environment = emptyEnv): Value = expr match {
//     case Literal(value) => value
//     case BinaryOp(left, right, op) => executeBinaryOp(evaluate(left, env), evaluate(right, env), op)
//     case UnaryOp(expr, op) => executeUnaryOp(evaluate(expr, env), op)
//     case VariableRef(name) => env.lookup(name).getOrElse(ErrorValue(s"未定义的变量: $name"))
//     case FunctionCall(fn, args) =>
//       val fnValue = evaluate(fn, env)
//       val argValues = args.map(arg => evaluate(arg, env))
//       executeFunctionCall(fnValue, argValues)
//     case FieldAccess(expr, fieldName) => executeFieldAccess(evaluate(expr, env), fieldName)
//     case IndexAccess(expr, index) => executeIndexAccess(evaluate(expr, env), evaluate(index, env))
//   }

//   // 执行函数调用
//   def executeFunctionCall(fn: Value, args: List[Value]): Value = fn match {
//     case FunctionValue(params, body, _) =>
//       if (params.length != args.length) { ErrorValue(s"参数数量不匹配: 期望 ${params.length}, 实际 ${args.length}") }
//       else {
//         try body(args)
//         catch { case e: Exception => ErrorValue(s"函数执行错误: ${e.getMessage}") }
//       }
//     case _ => ErrorValue(s"${fn}不是一个可调用的函数")
//   }

//   // 执行字段访问
//   def executeFieldAccess(value: Value, fieldName: String): Value = value match {
//     case StructValue(_, fields) => fields.getOrElse(fieldName, ErrorValue(s"字段不存在: $fieldName"))
//     case _ => ErrorValue(s"无法从${value}访问字段")
//   }

//   // 执行索引访问
//   def executeIndexAccess(value: Value, index: Value): Value = (value, index) match {
//     case (ArrayValue(elements, _), IntValue(idx, _)) =>
//       if (idx >= 0 && idx < elements.length) { elements(idx.toInt) }
//       else { ErrorValue(s"索引越界: $idx, 数组长度: ${elements.length}") }
//     case (StringValue(str), IntValue(idx, _)) =>
//       if (idx >= 0 && idx < str.length) {
//         ??? // 需要一个字符值类型，这里可能返回 IntValue(str(idx.toInt).toInt)
//       } else { ErrorValue(s"索引越界: $idx, 字符串长度: ${str.length}") }
//     case (TupleValue(elements), IntValue(idx, _)) =>
//       if (idx >= 0 && idx < elements.length) { elements(idx.toInt) }
//       else { ErrorValue(s"索引越界: $idx, 元组长度: ${elements.length}") }
//     case _ => ErrorValue(s"无法对${value}使用索引$index")
//   }

//   // 执行二元操作符
//   def executeBinaryOp(left: Value, right: Value, op: String): Value = (left, op, right) match {
//     // 整数运算
//     case (IntValue(l, _), "+", IntValue(r, _)) => IntValue(l + r)
//     case (IntValue(l, _), "-", IntValue(r, _)) => IntValue(l - r)
//     case (IntValue(l, _), "*", IntValue(r, _)) => IntValue(l * r)
//     case (IntValue(l, _), "/", IntValue(r, _)) => if (r == 0) ErrorValue("除以零") else IntValue(l / r)
//     case (IntValue(l, _), "%", IntValue(r, _)) => if (r == 0) ErrorValue("模零") else IntValue(l % r)
//     case (IntValue(l, _), "<<", IntValue(r, _)) => IntValue(l << r)
//     case (IntValue(l, _), ">>", IntValue(r, _)) => IntValue(l >> r)
//     case (IntValue(l, _), "&", IntValue(r, _)) => IntValue(l & r)
//     case (IntValue(l, _), "|", IntValue(r, _)) => IntValue(l | r)
//     case (IntValue(l, _), "^", IntValue(r, _)) => IntValue(l ^ r)

//     // 浮点数运算
//     case (FloatValue(l, _), "+", FloatValue(r, _)) => FloatValue(l + r)
//     case (FloatValue(l, _), "-", FloatValue(r, _)) => FloatValue(l - r)
//     case (FloatValue(l, _), "*", FloatValue(r, _)) => FloatValue(l * r)
//     case (FloatValue(l, _), "/", FloatValue(r, _)) => FloatValue(l / r)

//     // 混合数值运算 (整数和浮点数)
//     case (IntValue(l, _), "+", FloatValue(r, _)) => FloatValue(l + r)
//     case (FloatValue(l, _), "+", IntValue(r, _)) => FloatValue(l + r)
//     case (IntValue(l, _), "-", FloatValue(r, _)) => FloatValue(l - r)
//     case (FloatValue(l, _), "-", IntValue(r, _)) => FloatValue(l - r)
//     case (IntValue(l, _), "*", FloatValue(r, _)) => FloatValue(l * r)
//     case (FloatValue(l, _), "*", IntValue(r, _)) => FloatValue(l * r)
//     case (IntValue(l, _), "/", FloatValue(r, _)) => FloatValue(l / r)
//     case (FloatValue(l, _), "/", IntValue(r, _)) => if (r == 0) ErrorValue("除以零") else FloatValue(l / r)

//     // 字符串操作
//     case (StringValue(l), "+", StringValue(r)) => StringValue(l + r)
//     case (StringValue(l), "+", v) => StringValue(l + v.toString)
//     case (v, "+", StringValue(r)) => StringValue(v.toString + r)

//     // 布尔运算
//     case (BoolValue(l), "&&", BoolValue(r)) => BoolValue(l && r)
//     case (BoolValue(l), "||", BoolValue(r)) => BoolValue(l || r)

//     // 比较运算
//     case (IntValue(l, _), "==", IntValue(r, _)) => BoolValue(l == r)
//     case (IntValue(l, _), "!=", IntValue(r, _)) => BoolValue(l != r)
//     case (IntValue(l, _), "<", IntValue(r, _)) => BoolValue(l < r)
//     case (IntValue(l, _), "<=", IntValue(r, _)) => BoolValue(l <= r)
//     case (IntValue(l, _), ">", IntValue(r, _)) => BoolValue(l > r)
//     case (IntValue(l, _), ">=", IntValue(r, _)) => BoolValue(l >= r)

//     case (FloatValue(l, _), "==", FloatValue(r, _)) => BoolValue(l == r)
//     case (FloatValue(l, _), "!=", FloatValue(r, _)) => BoolValue(l != r)
//     case (FloatValue(l, _), "<", FloatValue(r, _)) => BoolValue(l < r)
//     case (FloatValue(l, _), "<=", FloatValue(r, _)) => BoolValue(l <= r)
//     case (FloatValue(l, _), ">", FloatValue(r, _)) => BoolValue(l > r)
//     case (FloatValue(l, _), ">=", FloatValue(r, _)) => BoolValue(l >= r)

//     case (StringValue(l), "==", StringValue(r)) => BoolValue(l == r)
//     case (StringValue(l), "!=", StringValue(r)) => BoolValue(l != r)

//     case _ => ErrorValue(s"不支持的操作: $left $op $right")
//   }

//   // 执行一元操作符
//   def executeUnaryOp(value: Value, op: String): Value = (op, value) match {
//     case ("-", IntValue(v, size)) => IntValue(-v, size)
//     case ("-", FloatValue(v, size)) => FloatValue(-v, size)
//     case ("!", BoolValue(v)) => BoolValue(!v)
//     case ("~", IntValue(v, size)) => IntValue(~v, size)
//     case _ => ErrorValue(s"不支持的一元操作: $op $value")
//   }

//   // 预定义的内置函数
//   val builtinFunctions: Map[String, FunctionValue] = Map(
//     "len" -> FunctionValue(
//       List(("arr", ???)), // 需要表示"任何可测量长度的类型"
//       args =>
//         args.head match {
//           case ArrayValue(elements, _) => IntValue(elements.length)
//           case StringValue(s) => IntValue(s.length)
//           case _ => ErrorValue(s"无法获取长度: ${args.head}")
//         },
//       Int(64)
//     ),
//     "toInt" -> FunctionValue(
//       List(("value", ???)), // 需要表示"可转换为整数的类型"
//       args => args.head match { case v: Value => v.castTo(Int(64)).getOrElse(ErrorValue(s"无法转换为整数: $v")) },
//       Int(64)
//     ),
//     "typeof" -> FunctionValue(
//       List(("value", ???)), // 表示"任何类型"
//       args => TypeValue(args.head.getType),
//       ??? // 表示"类型的类型"
//     )
//   )

//   // 创建一个包含内置函数的初始环境
//   val initialEnv: Environment = Environment(builtinFunctions)
// }
