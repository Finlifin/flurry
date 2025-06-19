// package analysis1

// import scala.collection.mutable
// import scala.util.{Failure, Success, Try}
// import errors.FlurryError

// // --- 编译时值表示（在您的HIR中已部分定义） ---
// // 我们假设 Hir.Integer, Hir.Real, Hir.Bool, Hir.Str, Hir.CharVal, Hir.Symbol,
// // Hir.Type, Hir.Object, Hir.Tuple, Hir.FunctionDef（用于编译时函数）等
// // 可以表示编译时值。如果Hir节点变得过于模糊或携带过多运行时特定信息，
// // 我们可能需要一个更独特的ComptimeValue枚举。
// // 现在，让我们在适当的地方直接使用Hir节点。

// // --- 编译时执行上下文 ---
// case class ComptimeContext(
//     engine: ComptimeEngine, // 访问引擎以进行递归调用或查询执行
//     scopeManager: ScopeManager, // 在编译时执行中解析符号
//     currentScope: ScopeId, // 符号解析的当前词法作用域
//     callStack: mutable.Stack[ComptimeStackFrame] = mutable.Stack(), // 用于编译时函数调用
//     // 可能需要一种访问Luna查询引擎的方式，用于如item'type等操作
//     // 或在必要时触发其他编译时查询。
//     // luna: QueryEngine（可以通过engine传递）
//     compilationContext: GlobalCompilationContext // 用于全局设置或诊断
// ):
//   def withScope(newScope: ScopeId): ComptimeContext = this.copy(currentScope = newScope)

//   def pushFrame(frame: ComptimeStackFrame): Unit = callStack.push(frame)
//   def popFrame(): Option[ComptimeStackFrame] = Try(callStack.pop()).toOption
//   def currentFrame: Option[ComptimeStackFrame] = callStack.headOption

// case class ComptimeStackFrame(
//     functionName: String, // 或编译时函数的Symbol
//     // 存储当前编译时函数调用的局部变量和参数
//     locals: mutable.Map[String, Hir] = mutable.Map() // String是变量名，Hir是其ComptimeValue
// )

// // --- 编译时引擎 ---
// class ComptimeEngine(
//     // 访问全局编译上下文以获取诊断等信息
//     // 如果不通过ComptimeContext传递，可能还需要QueryEngine。
//     globalContext: GlobalCompilationContext,
//     // 顶层编译时符号的初始作用域管理器
//     initialScopeManager: ScopeManager
// ):

//   /** 在编译时上下文中评估HIR节点。结果应该是表示编译时常量值的HIR节点 （如Hir.Integer、Hir.Str、Hir.Type、表示编译时映射的Hir.Object）。
//     */
//   def evaluate(hir: Hir, context: ComptimeContext): Either[FlurryError, Hir] = Try {
//     // 检查最大编译时评估深度或步骤以防止无限循环（可选）
//     // context.compilationContext.incrementComptimeSteps()

//     hir match
//       // 字面量已经是编译时值
//       case lit @ Hir.Integer(_) => Right(lit)
//       case lit @ Hir.Real(_) => Right(lit)
//       case lit @ Hir.Bool(_) => Right(lit)
//       case lit @ Hir.Str(_) => Right(lit)
//       case lit @ Hir.CharVal(_) => Right(lit)
//       case lit @ Hir.Symbol(_) => Right(lit)
//       case lit @ Hir.Type(_) => Right(lit) // Hir.Type节点是编译时值
//       case Hir.TypeVoid | Hir.TypeNoReturn | Hir.TypeAny | Hir.TypeInteger | Hir.TypeReal | Hir.TypeBool | Hir.TypeStr |
//           Hir.TypeChar | Hir.TypeSymbol | Hir.TypeObject | Hir.TypeType => Right(hir) // 这些基本类型是编译时值

//       case Hir.Tuple(elements) =>
//         val evalElements = elements.map(e => evaluate(e, context)).collect {
//           case Right(value) => value
//           case Left(err) => return Left(err) // 出错时提前退出
//         }
//         Right(Hir.Tuple(evalElements).withAst(hir.ast.get.file, hir.ast.get.node)) // 保留AST跟踪

//       case Hir.Object(children, properties) =>
//         val evalChildren = children.map(c => evaluate(c, context)).collect {
//           case Right(value) => value
//           case Left(err) => return Left(err)
//         }
//         val evalProperties = properties.map { (key, valueExpr) =>
//           evaluate(valueExpr, context) match
//             case Right(value) => key -> value
//             case Left(err) => return Left(err)
//         }
//         Right(Hir.Object(evalChildren, evalProperties).withAst(hir.ast.get.file, hir.ast.get.node))

//       case Hir.BinaryApplication(op, lhs, rhs) =>
//         for
//           evalLhs <- evaluate(lhs, context)
//           evalRhs <- evaluate(rhs, context)
//           result <- evaluateBinaryOp(op, evalLhs, evalRhs, hir.ast)
//         yield result

//       case Hir.UnaryApplication(op, operand) =>
//         for
//           evalOperand <- evaluate(operand, context)
//           result <- evaluateUnaryOp(op, evalOperand, hir.ast)
//         yield result

//       // // 符号/标识符解析（在编译时上下文中）
//       // // 这假设'ResolvedId'是名称解析的结果
//       // // 如果它是常量，其'typ'字段可能已经是编译时值。
//       // // 或者，它可能指向编译时函数定义。
//       // case Hir.ResolvedId(symbol, typ) =>
//       //   // 如果它是当前栈帧中的编译时变量
//       //   context.currentFrame.flatMap(_.locals.get(symbol.name)) match
//       //     case Some(localValue) => Right(localValue)
//       //     case None =>
//       //       // 否则，它可能是全局编译时常量或编译时函数
//       //       // 这部分需要仔细设计如何让`Symbol`链接到其定义
//       //       // 以及如何存储和访问`comptime const`。
//       //       // 现在，让我们假设如果它不是局部变量，就是直接的编译时值或错误。
//       //       // 更健壮的解决方案将涉及在`scopeManager`中查找`symbol`
//       //       // 并检查其定义是否为`comptime const`或`comptime fn`。
//       //       symbol.hir match // 假设Symbol case class有'hir: Hir'字段用于其定义
//       //         case comptimeConstDef @ (_: Hir.Integer | _: Hir.Str | _: Hir.Bool | _: Hir.Type | _: Hir.Object |
//       //             _: Hir.Tuple) => Right(comptimeConstDef)
//       //         // 我们暂时单独处理Hir.FunctionDef调用
//       //         case _ =>
//       //           Left(ComptimeError(s"Cannot evaluate runtime symbol '${symbol.name}' at compile time.", hir.ast))

//       // 函数调用（编译时函数调用）
//       case call @ Hir.TypeApplication(caller, args) => // 假设TypeApplication也可以是编译时函数调用
//         handleComptimeFunctionCall(caller, args, context, call.ast)

//       // // 获取表达式的'type'（image操作）
//       // // 这需要访问类型推断结果，可能通过QueryEngine
//       // // 现在，如果是简单情况，我们假设类型已经在Hir节点上。
//       // case Hir.UnaryApplication(UnaryOp.Image("type"), operand) => // 假设的HIR用于'operand'type
//       //   // 编译时值的'type'也是编译时值（Hir.Type）
//       //   // 如果操作数已经是值，其类型是已知的。
//       //   // 如果操作数是*可能*具有运行时类型的表达式，
//       //   // 我们需要获取该类型。这是QueryEngine交互的关键所在。
//       //   // 示例：(10 + 20)'type -> TypeInt(32)
//       //   // 示例：myRuntimeVar'type -> 其推断的Hir.Type
//       //   operand.inferred_type match // 假设HIR节点有来自先前pass的inferred_type
//       //     case Some(typ) => Right(Hir.Type(typ)) // 将Hir.Type包装在Hir.Type中作为ComptimeValue
//       //     case None =>
//       //       // 这是一个简化。实际上，如果还不知道，你会询问QueryEngine
//       //       // 推断`operand`的类型。
//       //       // 现在，如果我们没有它，就是错误。
//       //       Left(ComptimeError(s"Cannot determine type of expression for 'type image at compile time.", hir.ast))

//       // TODO: 实现其他编译时构造：
//       // - 编译时控制流（if、for - 通常通过'inline if/for'成为直接HIR）
//       // - 编译时对象/结构实例化（如package.fl）
//       // - 访问属性：builtin.getAttributes(type)
//       // - 动态创建类型：comptime fn createStruct() -> Type { ... }

//       case Hir.Invalid => Left(ComptimeError("Attempted to evaluate an invalid HIR node.", hir.ast))
//       case Hir.AwaitingAnalysis => Left(ComptimeError("Attempted to evaluate an unresolved HIR node.", hir.ast))

//       case _ =>
//         Left(ComptimeError(s"Comptime evaluation not implemented for HIR node: ${hir.getClass.getSimpleName}", hir.ast))

//   } match
//     case Success(result) => result
//     case Failure(e: FlurryError) => Left(e)
//     case Failure(e) => Left(
//         ComptimeError(s"Unexpected exception during comptime evaluation: ${e.getMessage}", hir.ast, code = 5000)
//       ) // 通用编译时错误

//   private def evaluateBinaryOp(
//       op: BinaryOp,
//       lhs: Hir,
//       rhs: Hir,
//       location: Option[AstLocation]
//   ): Either[FlurryError, Hir] = (op, lhs, rhs) match
//     case (BinaryOp.Add, Hir.Integer(l), Hir.Integer(r)) => Right(Hir.Integer(l + r))
//     case (BinaryOp.Add, Hir.Str(l), Hir.Str(r)) => Right(Hir.Str(l + r)) // 编译时字符串连接
//     case (BinaryOp.Sub, Hir.Integer(l), Hir.Integer(r)) => Right(Hir.Integer(l - r))
//     case (BinaryOp.Mul, Hir.Integer(l), Hir.Integer(r)) => Right(Hir.Integer(l * r))
//     case (BinaryOp.Div, Hir.Integer(l), Hir.Integer(r)) if r != 0 => Right(Hir.Integer(l / r))
//     case (BinaryOp.Div, Hir.Integer(_), Hir.Integer(r)) if r == 0 =>
//       Left(ComptimeError("Compile-time division by zero.", location))

//     case (BinaryOp.Eq, Hir.Integer(l), Hir.Integer(r)) => Right(Hir.Bool(l == r))
//     case (BinaryOp.Eq, Hir.Str(l), Hir.Str(r)) => Right(Hir.Bool(l == r))
//     case (BinaryOp.Eq, Hir.Bool(l), Hir.Bool(r)) => Right(Hir.Bool(l == r))
//     case (BinaryOp.Eq, Hir.Symbol(l), Hir.Symbol(r)) => Right(Hir.Bool(l == r))
//     // TODO: 如果类型在编译时可以直接按值比较，为Hir.Type添加Eq

//     case (BinaryOp.And, Hir.Bool(l), Hir.Bool(r)) => Right(Hir.Bool(l && r))
//     case (BinaryOp.Or, Hir.Bool(l), Hir.Bool(r)) => Right(Hir.Bool(l || r))

//     // TODO: 实现其他二元操作（比较等）
//     case _ => Left(ComptimeError(
//         s"Unsupported binary operation '$op' between ${lhs.getClass.getSimpleName} and ${rhs.getClass.getSimpleName} at compile time.",
//         location
//       ))

//   private def evaluateUnaryOp(op: UnaryOp, operand: Hir, location: Option[AstLocation]): Either[FlurryError, Hir] =
//     (op, operand) match
//       case (UnaryOp.Neg, Hir.Integer(v)) => Right(Hir.Integer(-v))
//       case (UnaryOp.Not, Hir.Bool(v)) => Right(Hir.Bool(!v))
//       // TODO: 实现其他一元操作
//       case _ => Left(ComptimeError(
//           s"Unsupported unary operation '$op' on ${operand.getClass.getSimpleName} at compile time.",
//           location
//         ))

//   private def handleComptimeFunctionCall(
//       callerExpr: Hir,
//       argExprs: List[Hir],
//       context: ComptimeContext,
//       callLocation: Option[AstLocation]
//   ): Either[FlurryError, Hir] =
//     // 1. 评估调用者表达式以获取函数定义或函数值
//     evaluate(callerExpr, context).flatMap {
//       case funcDef @ Hir.FunctionDef(name, comptimeFnParams, runtimeFnParams, returnType, _, body, attributes) =>
//         // 这是对已知编译时函数定义的直接调用
//         // 对于Flurry，`fn foo where T`本身就是编译时函数。
//         // 我们需要区分这是对泛型实例化的调用（单态化）
//         // 还是对简单编译时函数的调用。

//         // Flurry的模型：`struct Vec where T`是`comptime fn<T:Type> -> Type`
//         // `fn show where T`是`comptime fn<T:Type> -> fn(T) -> String`
//         // 这个`handleComptimeFunctionCall`是用于在编译时*执行*函数
//         // 以*返回编译时值*。
//         // 单态化是更高级别的查询，*使用*这个来评估编译时参数。

//         // 对于简单的编译时函数（不是泛型模板构造器）：
//         if !attributes.getAttribute("flurry_keyword_comptime").contains(Hir.Bool(true)) &&
//           !attributes.getAttribute("flurry_keyword_pure_comptime").contains(Hir.Bool(true))
//         then // 检查是否标记为comptime
//           return Left(ComptimeError(
//             s"Function '$name' is not a comptime function and cannot be called at compile time.",
//             callLocation
//           ))

//         if runtimeFnParams.length != argExprs.length then
//           return Left(ComptimeError(
//             s"Comptime function '$name' expected ${runtimeFnParams.length} arguments, got ${argExprs.length}.",
//             callLocation
//           ))

//         // 在当前上下文中评估参数
//         val evalArgsEither = argExprs.map(arg => evaluate(arg, context))
//         val evalArgs = mutable.ListBuffer[Hir]()
//         for eitherArg <- evalArgsEither do
//           eitherArg match
//             case Right(argVal) => evalArgs += argVal
//             case Left(err) => return Left(err)

//         // 为调用创建新的栈帧
//         val newFrame = ComptimeStackFrame(name)
//         runtimeFnParams.zip(evalArgs).foreach { case (paramDef, argVal) =>
//           // TODO: 在编译时检查argVal与paramDef.paramType的类型
//           newFrame.locals(paramDef.name) = argVal
//         }
//         context.pushFrame(newFrame)

//         // 执行函数体
//         val result = body match
//           case block @ Hir.Block(_) => evaluate(block, context.withScope(???)) // TODO: 函数体需要自己的作用域
//           case singleExpr => evaluate(singleExpr, context.withScope(???))
//           case Hir.Invalid => Left(ComptimeError(s"Comptime function '$name' has no body.", callLocation))
//           case _ => Left(ComptimeError(s"Unsupported comptime function body type for '$name'.", callLocation))

//         context.popFrame()
//         // TODO: 在编译时检查结果与函数的returnType
//         result

//       // TODO: 处理调用Hir.FunctionVal（编译时闭包/函数指针）
//       // case funcVal @ Hir.FunctionVal(fnSymbol, capturedEnv) => ...

//       case other =>
//         Left(ComptimeError(s"Expression does not evaluate to a callable comptime function: $callerExpr", callLocation))
//     }

//   /** 评估预期在编译时上下文中表示类型的HIR节点。这对于 `item'type`、类型别名解析和泛型实例化至关重要。结果应该是Hir.Type节点。
//     */
//   def evaluateType(hir: Hir, context: ComptimeContext): Either[FlurryError, Hir.Type] =
//     // 这个函数与`evaluate`非常相似，但其结果专门是Hir.Type。
//     // 它处理如下情况：
//     // - 解析类型别名：`type MyInt = i32; ... let x: MyInt;` -> MyInt评估为Hir.TypeInt(32)
//     // - 实例化泛型类型：`Vec<i32>`
//     //    - `Vec`是TypeConstructor（编译时函数）
//     //    - `i32`是Hir.Type（编译时值）
//     //    - 这个调用有效地使用`T=i32`运行`Vec`编译时函数
//     //      并返回`Vec<i32>`的`Hir.StructInstance`。

//     evaluate(hir, context).flatMap {
//       case Hir.Type(typ) => Right(typ) // 如果它已经是`ComptimeValue.TypeVal`
//       case t: Hir.Type => Right(t) // 如果它直接是`Hir.Type`节点（如Hir.TypeInt）
//       // case Hir.ResolvedId(symbol, _) => // 可能是类型别名或泛型类型构造器
//       //   // TODO: 查找symbol，如果是TypealiasDef，评估其别名类型。
//       //   // 如果是带有comptime_params的StructDef/EnumDef，则是TypeConstructor。
//       //   // 如果是不带comptime_params的直接类型定义，返回其Hir.Type表示。
//       //   Left(ComptimeError(s"Comptime type evaluation for ResolvedId '${symbol.name}' not fully implemented.", hir.ast))

//       case Hir.TypeApplication(caller, args) => // 如Vec<i32>
//         // 1. 评估`caller`以获取TypeConstructor（如Hir.TypeConstructor(Vec_symbol)）
//         // 2. 评估`args`以获取List[ComptimeValue]（如[TypeVal(Hir.TypeInt(32))]）
//         // 3. 使用这些参数"调用"TypeConstructor。这就是
//         //    `struct Vec where T`作为`comptime fn<T:Type> -> Type`的魔法所在。
//         //    这将涉及查找Vec的Hir.StructDef，然后
//         //    通过用i32替换T来创建Hir.StructInstance。
//         //    这实际上是一个专门的查询：
//         //    `context.engine.execute(InstantiateGenericTypeQuery(callerSymbol, evalArgs))`
//         Left(ComptimeError(s"Comptime type evaluation for TypeApplication not fully implemented.", hir.ast))

//       case other => Left(TypeResolutionError(
//           s"Expression does not evaluate to a type at compile time: ${other.getClass.getSimpleName}",
//           hir.ast
//         ))
//     }

// end ComptimeEngine

// // --- 全局编译上下文（占位符） ---
// trait GlobalCompilationContext:
//   def diagnostics: DiagnosticsReporter // 用于报告错误/警告
//   // def queryEngine: QueryEngine // 如果不通过ComptimeEngine传递，ComptimeContext可能需要
//   // def config: BuildConfig // 目标三元组、优化级别等

// trait DiagnosticsReporter:
//   def report(error: FlurryError): Unit
