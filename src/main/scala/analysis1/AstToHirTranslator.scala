package analysis1

import scala.collection.mutable
import vfs.VfsNode // 假设VfsNode可用于AstLocation
import parse.Ast // 假设你的AST定义在'parse'包中
import errors.FlurryError
import errors.Severity

// // 翻译过程的上下文
// case class TranslationContext(
//     engine: QueryEngine, // 运行其他查询（如属性的编译时求值）
//     scopeManager: ScopeManager,
//     currentScope: ScopeId,
//     currentFile: VfsNode, // 用于AstLocation
//     // 任何其他相关信息，如当前包、编译时上下文标志
//     globalContext: GlobalCompilationContext // 用于诊断
// ):
//   def withScope(newScope: ScopeId): TranslationContext = this.copy(currentScope = newScope)

//   // 创建AstLocation的辅助方法
//   def location(astNode: Ast): Option[AstLocation] = Some(AstLocation(currentFile, astNode))

//   // 报告诊断的辅助方法
//   def reportError(error: FlurryError): Unit = globalContext.diagnostics.report(error)

// // HIR构建器 - 替代QueryEngine中的简单HirBuilder实现
// class AstToHirTranslator(using engine: QueryEngine): // 通过上下文参数使QueryEngine可用

//   /** 将AST节点翻译为HIR的主要入口点。这通常由Luna查询（如AstToHirQuery）调用。
//     */
//   def translate(ast: Ast, context: TranslationContext): Either[FlurryError, Hir] =
//     // 基于AST节点类型的顶层分发
//     // 每个case将调用更具体的翻译方法
//     val resultHir: Either[FlurryError, Hir] = ast match
//       // 字面量和基本标识符
//       case Ast.Id(name) => translateId(ast, name, context)
//       case Ast.Integer(value) => Right(Hir.Integer(BigInt(value)).withAst(context.currentFile, ast))
//       case Ast.Real(value) => Right(Hir.Real(BigDecimal(value)).withAst(context.currentFile, ast))
//       case Ast.Str(value) => Right(Hir.Str(value).withAst(context.currentFile, ast))
//       case Ast.Bool(value) => Right(Hir.Bool(value).withAst(context.currentFile, ast))
//       case Ast.LitChar(value) => Right(Hir.CharVal(value).withAst(context.currentFile, ast))
//       case Ast.Symbol(name) => Right(Hir.Symbol(name).withAst(context.currentFile, ast))
//       case Ast.NullVal =>
//         Right(Hir.TypeOption(Hir.TypeAny /* 占位符或稍后推断 */ ).withAst(context.currentFile, ast)) // 或特定的Hir.Null节点
//       case Ast.Unit => Right(Hir.Tuple(Nil).withAst(context.currentFile, ast)) // 或者如果用作类型则为Hir.TypeVoid

//       // 表达式
//       case Ast.Block(statements) => translateBlock(ast, statements, context)
//       case Ast.Select(expr, Ast.Id(idName)) => translateSelect(ast, expr, idName, context)
//       case Ast.Image(expr, Ast.Id(idName)) => translateImage(ast, expr, idName, context)
//       case Ast.Call(func, args) => translateCall(ast, func, args.toList, context)
//       // ... 其他表达式类型：BinaryOp、UnaryOp、If、Match、Object、Tuple等

//       // 定义
//       case Ast.FunctionDef(id, params, returnType, clauses, body) =>
//         translateFunctionDef(ast, id, params.toList, returnType, clauses.toList, body, context)
//       case Ast.StructDef(id, clauses, body) => translateStructDef(ast, id, clauses.toList, body.toList, context)
//       case Ast.EnumDef(id, clauses, body) => translateEnumDef(ast, id, clauses.toList, body.toList, context)
//       case Ast.ModuleDef(id, clauses, items) => translateModuleDef(ast, id, clauses.toList, items.toList, context)
//       // ... 其他定义：TraitDef、ImplDef、Typealias、Newtype

//       // 语句
//       case Ast.LetDecl(pattern, optType, init) => translateLetDecl(ast, pattern, optType, init, context)
//       case Ast.ConstDecl(pattern, optType, init) => translateConstDecl(ast, pattern, optType, init, context)
//       // ... 其他语句：Return、If、While、For

//       // 类型
//       case Ast.PointerType(typ) => translateType(typ, context).map(Hir.TypePointer(_))
//       case Ast.OptionalType(typ) => translateType(typ, context).map(Hir.TypeOption(_))
//       // ... 其他Ast.Type节点

//       // 属性
//       case Ast.Attribute(attrAst, termAst) => translateAttribute(ast, attrAst, termAst, context)
//       case Ast.AttributeSetTrue(attrName, termAst) =>
//         // 这是特定属性结构的语法糖
//         val attrObject = Hir.Object(properties = Map(attrName -> Hir.Bool(true))) // 编译时值
//         translateAttribute(ast, attrObject, termAst, context) // 如果translateAttribute期望attr为AST，则attrObject需要是AST

//       case Ast.Invalid => Right(Hir.Invalid.withAst(context.currentFile, ast))

//       // 未处理的AST节点的占位符
//       case _ => Left(TranslationError(s"AST到HIR的翻译未实现：${ast.getClass.getSimpleName}", context.location(ast)))

//     // 将原始AST位置附加到成功翻译的HIR
//     resultHir.map: hir =>
//       if hir.ast.isEmpty then hir.withAst(context.currentFile, ast) else hir

//   // --- 具体翻译方法 ---

//   private def translateId(astNode: Ast, name: String, context: TranslationContext): Either[FlurryError, Hir] =
//     // 名称解析：尝试找到这个Id引用的内容
//     // 这是一个简化版本。真正的名称解析器会处理遮蔽、导入等
//     // 并且可能返回不仅仅是Symbol（例如，区分值、类型、模块名称）
//     context.scopeManager.resolve(name, context.currentScope) match
//       case Some(symbol) => Right(
//           Hir.ResolvedId(symbol, symbol.hir.inferred_type.getOrElse(Hir.TypeAny /* 占位符 */ ))
//             .withAst(context.currentFile, astNode)
//         )
//       case None => Right(Hir.Unresolved.withAst(context.currentFile, astNode)) // 或特定的UnresolvedId(name)
//       // Left(NameResolutionError(s"未定义的标识符：'$name'", context.location(astNode)))

//   private def translateBlock(
//       astNode: Ast,
//       statements: List[Ast],
//       context: TranslationContext
//   ): Either[FlurryError, Hir] =
//     // 对于块，创建一个新的作用域
//     val blockScopeId = context.scopeManager.createScope(s"block_${astNode.hashCode()}", Some(context.currentScope))
//     val blockContext = context.withScope(blockScopeId)

//     val translatedStmts = statements.map(stmt => translate(stmt, blockContext))

//     // 收集结果，如果有错误则传播第一个错误
//     val hirStmts = mutable.ListBuffer[Hir]()
//     for stmtResult <- translatedStmts do
//       stmtResult match
//         case Right(hirStmt) => hirStmts += hirStmt
//         case Left(err) => return Left(err)

//     Right(Hir.Block(hirStmts.toList).withAst(context.currentFile, astNode))

//   private def translateSelect(
//       astNode: Ast,
//       expr: Ast,
//       idName: String,
//       context: TranslationContext
//   ): Either[FlurryError, Hir] =
//     // 翻译基础表达式
//     translate(expr, context).flatMap: baseHir =>
//       // 这是简化版本。实际的选择取决于baseHir的类型
//       // 如果baseHir是模块，idName是成员
//       // 如果baseHir是结构体实例，idName是字段
//       // 如果baseHir是编译时对象，idName是键
//       // 这可能涉及此处或后续阶段的类型检查或编译时求值
//       // 目前，以通用方式表示它
//       // 更健壮的HIR可能有Hir.FieldAccess、Hir.ModuleMemberAccess等
//       Right(
//         Hir.BinaryApplication(BinaryOp.Select, baseHir, Hir.Symbol(idName)).withAst(context.currentFile, astNode)
//       ) // 使用Symbol表示字段/成员名称

//   private def translateImage(
//       astNode: Ast,
//       expr: Ast,
//       idName: String,
//       context: TranslationContext
//   ): Either[FlurryError, Hir] = translate(expr, context).flatMap: baseHir =>
//     // 这里的`idName`是"图像标识符"，如"type"、"tag_name"
//     // 图像的实际计算在编译时求值或类型检查期间发生
//     // HIR只是表示操作
//     Right(Hir.UnaryApplication(UnaryOp.Image(idName), baseHir).withAst(context.currentFile, astNode))

//   private def translateCall(
//       astNode: Ast,
//       funcAst: Ast,
//       argAsts: List[Ast],
//       context: TranslationContext
//   ): Either[FlurryError, Hir] =
//     for
//       funcHir <- translate(funcAst, context)
//       argHirsSequence = argAsts.map(arg => translate(arg, context))
//       // 收集argHirs，传播错误
//       argHirs <- argHirsSequence.foldRight(Right(Nil): Either[FlurryError, List[Hir]]): (nextEither, accEither) =>
//         for
//           acc <- accEither
//           next <- nextEither
//         yield next :: acc
//     yield
//     // 这是一个通用调用。它可能是运行时函数调用、
//     // 编译时函数调用或类型构造器应用（如Vec<i32>）
//     // 区别将在类型检查/编译时求值期间确定
//     // 如果funcHir是指向泛型FunctionDef的Hir.ResolvedId，并且args是类型，
//     // 它就是类型应用
//     // 如果funcHir是编译时函数，并且args是编译时值，它就是编译时调用
//     Hir.TypeApplication(funcHir, argHirs).withAst(context.currentFile, astNode) // 目前使用TypeApplication，HIR需要更具体的调用节点

//   // --- 定义 ---
//   private def translateFunctionDef(
//       astNode: Ast,
//       name: String,
//       paramAsts: List[Ast],
//       returnTypeAst: Ast,
//       clauseAsts: List[Ast],
//       bodyAst: Ast,
//       context: TranslationContext
//   ): Either[FlurryError, Hir.FunctionDef] =
//     // 1. 为函数创建新的作用域（用于其参数和函数体）
//     //    函数名本身在*父*作用域中注册
//     val funcSymbol = Symbol(name, Hir.Unresolved) // 目前的占位符HIR
//     context.scopeManager.addSymbol(context.currentScope, funcSymbol) // 在当前作用域中注册函数

//     val funcScopeId = context.scopeManager.createScope(s"fn_$name", Some(context.currentScope))
//     val funcContext = context.withScope(funcScopeId)

//     // 2. 翻译子句（编译时参数，如`where T: Type`）
//     //    这些定义在此函数的"编译时构造"期间可用的符号
//     val translatedClauses = clauseAsts.map(c => translateClause(c, funcContext))
//     // ... 收集并处理子句的错误 ...
//     val hirClauses = translatedClauses.collect { case Right(c) => c } // 简化版本

//     // 3. 翻译参数，在函数作用域中注册它们
//     val translatedParams = paramAsts.map(p => translateParam(p, funcContext))
//     // ... 收集并处理参数的错误 ...
//     val hirParams = translatedParams.collect { case Right(p) => p } // 简化版本

//     // 4. 翻译返回类型
//     val hirReturnTypeEither = translateType(returnTypeAst, funcContext)

//     // 5. 在函数作用域中翻译函数体
//     val hirBodyEither = translate(bodyAst, funcContext)

//     for
//       retType <- hirReturnTypeEither
//       body <- hirBodyEither
//     yield
//       val funcDefHir = Hir.FunctionDef(name, hirParams, retType, hirClauses, body)
//       funcSymbol.hir = funcDefHir // 现在我们有了HIR，更新符号的HIR
//       funcDefHir.withAst(context.currentFile, astNode)
//   private def translateStructDef(
//       astNode: Ast,
//       name: String,
//       clauseAsts: List[Ast],
//       fieldAsts: List[Ast],
//       context: TranslationContext
//   ): Either[FlurryError, Hir.StructDef] = {
//     val structSymbol = Symbol(name, Hir.Unresolved)
//     context.scopeManager.addSymbol(context.currentScope, structSymbol)

//     // Structs also define a scope for their members (e.g., methods, associated types if any)
//     // and for their comptime parameters (clauses).
//     val structScopeId = context.scopeManager.createScope(s"struct_$name", Some(context.currentScope))
//     val structContext = context.withScope(structScopeId)

//     val translatedClauses = clauseAsts.map(c => translateClause(c, structContext))
//     // ... collect ...
//     val hirClauses = translatedClauses.collect { case Right(c) => c }

//     // Translate fields. Field definitions don't create new sub-scopes for name resolution here,
//     // they are part of the struct's definition.
//     val translatedFields = fieldAsts.map {
//       case Ast.StructField(fieldName, typeAst, defaultAstOpt) => for {
//           fieldType <- translateType(typeAst, structContext) // Types in fields resolved in struct's (comptime) scope
//           defaultHir <-
//             defaultAstOpt.map(da => translate(da, structContext)).map(_.map(Some(_)))
//               .getOrElse(Right(None)) // Translate optional default
//         } yield Hir.StructField(fieldName, fieldType, defaultHir.getOrElse(Hir.Invalid)) // Assuming Hir.StructField expects a Hir for default
//       case other => Left(TranslationError(s"Unexpected AST node in struct body: $other", context.location(other)))
//     }
//     // ... collect ...
//     val hirFields = translatedFields.collect { case Right(f) => f }

//     val structDefHir = Hir.StructDef(name, structScopeId, hirFields, hirClauses)
//     structSymbol.hir = structDefHir
//     Right(structDefHir.withAst(context.currentFile, astNode))
//   }

//   private def translateEnumDef(
//       astNode: Ast,
//       name: String,
//       clauseAsts: List[Ast],
//       variantAsts: List[Ast],
//       context: TranslationContext
//   ): Either[FlurryError, Hir.EnumDef] = {
//     val enumSymbol = Symbol(name, Hir.Unresolved)
//     context.scopeManager.addSymbol(context.currentScope, enumSymbol)
//     val enumScopeId = context.scopeManager.createScope(s"enum_$name", Some(context.currentScope))
//     val enumContext = context.withScope(enumScopeId)

//     val hirClauses = clauseAsts.flatMap(c => translateClause(c, enumContext).toOption)

//     val hirVariants = variantAsts.flatMap {
//       case Ast.EnumVariantWithPattern(vName, patAst) => translate(patAst, enumContext)
//           .map(Hir.EnumVariantWithPattern(vName, _)).toOption
//       // TODO: Other variant types
//       case _ => None
//     }
//     val enumDefHir = Hir.EnumDef(name, enumScopeId, hirVariants, hirClauses)
//     enumSymbol.hir = enumDefHir
//     Right(enumDefHir.withAst(context.currentFile, astNode))
//   }

//   private def translateModuleDef(
//       astNode: Ast,
//       name: String,
//       clauseAsts: List[Ast],
//       itemAsts: List[Ast],
//       context: TranslationContext
//   ): Either[FlurryError, Hir.ModuleDef] = {
//     val moduleSymbol = Symbol(name, Hir.Unresolved)
//     context.scopeManager.addSymbol(context.currentScope, moduleSymbol)
//     val moduleScopeId = context.scopeManager.createScope(name, Some(context.currentScope))
//     val moduleContext = context.withScope(moduleScopeId)

//     val hirClauses = clauseAsts.flatMap(c => translateClause(c, moduleContext).toOption)

//     // Translate items within the module's new scope
//     val translatedItems = itemAsts.map(item => translate(item, moduleContext))
//     // ... collect and handle errors ...
//     val hirItems = translatedItems.collect { case Right(i) => i }

//     val moduleDefHir = Hir.ModuleDef(name, moduleScopeId, hirClauses) // Assuming items are registered in scope directly
//     moduleSymbol.hir = moduleDefHir
//     Right(moduleDefHir.withAst(context.currentFile, astNode))
//   }

//   // --- 语句 ---
//   private def translateLetDecl(
//       astNode: Ast,
//       patternAst: Ast,
//       typeAstOpt: Option[Ast],
//       initAst: Ast,
//       context: TranslationContext
//   ): Either[FlurryError, Hir] =
//     // 1. 首先翻译初始化器（通常有助于模式的类型推断）
//     translate(initAst, context).flatMap { initHir =>
//       // 2. 翻译可选的类型注释
//       val expectedTypeHirEither: Either[FlurryError, Option[Hir.Type]] = typeAstOpt match {
//         case Some(typeAst) => translateType(typeAst, context).map(Some(_))
//         case None => Right(None)
//       }

//       expectedTypeHirEither.flatMap { optExpectedType =>
//         // 3. 翻译模式。这是复杂的，涉及解构。
//         //    模式翻译需要知道`initHir`（或`optExpectedType`）的类型
//         //    以正确绑定名称并在当前作用域中创建符号。
//         //    这可能涉及一个子过程或查询，用于"将类型检查模式与类型进行匹配"。
//         //    目前，翻译是简化的。
//         translatePattern(patternAst, context, Some(initHir.inferred_type.getOrElse(Hir.TypeAny))).map { patternHir =>
//           // TODO: 实际上将patternHir中的符号注册到context.currentScope
//           // TODO: 将initHir与patternHir / optExpectedType进行类型检查
//           // 目前，返回一个通用的Assign或一个特定的LetDecl HIR节点（如果你有的话）
//           Hir.Assign(patternHir, initHir).withAst(context.currentFile, astNode) // 占位符HIR
//         }
//       }
//     }

//   private def translateConstDecl(
//       astNode: Ast,
//       patternAst: Ast,
//       typeAstOpt: Option[Ast],
//       initAst: Ast,
//       context: TranslationContext
//   ): Either[FlurryError, Hir] =
//     // Const declarations require the initializer to be a comptime value.
//     // 1. Attempt to evaluate initAst at compile time.
//     //    This is where AstToHir itself might call the ComptimeEngine or a comptime eval query.
//     //    This shows the interwoven nature. For now, let's assume initAst is simple enough
//     //    or that comptime evaluation happens *after* an initial HIR is formed.
//     //    A simpler approach for this pass: translate initAst to initHir, then mark it as needing comptime eval.
//     translate(initAst, context).flatMap { initHir =>
//       // Here, you would ideally trigger a comptime evaluation query for initHir.
//       // engine.execute(EvaluateComptimeQuery(initHir, createComptimeContext(...)))
//       // For this translator, we might just create the HIR and let a later query do the comptime eval.
//       // Or, if initHir IS ALREADY a comptime literal (e.g. Hir.Integer), it's fine.

//       // (Similar logic to let for pattern and type)
//       translatePattern(patternAst, context, None).map { patternHir =>
//         // TODO: Register const symbols. Ensure initHir is comptime.
//         // This needs a Hir.ConstDecl node.
//         Hir.Assign(patternHir, initHir).withAttribute("flurry_keyword_comptime_const", Hir.Bool(true)) // Mark as const
//           .withAst(context.currentFile, astNode)
//       }
//     }

//   // --- 类型、参数、子句、属性（辅助方法）---
//   private def translateType(typeAst: Ast, context: TranslationContext): Either[FlurryError, Hir.Type] = {
//     // This needs to handle all Ast.Type variants and convert them to Hir.Type
//     // For Ast.Id used as a type, it's a name resolution query.
//     // For Ast.DiamondCall (like Vec<i32>), it's a type application.
//     val translated = typeAst match
//       case Ast.Id(name) => context.scopeManager.resolve(name, context.currentScope) match
//           case Some(symbol) if symbol.hir.isInstanceOf[Hir.Type] =>
//             Right(symbol.hir.asInstanceOf[Hir.Type]) // Simplified
//           case Some(symbol) if symbol.hir.isInstanceOf[Hir.StructDef] => // It's a type constructor like 'Vec'
//             Right(Hir.TypeConstructor(symbol)) // TODO: Refine Hir.Type representation
//           case _ => Left(TypeResolutionError(s"Unknown type name: $name", context.location(typeAst)))
//       case Ast.PointerType(pointeeAst) => translateType(pointeeAst, context).map(Hir.TypePointer(_))
//       case Ast.OptionalType(innerAst) => translateType(innerAst, context).map(Hir.TypeOption(_))
//       case Ast.DiamondCall(baseTypeAst, typeArgAsts) => // e.g. Vec<T, N>
//         for {
//           baseTypeConstructor <- translateType(baseTypeAst, context) // Should resolve to a TypeConstructor
//           argTypes <- Right(typeArgAsts.map(arg => translateType(arg, context))) // This needs to collect Eithers
//           // simplified collection:
//           resolvedArgTypes = argTypes.collect { case Right(t) => t }
//           // if resolvedArgTypes.length != typeArgAsts.length then ... error ...
//         } yield Hir.TypeApplication(baseTypeConstructor, resolvedArgTypes)
//       // TODO: Other Ast type variants
//       case _ => Left(TypeResolutionError(
//           s"Cannot translate AST type: ${typeAst.getClass.getSimpleName}",
//           context.location(typeAst)
//         ))

//     // Ensure all results are Hir.Type, potentially wrapping if necessary
//     translated.map {
//       case t: Hir.Type => t
//       case otherHir => Hir.TypeAnonymous(otherHir) // Or an error if it's not a valid type form
//     }
//   }

//   private def translateParam(paramAst: Ast, context: TranslationContext): Either[FlurryError, Hir.Param] =
//     paramAst match {
//       case Ast.ParamTyped(id, typeAst) => translateType(typeAst, context).map { hirType =>
//           val paramSymbol = Symbol(id, hirType) // Simplified, type is param's type, not symbol's HIR def
//           context.scopeManager.addSymbol(context.currentScope, paramSymbol) // Register param in func scope
//           Hir.Param(id, hirType, Hir.Invalid) // Default value handling needed
//         }
//       // TODO: Other Ast.Param variants (Optional, TraitBound, Self, etc.)
//       case _ => Left(TranslationError(s"Unsupported parameter AST: $paramAst", context.location(paramAst)))
//     }

//   private def translateClause(clauseAst: Ast, context: TranslationContext): Either[FlurryError, Hir] = clauseAst match {
//     case Ast.ClauseTypeDecl(id) => Right(Hir.ClauseDeclTypeWithoutTyped(id))
//     case Ast.ClauseDecl(id, typeAst, defaultAstOpt) => for {
//         clauseType <- translateType(typeAst, context)
//         defaultHir <- defaultAstOpt.map(da => translate(da, context)).map(_.map(Some(_))).getOrElse(Right(None))
//       } yield Hir.ClauseDecl(id, clauseType, defaultHir.getOrElse(Hir.Invalid))
//     // TODO: Other Ast.Clause variants
//     case _ => Left(TranslationError(s"Unsupported clause AST: $clauseAst", context.location(clauseAst)))
//   }

//   private def translatePattern(
//       patternAst: Ast,
//       context: TranslationContext,
//       expectedTypeHint: Option[Hir.Type]
//   ): Either[FlurryError, Hir] =
//     // 模式翻译很复杂，因为它引入绑定
//     // 它通常需要来自要匹配的值的类型信息
//     // 这是一个非常简化的版本
//     patternAst match
//       case Ast.Id(name) => // 绑定模式
//         // 这个新符号的类型取决于匹配的上下文
//         // 现在，让我们假设它获得expectedTypeHint或Any
//         val symbolType = expectedTypeHint.getOrElse(Hir.TypeAny)
//         val symbol = Symbol(name, symbolType) // 为了简单起见，符号的HIR在这里是其类型
//         context.scopeManager.addSymbol(context.currentScope, symbol)
//         Right(Hir.ResolvedId(symbol, symbolType)) // 表示模式变量本身
//       case Ast.Integer(v) => Right(Hir.Integer(BigInt(v))) // 字面量模式
//       // TODO: 其他Ast.Pattern变体（Tuple、Struct、构造器调用等）
//       case _ => Left(TranslationError(s"模式翻译未实现：$patternAst", context.location(patternAst)))
//   private def translateAttribute(
//       astNode: Ast,
//       attrAst: Ast,
//       termAst: Ast,
//       context: TranslationContext
//   ): Either[FlurryError, Hir] =
//     // 1. 首先翻译术语（被归属的东西）
//     translate(termAst, context).flatMap: termHir =>
//       // 2. 在编译时评估属性表达式
//       //    这需要ComptimeContext。我们需要引导一个或向下传递
//       //    这是QueryEngine交互的主要示例：
//       //    val attrValueEither = engine.execute(EvaluateComptimeQuery(attrHirFromAst, makeComptimeContext(...)))

//       // 简化版本：将attrAst翻译为attributeHir，假设它已经是编译时值
//       // 完整实现将涉及在此处调用编译时引擎
//       translate(attrAst, context).flatMap: attrValueHir =>
//         // 确保attrValueHir是属性的有效编译时表示（如Hir.Object或Hir.Bool）
//         // 现在，我们假设它是

//         // 将属性添加到termHir
//         // 这假设Hir节点有存储属性的方法
//         // 我们为此向Hir添加了`Attribute`特征
//         termHir match
//           case ah: Hir with Attribute =>
//             // 如何构造属性？如果attrAst是`{ .foo bar }`
//             // 那么attrValueHir可能是Hir.Object(Map("foo" -> barHir))
//             // 如果它只是`.foo`，它可能是Hir.Symbol("foo")
//             // 对于AttributeSetTrue，attrName是键，值是Hir.Bool(true)
//             // 这部分需要属性结构的约定
//             // 让我们现在假设属性存储在Hir节点的映射中
//             // 如果attrValueHir是Object，合并其属性。如果是Symbol或Bool，使用其名称
//             attrValueHir match
//               case Hir.Object(_, props) => Right(ah.withChangeSet(props)) // 假设withChangeSet存在
//               case Hir.Symbol(symName) => Right(ah.withAttribute(symName, Hir.Bool(true))) // 简单符号属性的约定
//               case Hir.Bool(true) if attrAst.isInstanceOf[Ast.Symbol] /* 粗略检查 */ =>
//                 Right(ah.withAttribute(attrAst.asInstanceOf[Ast.Symbol].name, Hir.Bool(true)))
//               case _ if astNode.isInstanceOf[Ast.AttributeSetTrue] => // 处理Ast.AttributeSetTrue
//                 val attrName = astNode.asInstanceOf[Ast.AttributeSetTrue].attrName
//                 Right(ah.withAttribute(attrName, Hir.Bool(true)))

//               case _ => Left(TranslationError(s"无效的属性值结构：$attrValueHir", context.location(attrAst)))
//           case _ =>
//             Left(TranslationError(s"无法将属性应用于类型为${termHir.getClass.getSimpleName}的HIR节点", context.location(termAst)))

// 定义翻译阶段的特定错误
case class TranslationError(message: String, location: Option[AstLocation] = None, override val code: Int = 4001)
    extends FlurryError:
  override def errorMessage: String = message
  override def severity: Severity = Severity.Error
