package analysis1

import errors.FlurryError
import parse.Ast
import analysis1.QueryTypes.*
import parse.Tag

// analyzer总是先进行translate, 将ast结构转换为HIR（中间表示），
// 然后再进行分析（analyze）
case class Analyzer(ast: AstLocation, scopeId: ScopeId, engine: QueryEngine):
  def analyze(): Either[FlurryError, AnalysisResult] = for hir <- translate() yield AnalysisResult(hir)

  // 简单翻译
  def translate(): Either[FlurryError, Hir] = translate(ast.node)
  def translate(astNode: Ast): Either[FlurryError, Hir] = astNode match
    // 字面量和基本标识符
    case Ast.Id(name) => translateId(name)
    case Ast.Integer(value) => Right(Hir.Integer(BigInt(value)))
    case Ast.Real(value) => Right(Hir.Real(BigDecimal(value)))
    case Ast.Str(value) => Right(Hir.Str(value))
    case Ast.Bool(value) => Right(Hir.Bool(value))
    case Ast.LitChar(value) => Right(Hir.CharVal(value))
    case Ast.Symbol(name) => Right(Hir.Symbol(name))
    case Ast.NullVal => Right(Hir.TypeOption(Hir.TypeAny /* 占位符或稍后推断 */ )) // 或特定的Hir.Null节点
    case Ast.Unit => Right(Hir.Tuple(Nil)) // 或者如果用作类型则为Hir.TypeVoid

    // // 表达式
    // case Ast.Block(statements) => translateBlock(ast, statements)
    // case Ast.Select(expr, Ast.Id(idName)) => translateSelect(ast, expr, idName)
    // case Ast.Image(expr, Ast.Id(idName)) => translateImage(ast, expr, idName)
    // case Ast.Call(func, args) => translateCall(ast, func, args.toList)
    // // ... 其他表达式类型：BinaryOp、UnaryOp、If、Match、Object、Tuple等

    // 定义
    case Ast.FunctionDef(id, params, returnType, clauses, body) =>
      translateFunctionDef(id, params, returnType, clauses, body)
    //   translateFunctionDef(id, params, returnType, clauses, body)
    // case Ast.StructDef(id, clauses, body) => translateStructDef(id, clauses, body)
    // case Ast.EnumDef(id, clauses, body) => translateEnumDef(ast, id, clauses.toList, body.toList)
    // case Ast.ModuleDef(id, clauses, items) => translateModuleDef(ast, id, clauses.toList, items.toList)
    // ... 其他定义：TraitDef、ImplDef、Typealias、Newtype

    // 语句
    // case Ast.LetDecl(pattern, optType, init) => translateLetDecl(ast, pattern, optType, init)
    case Ast.ConstDecl(pattern, optType, init) => translateConstDecl(pattern, Some(optType), init)
    // ... 其他语句：Return、If、While、For

    // 类型
    // case Ast.PointerType(typ) => translateType(typ).map(Hir.TypePointer(_))
    // case Ast.OptionalType(typ) => translateType(typ).map(Hir.TypeOption(_))
    // ... 其他Ast.Type节点

    // 属性
    // case Ast.Attribute(attrAst, termAst) => translateAttribute(ast, attrAst, termAst)
    // case Ast.AttributeSetTrue(attrName, termAst) =>
    //   // 这是特定属性结构的语法糖
    //   val attrObject = Hir.Object(properties = Map(attrName -> Hir.Bool(true))) // 编译时值
    //   translateAttribute(ast, attrObject, termAst) // 如果translateAttribute期望attr为AST，则attrObject需要是AST

    // FileScope和ModuleDef本身不带什么信息, 暂时不处理
    case Ast.FileScope(items) => Right(Hir.Invalid)
    case Ast.ModuleDef(_, _, _) => Right(Hir.Invalid)

    case Ast.Invalid => Right(Hir.Invalid)

    // 未处理的AST节点的占位符
    case _ => Left(TranslationError(s"AST到HIR的翻译未实现：${ast.node.getClass.getSimpleName}"))

  private def translateId(name: String): Either[FlurryError, Hir] =
    // 名称解析：尝试找到这个Id引用的内容
    engine.execute[NameResolutionResult](ResolveNameQuery(name, scopeId)) match
      case Right(NameResolutionResult(symbol, _)) =>
        // 如果找到符号，返回对应的HIR
        Right(symbol.hir)
      case Left(error) => Left(error)

  private def translateFunctionDef(
      id: String,
      params: List[Ast],
      returnType: Ast,
      clauses: List[Ast],
      body: Ast
  ): Either[FlurryError, Hir.FunctionDef] =
    // 先搞个简单的, 只解析简单参数、返回值、函数体
    val parameters = for (paramAst <- params) yield translateFunctionParameter(paramAst)
    val returnTy = translate(returnType)
    val bodyHir = translateBlock(body)

    val paramErrors = parameters.collect { case Left(err) => err }
    if paramErrors.nonEmpty then Left(paramErrors.head)
    else
      for
        hirReturnType <- returnTy
        hirBody <- bodyHir
      yield Hir.FunctionDef(id, parameters.collect { case Right(param) => param }, hirReturnType, Nil, hirBody)

  private def translateFunctionParameter(paramAst: Ast): Either[FlurryError, Hir.Param] = paramAst match
    case Ast.ParamTyped(id, typ) =>
      // 翻译参数类型
      for hirType <- translate(typ) yield Hir.Param(id, hirType, Hir.Invalid)
    case _ => Left(TranslationError(s"函数参数翻译未实现：${paramAst.getClass.getSimpleName}"))

  private def translateBlock(blockAst: Ast): Either[FlurryError, Hir] =
    // 假设blockAst是一个代码块，包含多个语句
    blockAst match
      case Ast.Block(statements) =>
        val translatedStatements = statements.map(translate)
        val errors = translatedStatements.collect { case Left(err) => err }
        if errors.nonEmpty then Left(errors.head)
        else Right(Hir.Block(translatedStatements.collect { case Right(hir) => hir }))
      case _ => Left(TranslationError(s"块翻译未实现：${blockAst.getClass.getSimpleName}"))

  // 1. 为函数创建新的作用域（用于其参数和函数体）
  //    函数名本身在*父*作用域中注册
  // val funcSymbol = Symbol(name, Hir.Unresolved) // 目前的占位符HIR
  // context.scopeManager.addSymbol(context.currentScope, funcSymbol) // 在当前作用域中注册函数

  // val funcScopeId = context.scopeManager.createScope(s"fn_$name", Some(context.currentScope))
  // val funcContext = context.withScope(funcScopeId)

  // // 2. 翻译子句（编译时参数，如`where T: Type`）
  // //    这些定义在此函数的"编译时构造"期间可用的符号
  // val translatedClauses = clauseAsts.map(c => translateClause(c, funcContext))
  // // ... 收集并处理子句的错误 ...
  // val hirClauses = translatedClauses.collect { case Right(c) => c } // 简化版本

  // // 3. 翻译参数，在函数作用域中注册它们
  // val translatedParams = paramAsts.map(p => translateParam(p, funcContext))
  // // ... 收集并处理参数的错误 ...
  // val hirParams = translatedParams.collect { case Right(p) => p } // 简化版本

  // // 4. 翻译返回类型
  // val hirReturnTypeEither = translateType(returnTypeAst, funcContext)

  // // 5. 在函数作用域中翻译函数体
  // val hirBodyEither = translate(bodyAst, funcContext)

  // for
  //   retType <- hirReturnTypeEither
  //   body <- hirBodyEither
  // yield
  //   val funcDefHir = Hir.FunctionDef(name, hirParams, retType, hirClauses, body)
  //   funcSymbol.hir = funcDefHir
  //   funcDefHir.withAst(context.currentFile, astNode)

  // private def translateStructDef(id: String, clauses: List[Ast], body: List[Ast]): Either[FlurryError, Hir.StructDef] =
  //   val translatedClauses = clauseAsts.map(c => translateClause(c, structContext))
  //   // ... collect ...
  //   val hirClauses = translatedClauses.collect { case Right(c) => c }

  //   val fields = body.collect { case Ast.StructField(fieldName, typeAst, defaultAstOpt) =>
  //     for {
  //       fieldType <- translateType(typeAst, structContext)
  //       defaultHir <- defaultAstOpt.map(da => translate(da, structContext)).map(_.map(Some(_))).getOrElse(Right(None))
  //     } yield Hir.StructField(fieldName, fieldType, defaultHir.getOrElse(Hir.Invalid))
  //   }
  //   val structDefHir = Hir.StructDef(name, structScopeId, hirFields, hirClauses)
  //   structSymbol.hir = structDefHir
  //   Right(structDefHir.withAst(context.currentFile, astNode))

  private def translateConstDecl(patternAst: Ast, typeAstOpt: Option[Ast], initAst: Ast): Either[FlurryError, Hir] =
    // 常量声明的处理逻辑
    // 1. 翻译模式（pattern）
    // 2. 如果有类型注释，则翻译类型
    // 3. 翻译初始化器（initAst）
    // 4. 创建 Hir.ConstDecl 节点
    for
      pattern <- translatePattern(patternAst)
      optType <- typeAstOpt match {
        case Some(typeAst) => translate(typeAst).map(Some(_))
        case None => Right(None)
      }
      init <- translate(initAst)
    yield Hir.ConstDecl(pattern, optType, init)

  private def translatePattern(patternAst: Ast): Either[FlurryError, Hir] = patternAst match
    // 基本模式
    case Ast.Id(name) => Right(Hir.PatternAsBind(Hir.Invalid, name)) // 简单标识符绑定
    case Ast.Integer(value) => Right(Hir.Integer(BigInt(value)))
    case Ast.Real(value) => Right(Hir.Real(BigDecimal(value)))
    case Ast.Str(value) => Right(Hir.Str(value))
    case Ast.Bool(value) => Right(Hir.Bool(value))
    case Ast.LitChar(value) => Right(Hir.CharVal(value))
    case Ast.Symbol(name) => Right(Hir.Symbol(name))
    case Ast.NullVal => Right(Hir.PatternFromExpr(Hir.TypeOption(Hir.TypeAny)))
    case Ast.Unit => Right(Hir.PatternTuple(Nil))

    // 复合模式
    case Ast.Tuple(elements) =>
      val translatedElements = elements.map(translatePattern)
      val errors = translatedElements.collect { case Left(err) => err }
      if errors.nonEmpty then Left(errors.head)
      else Right(Hir.PatternTuple(translatedElements.collect { case Right(p) => p }))

    case Ast.ListOf(elements) =>
      val translatedElements = elements.map(translatePattern)
      val errors = translatedElements.collect { case Left(err) => err }
      if errors.nonEmpty then Left(errors.head)
      else Right(Hir.PatternList(translatedElements.collect { case Right(p) => p }))

    // 模式组合
    case Ast.PatternOr(left, right) =>
      for
        leftPattern <- translatePattern(left)
        rightPattern <- translatePattern(right)
      yield Hir.PatternOr(leftPattern, rightPattern)

    case Ast.PatternAsBind(pattern, Ast.Id(name)) => translatePattern(pattern).map(p => Hir.PatternAsBind(p, name))

    case Ast.PatternIfGuard(pattern, condition) =>
      for
        patternHir <- translatePattern(pattern)
        conditionHir <- Analyzer(AstLocation(ast.file, condition), scopeId, engine).translate()
      yield Hir.PatternIfGuard(patternHir, conditionHir)

    case Ast.PatternAndIs(left, expr, pattern) =>
      for
        leftHir <- translatePattern(left)
        exprHir <- Analyzer(AstLocation(ast.file, expr), scopeId, engine).translate()
        patternHir <- translatePattern(pattern)
      yield Hir.PatternAndIs(leftHir, exprHir, patternHir)

    case Ast.PatternNot(pattern) => translatePattern(pattern).map(Hir.PatternNot(_))

    case Ast.PatternOptionSome(pattern) => translatePattern(pattern).map(Hir.PatternOptionSome(_))

    case Ast.PatternErrorOk(pattern) => translatePattern(pattern).map(Hir.PatternErrorOk(_))

    // 调用模式
    case Ast.PatternCall(pattern, args) =>
      for
        patternHir <- translatePattern(pattern)
        argsHir <-
          val translatedArgs = args.map(translatePattern)
          val errors = translatedArgs.collect { case Left(err) => err }
          if errors.nonEmpty then Left(errors.head) else Right(translatedArgs.collect { case Right(p) => p })
      yield Hir.PatternCall(patternHir, argsHir)

    case Ast.PatternObjectCall(pattern, fields) =>
      for
        patternHir <- translatePattern(pattern)
        fieldsHir <-
          val translatedFields = fields.map(translatePattern)
          val errors = translatedFields.collect { case Left(err) => err }
          if errors.nonEmpty then Left(errors.head) else Right(translatedFields.collect { case Right(p) => p })
      yield Hir.PatternObjectCall(patternHir, fieldsHir)

    case Ast.PatternDiamondCall(pattern, typeArgs) =>
      for
        patternHir <- translatePattern(pattern)
        typeArgsHir <-
          val translatedTypeArgs = typeArgs
            .map(typeArg => Analyzer(AstLocation(ast.file, typeArg), scopeId, engine).translate())
          val errors = translatedTypeArgs.collect { case Left(err) => err }
          if errors.nonEmpty then Left(errors.head) else Right(translatedTypeArgs.collect { case Right(t) => t })
      yield Hir.PatternDiamondCall(patternHir, typeArgsHir)

    // 范围模式
    case Ast.PatternRangeTo(end) => translatePattern(end)
        .map(endHir => Hir.PatternRange(start = None, end = Some(endHir), inclusive = false))

    case Ast.PatternRangeToInclusive(end) => translatePattern(end)
        .map(endHir => Hir.PatternRange(start = None, end = Some(endHir), inclusive = true))

    case Ast.PatternRangeFrom(start) => translatePattern(start)
        .map(startHir => Hir.PatternRange(start = Some(startHir), end = None, inclusive = false))

    case Ast.PatternRangeFromTo(start, end) =>
      for
        startHir <- translatePattern(start)
        endHir <- translatePattern(end)
      yield Hir.PatternRange(start = Some(startHir), end = Some(endHir), inclusive = false)

    case Ast.PatternRangeFromToInclusive(start, end) =>
      for
        startHir <- translatePattern(start)
        endHir <- translatePattern(end)
      yield Hir.PatternRange(start = Some(startHir), end = Some(endHir), inclusive = true)

    // 记录模式
    case Ast.PatternRecord(fields) =>
      val translatedFields = fields.map(translatePattern)
      val errors = translatedFields.collect { case Left(err) => err }
      if errors.nonEmpty then Left(errors.head)
      else Right(Hir.PatternRecord(translatedFields.collect { case Right(p) => p }))

    case Ast.PropertyPattern(Ast.Id(id), pattern) => translatePattern(pattern)
        .map(patternHir => Hir.PropertyPattern(id, patternHir))

    // 从表达式转换的模式
    case Ast.PatternFromExpr(expr) => Analyzer(AstLocation(ast.file, expr), scopeId, engine).translate()
        .map(Hir.PatternFromExpr(_))

    case Ast.Invalid => Right(Hir.Invalid)

    // 未处理的模式
    case _ => Left(TranslationError(s"模式翻译未实现：${patternAst.getClass.getSimpleName}"))
