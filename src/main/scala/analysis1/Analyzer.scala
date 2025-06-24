package analysis1

import errors.FlurryError
import parse.Ast
import analysis1.QueryTypes.*
import parse.Tag

// analyzer总是先进行translat
// 然后再进行分析（analyze）
case class Analyzer(ast: AstLocation, scopeId: ScopeId, engine: QueryEngine):
  def analyze(): Either[FlurryError, AnalysisResult] =
    for
      translatedHir <- translate()
      analyzedHir <- analyzeExpr(translatedHir)
    yield AnalysisResult(analyzedHir)

  // 接收初步translated HIR进行分析
  def analyzeFunction(func: Hir.FunctionDef): Either[FlurryError, Hir] =
    // 1. 在当前作用域中分析参数类型和返回类型
    val paramTypeResults = func.params.map(param => analyzeExpr(param.paramType))
    val returnTypeResult = analyzeExpr(func.returnType)

    // 收集类型分析错误
    val paramTypeErrors = paramTypeResults.collect { case Left(err) => err }
    val typeErrors = returnTypeResult.left.toOption.toList ++ paramTypeErrors

    if typeErrors.nonEmpty then Left(typeErrors.head)
    else
      // 2. 创建函数体的匿名、有序作用域
      val functionScope = engine.scopeManager.createScope("", Some(scopeId), ordered = true)

      // 3. 将分析过的参数添加到函数作用域中
      val analyzedParams: List[Hir.Param] = func.params.zip(paramTypeResults.collect { case Right(paramType) =>
        paramType
      }).map { case (param, analyzedParamType) =>
        // 将参数作为局部变量添加到函数作用域 - 存储Var节点而不是类型
        engine.scopeManager.addSymbol(functionScope, Symbol(param.name, Hir.Var(param.name, analyzedParamType)))
        // 返回分析过的参数
        param.copy(paramType = analyzedParamType)
      }

      val analyzedReturnType = returnTypeResult.getOrElse(func.returnType)

      // 4. 在函数作用域中分析函数体
      val bodyAnalyzer = Analyzer(ast, functionScope, engine)
      val bodyResult = bodyAnalyzer.analyzeExpr(func.body)

      bodyResult match
        case Right(analyzedBody) =>
          Right(func.copy(params = analyzedParams, returnType = analyzedReturnType, body = analyzedBody))
        case Left(error) => Left(error)

  def analyzeExpr(expr: Hir): Either[FlurryError, Hir] =
    // 对表达式进行类型检查和分析
    expr match
      // 基本类型和字面量
      case Hir.Integer(_) | Hir.Real(_) | Hir.Str(_) | Hir.Bool(_) | Hir.CharVal(_) | Hir.Symbol(_) => Right(expr)

      // 类型定义直接返回
      case Hir.TypeObject | Hir.TypeType | Hir.TypeInteger | Hir.TypeReal | Hir.TypeBool | Hir.TypeStr | Hir.TypeChar |
          Hir.TypeSymbol | Hir.TypeVoid | Hir.TypeNoReturn | Hir.TypeAny => Right(expr)

      // 复合类型
      case Hir.TypePointer(to) => analyzeExpr(to).map(analyzedTo => Hir.TypePointer(analyzedTo))

      case Hir.TypeOption(to) => analyzeExpr(to).map(analyzedTo => Hir.TypeOption(analyzedTo))

      case Hir.TypeArray(elementType, size) =>
        for
          analyzedElementType <- analyzeExpr(elementType)
          analyzedSize <- analyzeExpr(size)
        yield Hir.TypeArray(analyzedElementType, analyzedSize)

      case Hir.TypeFunction(isPure, isComptime, isDiamond, params, returnType) =>
        val paramResults = params.map(analyzeExpr)
        val paramErrors = paramResults.collect { case Left(err) => err }

        if paramErrors.nonEmpty then Left(paramErrors.head)
        else
          val analyzedParams = paramResults.collect { case Right(hir) => hir }
          analyzeExpr(returnType).map(analyzedReturnType =>
            Hir.TypeFunction(isPure, isComptime, isDiamond, analyzedParams, analyzedReturnType)
          )

      // 代码块 - 创建新的匿名、有序作用域
      case Hir.Block(statements) =>
        val blockScope = engine.scopeManager.createScope("", Some(scopeId), ordered = true)
        val blockAnalyzer = Analyzer(ast, blockScope, engine)
        val results = statements.map(blockAnalyzer.analyzeExpr)
        val errors = results.collect { case Left(err) => err }
        if errors.nonEmpty then Left(errors.head) else Right(Hir.Block(results.collect { case Right(hir) => hir }))

      // 函数定义
      case func: Hir.FunctionDef => analyzeFunction(func)
      // 元组和列表
      case Hir.Tuple(elements, ty) =>
        val elementResults = elements.map(analyzeExpr)
        val elementErrors = elementResults.collect { case Left(err) => err }

        if elementErrors.nonEmpty then Left(elementErrors.head)
        else
          val analyzedElements = elementResults.collect { case Right(hir) => hir }
          analyzeExpr(ty).map(analyzedTy => Hir.Tuple(analyzedElements, analyzedTy))

      case Hir.ListVal(elements, ty) =>
        val elementResults = elements.map(analyzeExpr)
        val elementErrors = elementResults.collect { case Left(err) => err }

        if elementErrors.nonEmpty then Left(elementErrors.head)
        else
          val analyzedElements = elementResults.collect { case Right(hir) => hir }
          analyzeExpr(ty).map(analyzedTy => Hir.ListVal(analyzedElements, analyzedTy))

      // 变量引用和名称解析
      case Hir.Unresolved(name) =>
        // 进行名称解析
        println(s"Analyzing unresolved name: $name in scope: $scopeId")
        engine.execute[NameResolutionResult](ResolveNameQuery(name, scopeId)) match
          case Right(NameResolutionResult(symbol, _)) =>
            println(s"Successfully resolved $name to ${engine.dumpHir(symbol.hir)}")
            // 区分类型和变量引用
            symbol.hir match
              case Hir.TypeInt(_, _) | Hir.TypeInteger | Hir.TypeReal | Hir.TypeBool | Hir.TypeStr | Hir.TypeChar | Hir
                    .TypeSymbol | Hir.TypeAny | _: Hir.TypePointer | _: Hir.TypeOption | _: Hir.TypeArray |
                  _: Hir.TypeFunction | Hir.TypeObject | Hir.TypeType | _: Hir.TypeFloat =>
                // 这是类型引用，直接返回类型
                Right(symbol.hir)
              case Hir.Var(varName, varType) =>
                // 这是变量定义，创建VarRef节点
                Right(Hir.VarRef(varName, varType))
              case _ =>
                // 其他情况，可能是函数或其他定义
                Right(symbol.hir)
          case Left(error) =>
            println(s"Failed to resolve $name: $error")
            Left(error)

      // 常量声明 - 分析并添加到作用域
      case Hir.ConstDecl(pattern, optType, init) =>
        // 1. 分析初始化表达式
        for
          analyzedInit <- analyzeExpr(init)
          analyzedOptType <- optType match
            case Some(typeExpr) => analyzeExpr(typeExpr).map(Some(_))
            case None => Right(None)
          // 2. 提取绑定的变量名并添加到作用域
          _ = addPatternBindingsToScope(pattern, analyzedOptType.getOrElse(analyzedInit))
          analyzedPattern <- analyzeExpr(pattern)
        yield Hir.ConstDecl(analyzedPattern, analyzedOptType, analyzedInit)

      // 变量引用
      case varRef: Hir.VarRef => Right(varRef)

      // 无效节点
      case Hir.Invalid => Right(expr)

      // 模式节点 - 一般不需要进一步分析，直接返回
      case pattern: Hir.PatternAsBind => Right(pattern)
      case pattern: Hir.PatternTuple => Right(pattern)
      case pattern: Hir.PatternList => Right(pattern)
      case pattern: Hir.PatternOr => Right(pattern)
      case pattern: Hir.PatternIfGuard => Right(pattern)
      case pattern: Hir.PatternAndIs => Right(pattern)
      case pattern: Hir.PatternNot => Right(pattern)
      case pattern: Hir.PatternOptionSome => Right(pattern)
      case pattern: Hir.PatternErrorOk => Right(pattern)
      case pattern: Hir.PatternCall => Right(pattern)
      case pattern: Hir.PatternObjectCall => Right(pattern)
      case pattern: Hir.PatternDiamondCall => Right(pattern)
      case pattern: Hir.PatternRange => Right(pattern)
      case pattern: Hir.PatternRecord => Right(pattern)
      case pattern: Hir.PropertyPattern => Right(pattern)
      case pattern: Hir.PatternFromExpr => Right(pattern)

      // 未处理的表达式类型
      case _ =>
        println(s"Unhandled HIR node in analyzeExpr: ${expr.getClass.getSimpleName}: $expr")
        Left(TranslationError(s"表达式分析未实现：${expr.getClass.getSimpleName}"))

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
    case Ast.NullVal => Right(Hir.NullVal) // 使用更精确的HIR节点
    case Ast.Unit => Right(Hir.Tuple(Nil)) // 或者如果用作类型则为Hir.TypeVoid

    // 表达式
    case Ast.Block(statements) => translateBlock(Ast.Block(statements))
    case Ast.Tuple(elements) =>
      val translatedElements = elements.map(translate)
      val errors = translatedElements.collect { case Left(err) => err }
      if errors.nonEmpty then Left(errors.head)
      else Right(Hir.Tuple(translatedElements.collect { case Right(hir) => hir }))

    case Ast.ListOf(elements) =>
      val translatedElements = elements.map(translate)
      val errors = translatedElements.collect { case Left(err) => err }
      if errors.nonEmpty then Left(errors.head)
      else Right(Hir.ListVal(translatedElements.collect { case Right(hir) => hir }))

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
    // 在翻译阶段只创建未解析的标识符，实际的名称解析在分析阶段进行
    Right(Hir.Unresolved(name))

  private def translateFunctionDef(
      id: String,
      params: List[Ast],
      returnType: Ast,
      clauses: List[Ast],
      body: Ast
  ): Either[FlurryError, Hir.FunctionDef] =
    // 翻译函数参数
    val paramResults = params.map(translateFunctionParameter)
    val paramErrors = paramResults.collect { case Left(err) => err }

    if paramErrors.nonEmpty then Left(paramErrors.head)
    else
      // 翻译返回类型
      val returnTypeResult = translate(returnType)

      // 翻译子句 (clauses)
      val clauseResults = clauses.map(translate)
      val clauseErrors = clauseResults.collect { case Left(err) => err }

      // 翻译函数体
      val bodyResult = translate(body)

      // 收集所有错误
      val allErrors = List(returnTypeResult, bodyResult).collect { case Left(err) => err } ++ clauseErrors

      if allErrors.nonEmpty then Left(allErrors.head)
      else
        val translatedParams = paramResults.collect { case Right(param) => param }
        val translatedReturnType = returnTypeResult.getOrElse(Hir.TypeVoid)
        val translatedClauses = clauseResults.collect { case Right(clause) => clause }
        val translatedBody = bodyResult.getOrElse(Hir.Invalid)

        Right(Hir.FunctionDef(
          name = id,
          params = translatedParams,
          returnType = translatedReturnType,
          clauses = translatedClauses,
          body = translatedBody
        ))

  private def translateFunctionParameter(paramAst: Ast): Either[FlurryError, Hir.Param] = paramAst match
    case Ast.ParamTyped(id, typ) =>
      // 有类型注释的参数
      for hirType <- translate(typ) yield Hir.Param(id, hirType, Hir.Invalid)

    case Ast.ParamOptional(id, typ, default) =>
      // 有类型注释和默认值的参数
      for
        hirType <- translate(typ)
        hirDefault <- translate(default)
      yield Hir.Param(id, hirType, hirDefault)

    case Ast.ParamId(id) =>
      // 简单参数名，无类型注释
      Right(Hir.Param(id, Hir.TypeAny, Hir.Invalid)) // 类型稍后推断

    case Ast.ParamOptionalId(id) =>
      // 可选参数，无类型注释
      Right(Hir.Param(id, Hir.TypeOption(Hir.TypeAny), Hir.Invalid)) // 类型稍后推断

    case Ast.ParamTraitBound(id, traitBound) =>
      // 有trait约束的参数
      for hirTraitBound <- translate(traitBound) yield Hir.Param(id, hirTraitBound, Hir.Invalid)

    case Ast.ParamRestBind(id, typ) =>
      // 剩余参数绑定
      for hirType <- translate(typ) yield Hir.Param(id, Hir.TypeArray(hirType, Hir.Invalid), Hir.Invalid)

    case Ast.ParamSelf =>
      // self参数
      Right(Hir.Param("self", Hir.TypeAny, Hir.Invalid)) // 类型稍后从上下文推断

    case Ast.ParamSelfRef =>
      // &self参数
      Right(Hir.Param("self", Hir.TypePointer(Hir.TypeAny), Hir.Invalid))

    case Ast.ParamItself =>
      // itself参数
      Right(Hir.Param("itself", Hir.TypeAny, Hir.Invalid))

    case Ast.ParamItselfRef =>
      // &itself参数
      Right(Hir.Param("itself", Hir.TypePointer(Hir.TypeAny), Hir.Invalid))

    case Ast.Id(id) =>
      // 兜底：简单标识符作为参数
      Right(Hir.Param(id, Hir.TypeAny, Hir.Invalid))

    case _ => Left(TranslationError(s"函数参数翻译未实现：${paramAst.getClass.getSimpleName}"))

  private def translateBlock(blockAst: Ast): Either[FlurryError, Hir] = blockAst match
    case Ast.Block(statements) =>
      val translatedStatements = statements.map(translate)
      val errors = translatedStatements.collect { case Left(err) => err }
      if errors.nonEmpty then Left(errors.head)
      else Right(Hir.Block(translatedStatements.collect { case Right(hir) => hir }))

    case _ =>
      // 如果不是Block，直接翻译这个AST节点
      translate(blockAst)

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

  // 帮助方法：将模式中的绑定添加到当前作用域
  private def addPatternBindingsToScope(pattern: Hir, varType: Hir): Unit = pattern match
    case Hir.PatternAsBind(_, name, _) =>
      // 简单变量绑定 - 存储Var节点而不是类型
      engine.scopeManager.addSymbol(scopeId, Symbol(name, Hir.Var(name, varType)))
    case Hir.PatternTuple(elements, _) =>
      // 元组模式：递归处理每个元素
      elements.foreach(addPatternBindingsToScope(_, varType)) // 简化：所有元素使用相同类型
    case Hir.PatternList(elements, _) =>
      // 列表模式：递归处理每个元素
      elements.foreach(addPatternBindingsToScope(_, varType))
    case _ =>
      // 其他模式暂时不处理
      ()
