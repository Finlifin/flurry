package analysis1

import parse.Ast
import errors.*
import vfs.VfsNode
import scala.collection.mutable
import QueryTypes.*

// Type Environment for contextual typing
case class TypeEnv(vars: Map[String, Hir] = Map.empty, parent: Option[TypeEnv] = None):
  def lookup(name: String): Option[Hir] = vars.get(name).orElse(parent.flatMap(_.lookup(name)))
  def extend(name: String, ty: Hir): TypeEnv = copy(vars = vars + (name -> ty))
  def extendAll(bindings: Map[String, Hir]): TypeEnv = copy(vars = vars ++ bindings)

// Contextual typing context (from bidirectional typing)
enum TypingContext:
  case EmptyContext // □
  case TypeExpectation(expected: Hir) // A (expected type)
  case ArgumentContext(argType: Hir, outer: TypingContext) // arg teleportation
  case FieldContext(fieldName: String, outer: TypingContext) // field teleportation

// Type substitution
type TypeSubst = Map[String, Hir]

// Analysis errors specific to typing
case class TypeError(message: String, location: Option[SourceLocation] = None, span: Option[SourceSpan] = None)
    extends FlurryError:
  override def errorMessage: String = message
  override def code: Int = 2000
  override def severity: Severity = Severity.Error

case class UnificationError(type1: Hir, type2: Hir, location: Option[SourceLocation] = None) extends FlurryError:
  override def errorMessage: String = s"Cannot unify types: $type1 and $type2"
  override def code: Int = 2001

case class OccursCheckError(variable: String, containingType: Hir, location: Option[SourceLocation] = None)
    extends FlurryError:
  override def errorMessage: String = s"Occurs check failed: variable $variable occurs in type $containingType"
  override def code: Int = 2002

case class UndefinedVariableError(name: String, location: Option[SourceLocation] = None) extends FlurryError:
  override def errorMessage: String = s"Undefined variable: $name"
  override def code: Int = 2003

// Type inference state - now immutable for query-based system
case class InferenceState(
    nextTypeVarId: Int = 0,
    constraints: List[(Hir, Hir)] = Nil,
    substitution: TypeSubst = Map.empty
):
  def freshTypeVar(): (Hir, InferenceState) =
    val newVar = Hir.TypeVar(s"t$nextTypeVarId")
    val newState = copy(nextTypeVarId = nextTypeVarId + 1)
    (newVar, newState)

  def addConstraint(t1: Hir, t2: Hir): InferenceState = copy(constraints = (t1, t2) :: constraints)

  def applySubst(subst: TypeSubst): InferenceState = copy(
    substitution = substitution ++ subst,
    constraints = constraints.map((t1, t2) => (applySubstToType(t1, subst), applySubstToType(t2, subst)))
  )

// Apply substitution to a type
def applySubstToType(ty: Hir, subst: TypeSubst): Hir = ty match
  case Hir.TypeVar(name) => subst.getOrElse(name, ty)
  case Hir.TypeFunction(isPure, isComptime, isDiamond, params, ret) => Hir
      .TypeFunction(isPure, isComptime, isDiamond, params.map(applySubstToType(_, subst)), applySubstToType(ret, subst))
  case Hir.Tuple(elems, _) => Hir.Tuple(elems.map(applySubstToType(_, subst)))
  case Hir.TypePointer(inner) => Hir.TypePointer(applySubstToType(inner, subst))
  case Hir.TypeOption(inner) => Hir.TypeOption(applySubstToType(inner, subst))
  case Hir.TypeWithEffects(effect, ret) => Hir
      .TypeWithEffects(applySubstToType(effect, subst), applySubstToType(ret, subst))
  case Hir.TypeWithErrors(error, ret) => Hir
      .TypeWithErrors(applySubstToType(error, subst), applySubstToType(ret, subst))
  case Hir.TypeArray(elemType, size) => Hir.TypeArray(applySubstToType(elemType, subst), applySubstToType(size, subst))
  case Hir.TypeApplication(caller, args) => Hir
      .TypeApplication(applySubstToType(caller, subst), args.map(applySubstToType(_, subst)))
  case Hir.TypeScheme(params, body) =>
    val paramNames = params.collect { case Hir.TypeVar(name) => name }
    val filteredSubst = subst -- paramNames
    Hir.TypeScheme(params, applySubstToType(body, filteredSubst))
  case _ => ty

// Unification algorithm
def unify(t1: Hir, t2: Hir): Either[FlurryError, TypeSubst] = (t1, t2) match
  case (Hir.TypeVar(v1), Hir.TypeVar(v2)) if v1 == v2 => Right(Map.empty)

  case (Hir.TypeVar(v), ty) => if occurs(v, ty) then Left(OccursCheckError(v, ty)) else Right(Map(v -> ty))

  case (ty, Hir.TypeVar(v)) => if occurs(v, ty) then Left(OccursCheckError(v, ty)) else Right(Map(v -> ty))

  case (
        Hir.TypeFunction(isPure1, isComptime1, isDiamond1, params1, ret1),
        Hir.TypeFunction(isPure2, isComptime2, isDiamond2, params2, ret2)
      ) =>
    if params1.length != params2.length then Left(UnificationError(t1, t2))
    else
      unifyLists(params1, params2).flatMap { paramSubst =>
        val ret1Applied = applySubstToType(ret1, paramSubst)
        val ret2Applied = applySubstToType(ret2, paramSubst)
        unify(ret1Applied, ret2Applied).map(retSubst => paramSubst ++ retSubst)
      }

  case (Hir.Tuple(elems1, _), Hir.Tuple(elems2, _)) =>
    if elems1.length != elems2.length then Left(UnificationError(t1, t2)) else unifyLists(elems1, elems2)

  case (Hir.TypeArray(elem1, size1), Hir.TypeArray(elem2, size2)) => unify(elem1, elem2).flatMap { elemSubst =>
      val size1Applied = applySubstToType(size1, elemSubst)
      val size2Applied = applySubstToType(size2, elemSubst)
      unify(size1Applied, size2Applied).map(sizeSubst => elemSubst ++ sizeSubst)
    }

  case (Hir.TypeOption(inner1), Hir.TypeOption(inner2)) => unify(inner1, inner2)

  case (Hir.TypePointer(inner1), Hir.TypePointer(inner2)) => unify(inner1, inner2)

  case (Hir.TypeApplication(caller1, args1), Hir.TypeApplication(caller2, args2)) =>
    if args1.length != args2.length then Left(UnificationError(t1, t2))
    else
      unify(caller1, caller2).flatMap { callerSubst =>
        val args1Applied = args1.map(applySubstToType(_, callerSubst))
        val args2Applied = args2.map(applySubstToType(_, callerSubst))
        unifyLists(args1Applied, args2Applied).map(argSubst => callerSubst ++ argSubst)
      }

  case _ if t1 == t2 => Right(Map.empty)
  case _ => Left(UnificationError(t1, t2))

// Helper functions for unification
def occurs(variable: String, ty: Hir): Boolean = ty match
  case Hir.TypeVar(v) => v == variable
  case Hir.TypeFunction(_, _, _, params, ret) => params.exists(occurs(variable, _)) || occurs(variable, ret)
  case Hir.Tuple(elems, _) => elems.exists(occurs(variable, _))
  case Hir.TypePointer(inner) => occurs(variable, inner)
  case Hir.TypeOption(inner) => occurs(variable, inner)
  case Hir.TypeWithEffects(effect, ret) => occurs(variable, effect) || occurs(variable, ret)
  case Hir.TypeWithErrors(error, ret) => occurs(variable, error) || occurs(variable, ret)
  case Hir.TypeArray(elemType, size) => occurs(variable, elemType) || occurs(variable, size)
  case Hir.TypeApplication(caller, args) => occurs(variable, caller) || args.exists(occurs(variable, _))
  case Hir.TypeScheme(params, body) =>
    val paramNames = params.collect { case Hir.TypeVar(name) => name }
    !paramNames.contains(variable) && occurs(variable, body)
  case _ => false

def unifyLists(types1: List[Hir], types2: List[Hir]): Either[FlurryError, TypeSubst] = types1.zip(types2)
  .foldLeft(Right(Map.empty): Either[FlurryError, TypeSubst]) { (acc, pair) =>
    acc.flatMap { subst =>
      val (t1, t2) = pair
      val t1Applied = applySubstToType(t1, subst)
      val t2Applied = applySubstToType(t2, subst)
      unify(t1Applied, t2Applied).map(newSubst => subst ++ newSubst)
    }
  }

// Main contextual typing interface - refactored as object methods
object ContextualTyper:

  // Convert AST to Hir for type annotations
  def astToType(ast: Ast, engine: QueryEngine, scope: ScopeId): Either[FlurryError, Hir] = ast match
    case Ast.Id(name) => name match
        case "Int" => Right(Hir.TypeInteger)
        case "Real" => Right(Hir.TypeReal)
        case "String" => Right(Hir.TypeStr)
        case "Bool" => Right(Hir.TypeBool)
        case "Char" => Right(Hir.TypeChar)
        case "Unit" => Right(Hir.TypeVoid)
        case "Void" => Right(Hir.TypeNoReturn)
        case _ =>
          // Try to resolve as type variable from scope
          engine.execute[NameResolutionResult](ResolveNameQuery(name, scope)) match
            case Right(NameResolutionResult(symbol, _)) => Right(symbol.hir)
            case Left(_) => Right(Hir.TypeVar(name)) // Type variable

    case Ast.OptionalType(inner) => astToType(inner, engine, scope).map(Hir.TypeOption.apply)

    case Ast.PointerType(inner) => astToType(inner, engine, scope).map(Hir.TypePointer.apply)

    case Ast.ListOf(List(elemType)) => astToType(elemType, engine, scope)
        .map(elemTy => Hir.TypeApplication(Hir.TypeVar("List"), List(elemTy)))

    case Ast.Tuple(elements) => elements.traverse(astToType(_, engine, scope)).map(Hir.Tuple(_))

    case Ast.ForallType(params, body) =>
      val paramResults = params.traverse {
        case Ast.Id(name) => Right(Hir.TypeVar(name))
        case _ => Left(TypeError("Invalid type parameter in forall"))
      }

      for
        paramTypes <- paramResults
        bodyTy <- astToType(body, engine, scope)
      yield Hir.TypeScheme(paramTypes, bodyTy)

    case Ast.EffectQualifiedType(effect, returnType) =>
      for
        effTy <- astToType(effect, engine, scope)
        retTy <- astToType(returnType, engine, scope)
      yield Hir.TypeWithEffects(effTy, retTy)

    case Ast.ErrorQualifiedType(error, returnType) =>
      for
        errTy <- astToType(error, engine, scope)
        retTy <- astToType(returnType, engine, scope)
      yield Hir.TypeWithErrors(errTy, retTy)

    case _ => Left(TypeError(s"Cannot convert AST node to type: $ast"))

  // Bidirectional type inference with context - now using QueryEngine
  def inferType(
      ast: Ast,
      env: TypeEnv,
      context: TypingContext,
      engine: QueryEngine,
      scope: ScopeId,
      file: VfsNode
  ): Either[FlurryError, (Hir, Hir)] = ast match
    // Literals
    case Ast.Integer(value) => Right((Hir.TypeInteger, Hir.Integer(BigInt(value))))
    case Ast.Real(value) => Right((Hir.TypeReal, Hir.Real(BigDecimal(value))))
    case Ast.Str(value) => Right((Hir.TypeStr, Hir.Str(value)))
    case Ast.Bool(value) => Right((Hir.TypeBool, Hir.Bool(value)))
    case Ast.LitChar(value) => Right((Hir.TypeChar, Hir.CharVal(value)))
    case Ast.Unit => Right((Hir.TypeVoid, Hir.Invalid))
    case Ast.NullVal => Right((Hir.TypeVoid, Hir.Invalid))

    // Variables - use QueryEngine for name resolution
    case Ast.Id(name) =>
      // First check local environment
      env.lookup(name) match
        case Some(ty) => Right((ty, Hir.TypeVar(name)))
        case None =>
          // Fall back to scope-based resolution
          engine.execute[NameResolutionResult](ResolveNameQuery(name, scope)) match
            case Right(NameResolutionResult(symbol, _)) => Right((symbol.hir, Hir.TypeVar(name)))
            case Left(_) => Left(UndefinedVariableError(name, getSourceLocation(ast, file)))

    // Function calls with contextual argument typing
    case Ast.Call(func, args) => inferFunctionCall(func, args, env, context, engine, scope, file)

    // Lambda expressions
    case Ast.Lambda(params, returnType, body) =>
      inferLambda(params, returnType, body, env, context, engine, scope, file)

    // Let declarations
    case Ast.LetDecl(pattern, typ, init) =>
      for
        initType <- astToType(typ, engine, scope)
        (actualType, initHir) <- inferType(init, env, TypingContext.TypeExpectation(initType), engine, scope, file)
        _ <- checkUnify(actualType, initType)
        patternName <- extractPatternName(pattern)
        newEnv = env.extend(patternName, initType)
      yield (Hir.TypeVoid, Hir.Assign(Hir.TypeVar(patternName), initHir))

    // Tuples
    case Ast.Tuple(elements) =>
      for
        results <- elements.traverse(inferType(_, env, TypingContext.EmptyContext, engine, scope, file))
        (types, hirs) = results.unzip
      yield (Hir.Tuple(types), Hir.Tuple(hirs))

    // Lists
    case Ast.ListOf(elements) => elements match
        case Nil =>
          val (elemVar, _) = InferenceState().freshTypeVar()
          Right((Hir.TypeApplication(Hir.TypeVar("List"), List(elemVar)), Hir.Object(Nil, Map.empty)))
        case head :: tail =>
          for
            (headType, headHir) <- inferType(head, env, TypingContext.EmptyContext, engine, scope, file)
            results <- tail.traverse(inferType(_, env, TypingContext.TypeExpectation(headType), engine, scope, file))
            (_, tailHirs) = results.unzip
          yield (Hir.TypeApplication(Hir.TypeVar("List"), List(headType)), Hir.Object(headHir :: tailHirs, Map.empty))

    // Binary operations
    case Ast.Add(left, right) =>
      inferBinaryOp(left, right, Hir.TypeInteger, env, context, BinaryOp.Add, engine, scope, file)
    case Ast.Sub(left, right) =>
      inferBinaryOp(left, right, Hir.TypeInteger, env, context, BinaryOp.Sub, engine, scope, file)
    case Ast.Mul(left, right) =>
      inferBinaryOp(left, right, Hir.TypeInteger, env, context, BinaryOp.Mul, engine, scope, file)
    case Ast.Div(left, right) =>
      inferBinaryOp(left, right, Hir.TypeInteger, env, context, BinaryOp.Div, engine, scope, file)
    case Ast.Mod(left, right) =>
      inferBinaryOp(left, right, Hir.TypeInteger, env, context, BinaryOp.Mod, engine, scope, file)

    case Ast.BoolEq(left, right) => inferComparisonOp(left, right, env, context, BinaryOp.Eq, engine, scope, file)
    case Ast.BoolNotEq(left, right) => inferComparisonOp(left, right, env, context, BinaryOp.Neq, engine, scope, file)
    case Ast.BoolGt(left, right) => inferComparisonOp(left, right, env, context, BinaryOp.Gt, engine, scope, file)
    case Ast.BoolGtEq(left, right) => inferComparisonOp(left, right, env, context, BinaryOp.Ge, engine, scope, file)
    case Ast.BoolLt(left, right) => inferComparisonOp(left, right, env, context, BinaryOp.Lt, engine, scope, file)
    case Ast.BoolLtEq(left, right) => inferComparisonOp(left, right, env, context, BinaryOp.Le, engine, scope, file)

    case Ast.BoolAnd(left, right) =>
      inferBinaryOp(left, right, Hir.TypeBool, env, context, BinaryOp.And, engine, scope, file)
    case Ast.BoolOr(left, right) =>
      inferBinaryOp(left, right, Hir.TypeBool, env, context, BinaryOp.Or, engine, scope, file)
    case Ast.BoolNot(expr) => inferUnaryOp(expr, Hir.TypeBool, env, context, UnaryOp.Not, engine, scope, file)

    // If expressions
    case Ast.IfStatement(cond, thenBranch, elseBranch) =>
      for
        (condType, condHir) <- inferType(cond, env, TypingContext.TypeExpectation(Hir.TypeBool), engine, scope, file)
        _ <- checkUnify(condType, Hir.TypeBool)
        (thenType, thenHir) <- inferType(thenBranch, env, context, engine, scope, file)
        (elseType, elseHir) <- inferType(elseBranch, env, TypingContext.TypeExpectation(thenType), engine, scope, file)
        _ <- checkUnify(thenType, elseType)
      yield (thenType, Hir.If(condHir, thenHir, elseHir))

    // Type annotations - use checking mode
    case Ast.TypeWith(expr, typeAst) =>
      for
        expectedType <- astToType(typeAst, engine, scope)
        (inferredType, hir) <- inferType(expr, env, TypingContext.TypeExpectation(expectedType), engine, scope, file)
        _ <- checkUnify(inferredType, expectedType)
      yield (expectedType, hir)

    case _ => Left(TypeError(s"Type inference not implemented for AST node: $ast", getSourceLocation(ast, file)))

  private def inferFunctionCall(
      func: Ast,
      args: List[Ast],
      env: TypeEnv,
      context: TypingContext,
      engine: QueryEngine,
      scope: ScopeId,
      file: VfsNode
  ): Either[FlurryError, (Hir, Hir)] =
    for
      (funcType, funcHir) <- inferType(func, env, TypingContext.EmptyContext, engine, scope, file)
      result <- funcType match
        case Hir.TypeFunction(_, _, _, paramTypes, returnType) =>
          if args.length != paramTypes.length then
            Left(TypeError(s"Function expects ${paramTypes.length} arguments, got ${args.length}"))
          else
            // Use argument teleportation for contextual typing
            val argContexts = paramTypes.map(TypingContext.TypeExpectation.apply)
            for
              argResults <- args.zip(argContexts).traverse((arg, ctx) => inferType(arg, env, ctx, engine, scope, file))
              (argTypes, argHirs) = argResults.unzip
              _ <- argTypes.zip(paramTypes).traverse((actual, expected) => checkUnify(actual, expected))
            yield (returnType, Hir.Block(funcHir :: argHirs))
        case _ => Left(TypeError(s"Cannot call non-function type: $funcType"))
    yield result

  private def inferLambda(
      params: List[Ast],
      returnTypeAst: Ast,
      body: Ast,
      env: TypeEnv,
      context: TypingContext,
      engine: QueryEngine,
      scope: ScopeId,
      file: VfsNode
  ): Either[FlurryError, (Hir, Hir)] =
    // Extract parameter names and types
    val paramResults = params.map {
      case Ast.Id(name) =>
        val (paramVar, _) = InferenceState().freshTypeVar()
        Right((name, paramVar))
      case Ast.TypeWith(Ast.Id(name), typeAst) => astToType(typeAst, engine, scope).map((name, _))
      case _ => Left(TypeError("Invalid lambda parameter"))
    }

    for
      paramInfo <- paramResults.traverse(identity)
      (paramNames, paramTypes) = paramInfo.unzip
      returnType <- astToType(returnTypeAst, engine, scope)
      paramEnv = paramInfo.foldLeft(env)((acc, pair) => acc.extend(pair._1, pair._2))
      (bodyType, bodyHir) <- inferType(body, paramEnv, TypingContext.TypeExpectation(returnType), engine, scope, file)
      _ <- checkUnify(bodyType, returnType)
      // 创建Hir参数列表
      hirParams = paramInfo.map { case (name, paramType) => Hir.Param(name, paramType, Hir.Invalid) }
    yield (Hir.TypeFunction(true, false, false, paramTypes, returnType), Hir.Block(List(bodyHir)))

  private def inferBinaryOp(
      left: Ast,
      right: Ast,
      expectedType: Hir,
      env: TypeEnv,
      context: TypingContext,
      op: BinaryOp,
      engine: QueryEngine,
      scope: ScopeId,
      file: VfsNode
  ): Either[FlurryError, (Hir, Hir)] =
    for
      (leftType, leftHir) <- inferType(left, env, TypingContext.TypeExpectation(expectedType), engine, scope, file)
      (rightType, rightHir) <- inferType(right, env, TypingContext.TypeExpectation(expectedType), engine, scope, file)
      _ <- checkUnify(leftType, expectedType)
      _ <- checkUnify(rightType, expectedType)
    yield (expectedType, Hir.BinaryApplication(op, leftHir, rightHir))

  private def inferComparisonOp(
      left: Ast,
      right: Ast,
      env: TypeEnv,
      context: TypingContext,
      op: BinaryOp,
      engine: QueryEngine,
      scope: ScopeId,
      file: VfsNode
  ): Either[FlurryError, (Hir, Hir)] =
    for
      (leftType, leftHir) <- inferType(left, env, TypingContext.EmptyContext, engine, scope, file)
      (rightType, rightHir) <- inferType(right, env, TypingContext.TypeExpectation(leftType), engine, scope, file)
      _ <- checkUnify(leftType, rightType)
    yield (Hir.TypeBool, Hir.BinaryApplication(op, leftHir, rightHir))

  private def inferUnaryOp(
      expr: Ast,
      expectedType: Hir,
      env: TypeEnv,
      context: TypingContext,
      op: UnaryOp,
      engine: QueryEngine,
      scope: ScopeId,
      file: VfsNode
  ): Either[FlurryError, (Hir, Hir)] =
    for
      (exprType, exprHir) <- inferType(expr, env, TypingContext.TypeExpectation(expectedType), engine, scope, file)
      _ <- checkUnify(exprType, expectedType)
    yield (expectedType, Hir.UnaryApplication(op, exprHir))

  private def checkUnify(actual: Hir, expected: Hir): Either[FlurryError, Unit] = unify(actual, expected).map(_ => ())

  private def extractPatternName(pattern: Ast): Either[FlurryError, String] = pattern match
    case Ast.Id(name) => Right(name)
    case _ => Left(TypeError("Complex patterns not yet supported"))

  private def getSourceLocation(ast: Ast, file: VfsNode): Option[SourceLocation] =
    // Convert AST span to SourceLocation
    Some(SourceLocation(file.name, ast.span.start, ast.span.start)) // Simplified

// Extension method for traverse
extension [A, B](list: List[A])
  def traverse[E](f: A => Either[E, B]): Either[E, List[B]] = list
    .foldRight(Right(Nil): Either[E, List[B]])((a, acc) => acc.flatMap(bs => f(a).map(_ :: bs)))

// 公共接口，让其他模块可以使用类型推断系统 - 保持已重构的接口
object ContextualTyping:
  def inferType(ast: AstLocation, engine: QueryEngine, scope: ScopeId): Either[FlurryError, (Hir, Hir)] =
    ContextualTyper.inferType(ast.node, TypeEnv(), TypingContext.EmptyContext, engine, scope, ast.file)

  def inferTypeWithEnv(
      ast: AstLocation,
      env: TypeEnv,
      engine: QueryEngine,
      scope: ScopeId
  ): Either[FlurryError, (Hir, Hir)] = ContextualTyper
    .inferType(ast.node, env, TypingContext.EmptyContext, engine, scope, ast.file)

  def checkType(
      ast: AstLocation,
      expectedType: Hir,
      engine: QueryEngine,
      scope: ScopeId
  ): Either[FlurryError, (Hir, Hir)] = ContextualTyper
    .inferType(ast.node, TypeEnv(), TypingContext.TypeExpectation(expectedType), engine, scope, ast.file).map(_._2)
    .map((expectedType, _))

  def checkTypeWithEnv(
      ast: AstLocation,
      expectedType: Hir,
      env: TypeEnv,
      engine: QueryEngine,
      scope: ScopeId
  ): Either[FlurryError, (Hir, Hir)] = ContextualTyper
    .inferType(ast.node, env, TypingContext.TypeExpectation(expectedType), engine, scope, ast.file).map(_._2)
    .map((expectedType, _))
