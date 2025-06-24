package analysis1

import parse.Ast
import errors.*
import vfs.VfsNode
import scala.collection.mutable
import QueryTypes.*

// Type Environment for contextual typing - now used for HIR type checking
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

def checkUnify(actual: Hir, expected: Hir): Either[FlurryError, Unit] = unify(actual, expected).map(_ => ())

// Main contextual typing interface - now works on HIR instead of AST
object ContextualTyper:

  // Type check HIR expressions - the main entry point
  def checkType(
      hir: Hir,
      env: TypeEnv,
      context: TypingContext,
      engine: QueryEngine,
      scope: ScopeId
  ): Either[FlurryError, Hir] = hir match

    // Literals - already typed correctly
    case i @ Hir.Integer(_) => Right(i)
    case r @ Hir.Real(_) => Right(r)
    case s @ Hir.Str(_) => Right(s)
    case b @ Hir.Bool(_) => Right(b)
    case c @ Hir.CharVal(_) => Right(c)
    case sym @ Hir.Symbol(_) => Right(sym)
    case Hir.NullVal => Right(Hir.NullVal)
    case Hir.UndefinedVal => Right(Hir.UndefinedVal)

    // Type constructors
    case ty @ (Hir.TypeInteger | Hir.TypeReal | Hir.TypeStr | Hir.TypeBool | Hir.TypeChar | Hir.TypeSymbol | Hir
          .TypeVoid | Hir.TypeNoReturn | Hir.TypeAny | Hir.TypeObject | Hir.TypeType) => Right(ty)

    // Variables and type variables
    case Hir.TypeVar(name) => env.lookup(name) match
        case Some(ty) => Right(ty)
        case None =>
          // Try to resolve from scope
          engine.execute[NameResolutionResult](ResolveNameQuery(name, scope)) match
            case Right(NameResolutionResult(symbol, _)) => Right(symbol.hir)
            case Left(_) => Right(hir) // Keep as type variable if not found

    // Function definitions
    case func @ Hir.FunctionDef(name, params, returnType, clauses, body, ty) =>
      for
        // Check parameter types
        checkedParams <- params.traverse { case param @ Hir.Param(paramName, paramType, default) =>
          for
            checkedParamType <- checkType(paramType, env, TypingContext.EmptyContext, engine, scope)
            checkedDefault <- checkType(default, env, TypingContext.EmptyContext, engine, scope)
          yield Hir.Param(paramName, checkedParamType, checkedDefault)
        }
        // Check return type
        checkedReturnType <- checkType(returnType, env, TypingContext.EmptyContext, engine, scope)
        // Create environment with parameters - use the original params for type environment
        paramEnv = params.foldLeft(env) { (acc, param) =>
          param match
            case Hir.Param(paramName, paramType, _) => acc.extend(paramName, paramType)
        }
        // Check body with expected return type
        checkedBody <- checkType(body, paramEnv, TypingContext.TypeExpectation(checkedReturnType), engine, scope)
        // Check clauses if any
        checkedClauses <- clauses
          .traverse(clause => checkType(clause, paramEnv, TypingContext.EmptyContext, engine, scope))
      yield func.copy(
        params = checkedParams.collect { case p: Hir.Param => p },
        returnType = checkedReturnType,
        clauses = checkedClauses,
        body = checkedBody
      )

    // Function applications
    case app @ Hir.Application(callee, args) =>
      for
        checkedCallee <- checkType(callee, env, TypingContext.EmptyContext, engine, scope)
        calleeType <- inferType(checkedCallee, env, engine, scope)
        result <- calleeType match
          case Hir.TypeFunction(_, _, _, paramTypes, returnType) =>
            // Extract args from Object
            val argList = args.children

            if argList.length != paramTypes.length then
              Left(TypeError(s"Function expects ${paramTypes.length} arguments, got ${argList.length}"))
            else
              for
                checkedArgs <- argList.zip(paramTypes).traverse((arg, expectedType) =>
                  checkType(arg, env, TypingContext.TypeExpectation(expectedType), engine, scope)
                )
                checkedArgsObj: Hir.Object = Hir.Object(checkedArgs, args.properties)
              yield app.copy(callee = checkedCallee, args = checkedArgsObj)
          case _ => Left(TypeError(s"Cannot call non-function type: $calleeType"))
      yield result

    // Binary operations
    case binOp @ Hir.BinaryApplication(op, lhs, rhs) =>
      val expectedType = inferBinaryOpType(op)
      for
        checkedLeft <- checkType(lhs, env, TypingContext.TypeExpectation(expectedType), engine, scope)
        checkedRight <- checkType(rhs, env, TypingContext.TypeExpectation(expectedType), engine, scope)
        leftType <- inferType(checkedLeft, env, engine, scope)
        rightType <- inferType(checkedRight, env, engine, scope)
        _ <- checkUnify(leftType, expectedType)
        _ <- checkUnify(rightType, expectedType)
      yield binOp.copy(lhs = checkedLeft, rhs = checkedRight)

    // Unary operations
    case unOp @ Hir.UnaryApplication(op, operand) =>
      val expectedType = inferUnaryOpType(op)
      for
        checkedOperand <- checkType(operand, env, TypingContext.TypeExpectation(expectedType), engine, scope)
        operandType <- inferType(checkedOperand, env, engine, scope)
        _ <- checkUnify(operandType, expectedType)
      yield unOp.copy(operand = checkedOperand)

    // Tuples
    case tuple @ Hir.Tuple(elements, metadata) =>
      for checkedElements <- elements.traverse(elem => checkType(elem, env, TypingContext.EmptyContext, engine, scope))
      yield tuple.copy(elements = checkedElements)

    // Blocks
    case block @ Hir.Block(statements) =>
      // Type check each statement in sequence, threading environment
      statements.foldLeft(Right((env, List.empty[Hir])): Either[FlurryError, (TypeEnv, List[Hir])]) {
        case (Right((currentEnv, checkedStmts)), stmt) =>
          checkType(stmt, currentEnv, TypingContext.EmptyContext, engine, scope) match
            case Right(checkedStmt) =>
              // Update environment if this is a binding
              val newEnv = stmt match
                case Hir.Assign(Hir.TypeVar(name), _) => inferType(checkedStmt, currentEnv, engine, scope) match
                    case Right(stmtType) => currentEnv.extend(name, stmtType)
                    case Left(_) => currentEnv
                case _ => currentEnv
              Right((newEnv, checkedStmts :+ checkedStmt))
            case Left(error) => Left(error)
        case (Left(error), _) => Left(error)
      }.map { case (_, checkedStmts) => block.copy(statements = checkedStmts) }

    // If expressions
    case ifExpr @ Hir.If(condition, thenBranch, elseBranch) =>
      for
        checkedCondition <- checkType(condition, env, TypingContext.TypeExpectation(Hir.TypeBool), engine, scope)
        condType <- inferType(checkedCondition, env, engine, scope)
        _ <- checkUnify(condType, Hir.TypeBool)
        checkedThenBranch <- checkType(thenBranch, env, context, engine, scope)
        thenType <- inferType(checkedThenBranch, env, engine, scope)
        checkedElseBranch <- checkType(elseBranch, env, TypingContext.TypeExpectation(thenType), engine, scope)
        elseType <- inferType(checkedElseBranch, env, engine, scope)
        _ <- checkUnify(thenType, elseType)
      yield ifExpr.copy(condition = checkedCondition, thenBranch = checkedThenBranch, elseBranch = checkedElseBranch)

    // Assignments
    case assign @ Hir.Assign(target, value) => context match
        case TypingContext.TypeExpectation(expectedType) =>
          for
            checkedValue <- checkType(value, env, TypingContext.TypeExpectation(expectedType), engine, scope)
            valueType <- inferType(checkedValue, env, engine, scope)
            _ <- checkUnify(valueType, expectedType)
          yield assign.copy(rhs = checkedValue)
        case _ =>
          for checkedValue <- checkType(value, env, TypingContext.EmptyContext, engine, scope)
          yield assign.copy(rhs = checkedValue)

    // Type constructors with parameters
    case Hir.TypeFunction(isPure, isComptime, isDiamond, params, returnType) =>
      for
        checkedParams <- params.traverse(param => checkType(param, env, TypingContext.EmptyContext, engine, scope))
        checkedReturnType <- checkType(returnType, env, TypingContext.EmptyContext, engine, scope)
      yield Hir.TypeFunction(isPure, isComptime, isDiamond, checkedParams, checkedReturnType)

    case Hir.TypePointer(inner) => checkType(inner, env, TypingContext.EmptyContext, engine, scope)
        .map(Hir.TypePointer.apply)

    case Hir.TypeOption(inner) => checkType(inner, env, TypingContext.EmptyContext, engine, scope)
        .map(Hir.TypeOption.apply)

    case Hir.TypeArray(elemType, size) =>
      for
        checkedElemType <- checkType(elemType, env, TypingContext.EmptyContext, engine, scope)
        checkedSize <- checkType(size, env, TypingContext.TypeExpectation(Hir.TypeInteger), engine, scope)
      yield Hir.TypeArray(checkedElemType, checkedSize)

    case Hir.TypeApplication(caller, args) =>
      for
        checkedCaller <- checkType(caller, env, TypingContext.EmptyContext, engine, scope)
        checkedArgs <- args.traverse(arg => checkType(arg, env, TypingContext.EmptyContext, engine, scope))
      yield Hir.TypeApplication(checkedCaller, checkedArgs)

    case Hir.TypeScheme(params, body) =>
      // Add type parameters to environment
      val paramEnv = params.foldLeft(env) { (acc, param) =>
        param match
          case Hir.TypeVar(name) => acc.extend(name, param)
          case _ => acc
      }
      for checkedBody <- checkType(body, paramEnv, TypingContext.EmptyContext, engine, scope)
      yield Hir.TypeScheme(params, checkedBody)

    // Default case - pass through if already well-typed
    case other => Right(other)

  // Infer the type of a HIR expression
  def inferType(hir: Hir, env: TypeEnv, engine: QueryEngine, scope: ScopeId): Either[FlurryError, Hir] = hir match
    case Hir.Integer(_) => Right(Hir.TypeInteger)
    case Hir.Real(_) => Right(Hir.TypeReal)
    case Hir.Str(_) => Right(Hir.TypeStr)
    case Hir.Bool(_) => Right(Hir.TypeBool)
    case Hir.CharVal(_) => Right(Hir.TypeChar)
    case Hir.Symbol(_) => Right(Hir.TypeSymbol)
    case Hir.NullVal => Right(Hir.TypeVoid) // or a special null type
    case Hir.UndefinedVal => Right(Hir.TypeVoid)

    case Hir.TypeVar(name) => env.lookup(name) match
        case Some(ty) => Right(ty)
        case None => Right(Hir.TypeType) // Type variables have type Type

    case ty @ (Hir.TypeInteger | Hir.TypeReal | Hir.TypeStr | Hir.TypeBool | Hir.TypeChar | Hir.TypeSymbol | Hir
          .TypeVoid | Hir.TypeNoReturn | Hir.TypeAny | Hir.TypeObject | Hir.TypeType) => Right(Hir.TypeType)

    case Hir.TypeFunction(_, _, _, params, returnType) => Right(Hir.TypeType)
    case Hir.TypePointer(_) => Right(Hir.TypeType)
    case Hir.TypeOption(_) => Right(Hir.TypeType)
    case Hir.TypeArray(_, _) => Right(Hir.TypeType)
    case Hir.TypeApplication(_, _) => Right(Hir.TypeType)
    case Hir.TypeScheme(_, _) => Right(Hir.TypeType)

    case Hir.FunctionDef(_, _, returnType, _, _, _) => Right(returnType)

    case Hir.Application(callee, args) => inferType(callee, env, engine, scope).flatMap {
        case Hir.TypeFunction(_, _, _, _, returnType) => Right(returnType)
        case other => Left(TypeError(s"Cannot call non-function type: $other"))
      }

    case Hir.BinaryApplication(op, left, right) => op match
        case BinaryOp.Eq | BinaryOp.Neq | BinaryOp.Lt | BinaryOp.Le | BinaryOp.Gt | BinaryOp.Ge => Right(Hir.TypeBool)
        case BinaryOp.And | BinaryOp.Or => Right(Hir.TypeBool)
        case BinaryOp.Add | BinaryOp.Sub | BinaryOp.Mul | BinaryOp.Div | BinaryOp.Mod =>
          // For now, assume integer arithmetic
          Right(Hir.TypeInteger)
        case BinaryOp.Implies => Right(Hir.TypeBool)
        case BinaryOp.TypeWith | BinaryOp.TraitBound => Right(Hir.TypeType)
        case BinaryOp.AddAdd => Right(Hir.TypeInteger) // Or string depending on operands

    case Hir.UnaryApplication(op, operand) => op match
        case UnaryOp.Not => Right(Hir.TypeBool)
        case UnaryOp.Neg => inferType(operand, env, engine, scope)
        case UnaryOp.Deref =>
          // Infer the type that the pointer points to
          inferType(operand, env, engine, scope).flatMap {
            case Hir.TypePointer(pointeeType) => Right(pointeeType)
            case other => Left(TypeError(s"Cannot dereference non-pointer type: $other"))
          }
        case UnaryOp.Refer =>
          // Create a pointer type
          inferType(operand, env, engine, scope).map(Hir.TypePointer(_))

    case Hir.Tuple(elements, _) =>
      for elementTypes <- elements.traverse(elem => inferType(elem, env, engine, scope)) yield Hir.Tuple(elementTypes)

    case Hir.Block(statements) => statements.lastOption match
        case Some(lastStmt) => inferType(lastStmt, env, engine, scope)
        case None => Right(Hir.TypeVoid)

    case Hir.If(_, thenBranch, _) => inferType(thenBranch, env, engine, scope)

    case Hir.Assign(_, value) => inferType(value, env, engine, scope)

    case other => Left(TypeError(s"Cannot infer type for HIR node: ${other.getClass.getSimpleName}"))

  // Helper methods
  private def inferBinaryOpType(op: BinaryOp): Hir = op match
    case BinaryOp.Add | BinaryOp.Sub | BinaryOp.Mul | BinaryOp.Div | BinaryOp.Mod => Hir.TypeInteger
    case BinaryOp.And | BinaryOp.Or => Hir.TypeBool
    case BinaryOp.Eq | BinaryOp.Neq | BinaryOp.Lt | BinaryOp.Le | BinaryOp.Gt | BinaryOp.Ge => Hir.TypeBool
    case BinaryOp.Implies => Hir.TypeBool
    case BinaryOp.TypeWith => Hir.TypeType
    case BinaryOp.TraitBound => Hir.TypeType
    case BinaryOp.AddAdd => Hir.TypeInteger // string concatenation or addition

  private def inferUnaryOpType(op: UnaryOp): Hir = op match
    case UnaryOp.Not => Hir.TypeBool
    case UnaryOp.Neg => Hir.TypeInteger
    case UnaryOp.Deref => Hir.TypeAny // type depends on what's being dereferenced
    case UnaryOp.Refer => Hir.TypePointer(Hir.TypeAny) // creates a pointer

// Extension method for traverse
extension [A, B](list: List[A])
  def traverse[E](f: A => Either[E, B]): Either[E, List[B]] = list
    .foldRight(Right(Nil): Either[E, List[B]])((a, acc) => acc.flatMap(bs => f(a).map(_ :: bs)))

// 公共接口，让其他模块可以使用类型推断系统 - 现在在HIR上工作
object ContextualTyping:
  // 在HIR上进行类型检查的主要接口
  def checkTypeOnHir(hir: Hir, engine: QueryEngine, scope: ScopeId): Either[FlurryError, Hir] = ContextualTyper
    .checkType(hir, TypeEnv(), TypingContext.EmptyContext, engine, scope)

  def checkTypeOnHirWithEnv(hir: Hir, env: TypeEnv, engine: QueryEngine, scope: ScopeId): Either[FlurryError, Hir] =
    ContextualTyper.checkType(hir, env, TypingContext.EmptyContext, engine, scope)

  def checkTypeOnHirWithExpectation(
      hir: Hir,
      expectedType: Hir,
      engine: QueryEngine,
      scope: ScopeId
  ): Either[FlurryError, Hir] = ContextualTyper
    .checkType(hir, TypeEnv(), TypingContext.TypeExpectation(expectedType), engine, scope)

  def inferTypeOnHir(hir: Hir, engine: QueryEngine, scope: ScopeId): Either[FlurryError, Hir] = ContextualTyper
    .inferType(hir, TypeEnv(), engine, scope)
