// Types
enum Ty:
  case TyConst(name: String)                       // Int, Bool, String
  case TyVar(name: String)                         // a, b (for generics)
  case TyFun(param: Ty, result: Ty)                // T1 -> T2
  case TyRecord(fields: Map[String, Ty])           // { f1: T1, f2: T2 }
  case TyScheme(vars: List[String], body: Ty)      // forall a b. a -> b -> a (for let-polymorphism)

  override def toString: String = this match
    case TyConst(n) => n
    case TyVar(n) => n
    case TyFun(p, r) => s"(${p.toString} -> ${r.toString})"
    case TyRecord(fs) => fs.map((k, v) => s"$k: ${v.toString}").mkString("{ ", ", ", " }")
    case TyScheme(Nil, b) => b.toString // No quantification if no vars
    case TyScheme(vs, b) => s"forall ${vs.mkString(" ")}. ${b.toString}"

// Expressions
enum Expr:
  case LitInt(value: Int)
  case LitBool(value: Boolean)
  case LitString(value: String)
  case Var(name: String)
  case Lambda(paramName: String, body: Expr)
  case App(func: Expr, arg: Expr)
  case Let(varName: String, value: Expr, body: Expr)
  case If(cond: Expr, thenBranch: Expr, elseBranch: Expr)
  case BinaryOp(op: String, left: Expr, right: Expr)
  case RecordCons(fields: Map[String, Expr])
  case RecordProj(record: Expr, fieldName: String)
  case Annotated(expr: Expr, ty: Ty) // e :: Ty

import scala.util.{Try, Success, Failure}

// Type Environment: variable name -> type scheme
type TyEnv = Map[String, Ty.TyScheme]

// Surrounding Context (Sigma from the paper)
enum Context:
  case EmptyCtx                                  // □
  case TypeExpectation(expected: Ty)             // A (e.g. from annotation or function return)
  case ArgTeleported(argTy: Ty, outerCtx: Context) // Type of arg e2 :: Σ' (for e1 in e1 e2)
  case FieldTeleported(fieldName: String, outerCtx: Context) // label a :: Σ' (for record in record.a)

// Substitution for type variables
type Subst = Map[String, Ty]

object TyperError:
  case class UnificationFail(t1: Ty, t2: Ty, msg: String = "") extends Exception(s"Cannot unify ${t1.toString} and ${t2.toString}. $msg")
  case class OccursCheckFail(v: String, t: Ty) extends Exception(s"Occurs check failed: $v in ${t.toString}")
  case class UndefinedVariable(name: String) extends Exception(s"Undefined variable: $name")
  case class NotAFunction(ty: Ty) extends Exception(s"Expected a function type, but got ${ty.toString}")
  case class NotARecord(ty: Ty) extends Exception(s"Expected a record type, but got ${ty.toString}")
  case class FieldNotFound(fieldName: String, ty: Ty) extends Exception(s"Field '$fieldName' not found in record ${ty.toString}")
  case class TypeMismatch(expected: Ty, actual: Ty, expr: Expr) extends Exception(s"Type mismatch for ${expr.toString}: expected ${expected.toString}, got ${actual.toString}")
  case class ContextError(msg: String, ctx: Context, expr: Expr) extends Exception(s"Context error: $msg for ${expr.toString} in context ${ctx.toString}")
  case class GeneralTyperError(msg: String) extends Exception(msg)

// Utility for fresh type variables
var tyVarCounter = 0
def freshTyVarName(): String =
  tyVarCounter += 1
  s"t${tyVarCounter}"

def freshTyVar(): Ty.TyVar = Ty.TyVar(freshTyVarName())

object Unifier:
  def applySubst(subst: Subst, ty: Ty): Ty = ty match
    case Ty.TyVar(name) => subst.getOrElse(name, ty)
    case Ty.TyFun(p, r) => Ty.TyFun(applySubst(subst, p), applySubst(subst, r))
    case Ty.TyRecord(fs) => Ty.TyRecord(fs.view.mapValues(t => applySubst(subst, t)).toMap)
    case Ty.TyScheme(vars, body) =>
      // Substitute only for free variables in the scheme's body
      val filteredSubst = subst.view.filterKeys(k => !vars.contains(k)).toMap
      Ty.TyScheme(vars, applySubst(filteredSubst, body))
    case t: Ty.TyConst => t

  def applySubstToEnv(subst: Subst, env: TyEnv): TyEnv =
    env.view.mapValues(scheme => applySubst(subst, scheme).asInstanceOf[Ty.TyScheme]).toMap // Safe cast if scheme logic is correct

  def composeSubst(s2: Subst, s1: Subst): Subst =
    s1.view.mapValues(t => applySubst(s2, t)).toMap ++ s2

  def freeTyVars(ty: Ty): Set[String] = ty match
    case Ty.TyConst(_) => Set.empty
    case Ty.TyVar(name) => Set(name)
    case Ty.TyFun(p, r) => freeTyVars(p) ++ freeTyVars(r)
    case Ty.TyRecord(fs) => fs.values.flatMap(freeTyVars).toSet
    case Ty.TyScheme(vars, body) => freeTyVars(body) -- vars.toSet

  def freeTyVarsInEnv(env: TyEnv): Set[String] =
    env.values.flatMap(freeTyVars).toSet

  def unify(t1_orig: Ty, t2_orig: Ty): Try[Subst] =
    var t1 = t1_orig
    var t2 = t2_orig
    // println(s"Unifying: ${t1.toString} and ${t2.toString}") // Debug
    (t1, t2) match
      case (Ty.TyVar(v1), Ty.TyVar(v2)) if v1 == v2 => Success(Map.empty)
      case (Ty.TyVar(v), t) => bindTyVar(v, t)
      case (t, Ty.TyVar(v)) => bindTyVar(v, t)
      case (Ty.TyConst(n1), Ty.TyConst(n2)) if n1 == n2 => Success(Map.empty)
      case (Ty.TyFun(p1, r1), Ty.TyFun(p2, r2)) =>
        for {
          s1 <- unify(p1, p2)
          s2 <- unify(applySubst(s1, r1), applySubst(s1, r2))
        } yield composeSubst(s2, s1)
      case (Ty.TyRecord(f1), Ty.TyRecord(f2)) =>
        if f1.keySet != f2.keySet then
          Failure(TyperError.UnificationFail(t1, t2, s"Record fields mismatch: ${f1.keySet} vs ${f2.keySet}"))
        else
          f1.keys.foldLeft(Try(Map.empty[String, Ty])) { (accSubstTry, key) =>
            accSubstTry.flatMap { accSubst =>
              val t1Field = applySubst(accSubst, f1(key))
              val t2Field = applySubst(accSubst, f2(key))
              unify(t1Field, t2Field).map(fieldSubst => composeSubst(fieldSubst, accSubst))
            }
          }
      case (s: Ty.TyScheme, _) => Failure(TyperError.UnificationFail(t1, t2, "Cannot unify with a TyScheme directly, instantiate first."))
      case (_, s: Ty.TyScheme) => Failure(TyperError.UnificationFail(t1, t2, "Cannot unify with a TyScheme directly, instantiate first."))
      case _ => Failure(TyperError.UnificationFail(t1, t2))

  private def bindTyVar(v: String, t: Ty): Try[Subst] =
    if t == Ty.TyVar(v) then Success(Map.empty)
    else if freeTyVars(t).contains(v) then Failure(TyperError.OccursCheckFail(v, t))
    else Success(Map(v -> t))
end Unifier

object Polymorphism:
  import Unifier.*

  def generalize(env: TyEnv, ty: Ty): Ty.TyScheme =
    val freeInEnv = freeTyVarsInEnv(env)
    val freeInTy = freeTyVars(ty)
    Ty.TyScheme((freeInTy -- freeInEnv).toList.sorted, ty) // sorted for consistent schemes

  def instantiate(scheme: Ty.TyScheme): Ty = scheme match
    case Ty.TyScheme(vars, body) =>
      val subst = vars.map(v => v -> freshTyVar()).toMap
      applySubst(subst, body)
end Polymorphism

object ContextualTyper:
  import Unifier.*
  import Polymorphism.*

  // Γ | Σ ⊢ e ⇒ (A, Subst)
  def typeCheckAlg(env: TyEnv, context: Context, expr: Expr): Try[(Ty, Subst)] =
    // Uncomment for debugging S combinator:
    // if (expr.toString.contains("App") && context != Context.EmptyCtx) 
    //   println(s"TC: ctx=$context, expr=$expr")
    expr match
      // ALit, AVar (simplified: these always infer in EmptyCtx then match context)
      case Expr.LitInt(v) => matchGenericConsumer(env, Ty.TyConst("Int"), Map.empty, context, expr)
      case Expr.LitBool(v) => matchGenericConsumer(env, Ty.TyConst("Bool"), Map.empty, context, expr)
      case Expr.LitString(v) => matchGenericConsumer(env, Ty.TyConst("String"), Map.empty, context, expr)

      case Expr.Var(name) =>
        env.get(name) match
          case Some(scheme) =>
            val ty = instantiate(scheme)
            matchGenericConsumer(env, ty, Map.empty, context, expr)
          case None => Failure(TyperError.UndefinedVariable(name))

      // AAnn (Annotation rule)
      case Expr.Annotated(e, tyAnnotation) =>
        for {
          (actualTy, s1) <- typeCheckAlg(env, Context.TypeExpectation(tyAnnotation), e)
          s2 <- unify(applySubst(s1, actualTy), applySubst(s1, tyAnnotation))
        } yield (applySubst(s2, actualTy), composeSubst(s2, s1)) // Return the annotated type

      // ALam1 / ALam2 (Lambda rules)
      case Expr.Lambda(paramName, body) => context match
        case Context.TypeExpectation(Ty.TyFun(expectedParamTy, expectedRetTy)) => // ALam1
          val newEnv: TyEnv = env + (paramName -> Ty.TyScheme(Nil, expectedParamTy))
          for {
            (actualRetTy, s1) <- typeCheckAlg(newEnv, Context.TypeExpectation(expectedRetTy), body)
            // Resulting function type
            resultFunTy = Ty.TyFun(applySubst(s1, expectedParamTy), actualRetTy)
          } yield (resultFunTy, s1)

        case Context.ArgTeleported(argTy, outerCtx) => // ALam2 (argTy is type of e2 in (λx.e) e2)
          val paramTy = argTy
          val newEnv: TyEnv = env + (paramName -> Ty.TyScheme(Nil, paramTy))
          for {
            (bodyTy, s1) <- typeCheckAlg(newEnv, outerCtx, body)
          } yield (bodyTy, s1) // Return body type directly - this is the result of the application

        case Context.EmptyCtx => // Infer lambda: needs param type annotation or later application
          val paramTy = freshTyVar()
          val newEnv: TyEnv = env + (paramName -> Ty.TyScheme(Nil, paramTy))
          for {
            (bodyTy, s1) <- typeCheckAlg(newEnv, Context.EmptyCtx, body) 
            finalSubst = s1
            finalParamTy = applySubst(finalSubst, paramTy)
            finalBodyTy = applySubst(finalSubst, bodyTy)
          } yield (Ty.TyFun(finalParamTy, finalBodyTy), finalSubst)

        case Context.TypeExpectation(expectedTy) if !expectedTy.isInstanceOf[Ty.TyFun] =>
          // If we expect a non-function type, this is an error
          Failure(TyperError.TypeMismatch(expectedTy, Ty.TyFun(freshTyVar(), freshTyVar()), expr))

        case _ => Failure(TyperError.ContextError("Lambda expects function type expectation or arg context", context, expr))

      // AApp (Application rule) - improved version with better context handling
      case Expr.App(func, arg) =>
        context match {
          case Context.EmptyCtx =>
            // For empty context, we need to handle this more carefully
            for {
              (argTy, s1) <- typeCheckAlg(env, Context.EmptyCtx, arg)
              env_s1 = applySubstToEnv(s1, env)
              argTy_s1 = applySubst(s1, argTy)
              
              // Create a fresh result type variable
              resultTy = freshTyVar()
              expectedFunTy = Ty.TyFun(argTy_s1, resultTy)
              
              // Check if func can be a function that takes argTy_s1 and returns resultTy
              (actualFunTy, s2) <- typeCheckAlg(env_s1, Context.TypeExpectation(expectedFunTy), func)
              
              // Unify the expected and actual function types
              s3 <- unify(applySubst(s2, expectedFunTy), actualFunTy)
              finalSubst = composeSubst(s3, composeSubst(s2, s1))
              finalResultTy = applySubst(finalSubst, resultTy)
            } yield (finalResultTy, finalSubst)
            
          case Context.TypeExpectation(expectedRetTy) =>
            // When we expect a specific return type, we can be more specific
            for {
              (argTy, s1) <- typeCheckAlg(env, Context.EmptyCtx, arg)
              env_s1 = applySubstToEnv(s1, env)
              argTy_s1 = applySubst(s1, argTy)
              expectedFunTy = Ty.TyFun(argTy_s1, expectedRetTy)
              (actualFunTy, s2) <- typeCheckAlg(env_s1, Context.TypeExpectation(expectedFunTy), func)
              s3 <- unify(applySubst(s2, expectedFunTy), actualFunTy)
              finalSubst = composeSubst(s3, composeSubst(s2, s1))
              finalRetTy = applySubst(finalSubst, expectedRetTy)
            } yield (finalRetTy, finalSubst)
            
          case _ =>
            // When there's an outer context, use the teleportation approach
            for {
              (argTy, s1) <- typeCheckAlg(env, Context.EmptyCtx, arg)
              env_s1 = applySubstToEnv(s1, env)
              argTy_s1 = applySubst(s1, argTy)
              (resultTy, s2) <- typeCheckAlg(env_s1, Context.ArgTeleported(argTy_s1, context), func)
              finalSubst = composeSubst(s2, s1)
            } yield (resultTy, finalSubst)
        }


      // Let rule (with let-polymorphism)
      case Expr.Let(varName, value, body) =>
        for {
          (valTy, s1) <- typeCheckAlg(env, Context.EmptyCtx, value) // Infer value type in empty context
          env_s1 = applySubstToEnv(s1, env)
          generalizedValTy = generalize(env_s1, applySubst(s1, valTy))
          newEnv = env_s1 + (varName -> generalizedValTy)
          (bodyTy, s2) <- typeCheckAlg(newEnv, context, body) // body uses original context
        } yield (bodyTy, composeSubst(s2, s1))

      case Expr.If(cond, thenB, elseB) =>
        for {
          (condTy, s1) <- typeCheckAlg(env, Context.TypeExpectation(Ty.TyConst("Bool")), cond)
          env_s1 = applySubstToEnv(s1, env)
          (thenTy, s2) <- typeCheckAlg(env_s1, context, thenB) // thenBranch takes original context
          finalSubst12 = composeSubst(s2, s1)
          env_s12 = applySubstToEnv(finalSubst12, env)
          thenTy_s12 = applySubst(finalSubst12, thenTy)
          // elseBranch must match thenBranch's type, considering the original context's influence on thenBranch
          (elseTy, s3) <- typeCheckAlg(env_s12, Context.TypeExpectation(thenTy_s12), elseB)
          finalSubst = composeSubst(s3, finalSubst12)
          // The final type is thenTy, after all substitutions
        } yield (applySubst(finalSubst, thenTy_s12), finalSubst)

      case Expr.BinaryOp(op, left, right) =>
        op match
          case "+" | "-" | "*" | "/" =>
            for {
              (lTy, s1) <- typeCheckAlg(env, Context.TypeExpectation(Ty.TyConst("Int")), left)
              env_s1 = applySubstToEnv(s1, env)
              (rTy, s2) <- typeCheckAlg(env_s1, Context.TypeExpectation(Ty.TyConst("Int")), right)
              finalSubst = composeSubst(s2, s1)
              // Result type is Int. Match with outer context.
              (resTy, s3) <- matchGenericConsumer(env, Ty.TyConst("Int"), finalSubst, context, expr)
            } yield (resTy, composeSubst(s3, finalSubst))
          case "==" => // Polymorphic equality for base types for now
            val tyVar = freshTyVar()
            for {
              (lTy, s1) <- typeCheckAlg(env, Context.TypeExpectation(tyVar), left)
              env_s1 = applySubstToEnv(s1, env)
              tyVar_s1 = applySubst(s1, tyVar)
              (rTy, s2) <- typeCheckAlg(env_s1, Context.TypeExpectation(tyVar_s1), right)
              finalSubst = composeSubst(s2, s1)
              // Result type is Bool. Match with outer context.
              (resTy, s3) <- matchGenericConsumer(env, Ty.TyConst("Bool"), finalSubst, context, expr)
            } yield (resTy, composeSubst(s3, finalSubst))
          case "&&" | "||" =>
            for {
              (lTy, s1) <- typeCheckAlg(env, Context.TypeExpectation(Ty.TyConst("Bool")), left)
              env_s1 = applySubstToEnv(s1, env)
              (rTy, s2) <- typeCheckAlg(env_s1, Context.TypeExpectation(Ty.TyConst("Bool")), right)
              finalSubst = composeSubst(s2, s1)
              // Result type is Bool. Match with outer context.
              (resTy, s3) <- matchGenericConsumer(env, Ty.TyConst("Bool"), finalSubst, context, expr)
            } yield (resTy, composeSubst(s3, finalSubst))
          case _ => Failure(TyperError.GeneralTyperError(s"Unknown binary operator: $op"))


      // Record Construction
      case Expr.RecordCons(fields) =>
        // Infer types of all field expressions in EmptyCtx
        val initialResult: Try[(Map[String, Ty], Subst)] = Success((Map.empty, Map.empty))
        val inferredFieldTypesTry = fields.foldLeft(initialResult) {
          case (Failure(err), _) => Failure(err)
          case (Success((accFieldTypes, accSubst)), (fieldName, fieldExpr)) =>
            for
              (fieldTy, fieldSubst) <- typeCheckAlg(applySubstToEnv(accSubst, env), Context.EmptyCtx, fieldExpr)
              newAccSubst = composeSubst(fieldSubst, accSubst)
            yield (accFieldTypes + (fieldName -> applySubst(newAccSubst, fieldTy)), newAccSubst)
        }
        for {
          (inferredFs, s1) <- inferredFieldTypesTry
          recordTy = Ty.TyRecord(inferredFs)
          // Match this inferred record type against the outer context
          (finalTy, s2) <- matchGenericConsumer(env, recordTy, s1, context, expr)
        } yield (finalTy, composeSubst(s2, s1))


      // Record Projection (e.g. r.fieldName)
      // This rule needs to ensure that 'r' is type checked expecting a record that *has* fieldName
      case Expr.RecordProj(recordExpr, fieldName) =>
        // The recordExpr is checked in a context where we expect it to provide fieldName
        // The `outerCtx` for the `FieldTeleported` is the original context for the RecordProj expression itself.
        for {
          // First, infer the actual type of the record expression
          (inferredRecordTy, s1) <- typeCheckAlg(env, Context.EmptyCtx, recordExpr)
          
          // Extract the field type from the inferred record type
          fieldTy <- applySubst(s1, inferredRecordTy) match {
            case Ty.TyRecord(fields) =>
              fields.get(fieldName) match {
                case Some(fTy) => Success(fTy)
                case None => Failure(TyperError.FieldNotFound(fieldName, inferredRecordTy))
              }
            case otherTy => Failure(TyperError.NotARecord(otherTy))
          }
          
          // Apply substitution to the field type
          finalFieldTy = applySubst(s1, fieldTy)
          
          // Now, this finalFieldTy needs to be matched against the original `context` of RecordProj
          _ <- matchContext(finalFieldTy, context, s1)
        } yield (finalFieldTy, s1)

  // Helper for ASub-like logic: infer in EmptyCtx, then match against Σ
  // For "generic consumers" (literals, vars, potentially records, etc.)
  private def matchGenericConsumer(
    env: TyEnv,
    inferredTyInEmptyCtx: Ty, // Type A inferred using Γ | □ ⊢ g ⇒ A
    substFromEmptyCtxInfer: Subst,
    currentContext: Context,  // Context Σ
    exprForError: Expr
  ): Try[(Ty, Subst)] = currentContext match
    case Context.EmptyCtx => Success((inferredTyInEmptyCtx, substFromEmptyCtxInfer))
    case Context.TypeExpectation(expectedTy) =>
      for {
        s1 <- unify(applySubst(substFromEmptyCtxInfer, inferredTyInEmptyCtx), applySubst(substFromEmptyCtxInfer, expectedTy))
        finalSubst = composeSubst(s1, substFromEmptyCtxInfer)
      } yield (applySubst(finalSubst, expectedTy), finalSubst) // Return the expected type after successful unification

    case Context.ArgTeleported(argTy, outerCtx) => // `inferredTyInEmptyCtx` must be a function type
      applySubst(substFromEmptyCtxInfer, inferredTyInEmptyCtx) match
        case Ty.TyFun(paramTy, retTy) =>
          for {
            s1 <- unify(paramTy, applySubst(substFromEmptyCtxInfer, argTy)) // param must match teleported arg
            s_total = composeSubst(s1, substFromEmptyCtxInfer)
            // The return type `retTy` now needs to be matched against the `outerCtx`
            (finalRetTy, s2) <- matchGenericConsumer(env, applySubst(s_total, retTy), s_total, outerCtx, exprForError)
          } yield (finalRetTy, composeSubst(s2,s_total))
        case otherTy => Failure(TyperError.NotAFunction(otherTy))

    case Context.FieldTeleported(fieldName, outerCtx) => // `inferredTyInEmptyCtx` must be a record type
      applySubst(substFromEmptyCtxInfer, inferredTyInEmptyCtx) match
        case Ty.TyRecord(fields) =>
          fields.get(fieldName) match
            case Some(fieldType) =>
              // The `fieldType` now needs to be matched against the `outerCtx`
              matchGenericConsumer(env, fieldType, substFromEmptyCtxInfer, outerCtx, exprForError)
            case None => Failure(TyperError.FieldNotFound(fieldName, inferredTyInEmptyCtx))
        case otherTy => Failure(TyperError.NotARecord(otherTy))

  // Simplified matchContext - checks if a type matches context, part of ASub for Γ ⊢ A ≈ Σ
  private def matchContext(ty: Ty, context: Context, currentSubst: Subst): Try[Subst] = context match
    case Context.EmptyCtx => Success(currentSubst)
    case Context.TypeExpectation(expected) =>
      unify(applySubst(currentSubst, ty), applySubst(currentSubst, expected)).map(s => composeSubst(s, currentSubst))
    case Context.ArgTeleported(argT, outerCtx) => ty match
      case Ty.TyFun(p, r) =>
        for {
          s1 <- unify(applySubst(currentSubst, p), applySubst(currentSubst, argT))
          s_total = composeSubst(s1, currentSubst)
          s2 <- matchContext(applySubst(s_total, r), outerCtx, s_total)
        } yield s2
      case _ => Failure(TyperError.NotAFunction(ty))
    case Context.FieldTeleported(fName, outerCtx) => ty match
      case Ty.TyRecord(fs) => fs.get(fName) match
        case Some(fTy) => matchContext(fTy, outerCtx, currentSubst)
        case None => Failure(TyperError.FieldNotFound(fName, ty))
      case _ => Failure(TyperError.NotARecord(ty))


  // Top-level type inference function
  def inferType(expr: Expr, initialEnv: TyEnv = Map.empty): Try[Ty] =
    typeCheckAlg(initialEnv, Context.EmptyCtx, expr).map { case (ty, subst) =>
      // Apply final substitution and generalize if it's a top-level expression
      // For simplicity here, just apply substitution. Full generalization is complex for top-level.
      val finalTy = applySubst(subst, ty)
      // If finalTy still has free type variables not in initialEnv, it's ambiguous or needs more context.
      // For now, we allow it.
      finalTy
    }

end ContextualTyper

@main def runTyperDemo(): Unit =
  import Ty.*
  import Expr.*
  import ContextualTyper.*

  val env: TyEnv = Map(
    "id" -> TyScheme(List("a"), TyFun(TyVar("a"), TyVar("a"))),
    "const" -> TyScheme(List("a", "b"), TyFun(TyVar("a"), TyFun(TyVar("b"), TyVar("a")))),
    "add" -> TyScheme(Nil, TyFun(TyConst("Int"), TyFun(TyConst("Int"), TyConst("Int")))),
    "pairRecord" -> TyScheme(List("a", "b"), TyRecord(Map("fst" -> TyVar("a"), "snd" -> TyVar("b"))))
  )

  def testExpr(name: String, expr: Expr, customEnv: TyEnv = env) =
    println(s"--- Testing: $name ---")
    println(s"Expr: $expr")
    inferType(expr, customEnv) match
      case Success(ty) => println(s"Inferred Type: ${ty.toString}\n")
      case Failure(err) => println(s"Type Error: ${err.getMessage}\n")// ; err.printStackTrace())

  testExpr("Literal Int", LitInt(10))
  testExpr("Literal Bool", LitBool(true))

  testExpr("Variable id", Var("id"))
  testExpr("Variable add", Var("add"))

  testExpr("Simple Lambda", Lambda("x", Var("x"))) // Should be forall a. a -> a (or t1 -> t1)
  testExpr("Lambda with annotation", Lambda("x", Annotated(Var("x"), TyConst("Int")))) // With empty ctx on lambda, still t1->t1

  testExpr("Annotated Lambda", Annotated(Lambda("x", Var("x")), TyFun(TyConst("Int"),TyConst("Int")))) // Int -> Int

  testExpr("Let id", Let("myId", Lambda("x", Var("x")), App(Var("myId"), LitInt(5))))
  testExpr("Let id used twice",
    Let("f", Lambda("x", Var("x")),
      App(App(Var("add"), App(Var("f"), LitInt(10))), App(Var("f"), LitBool(true))) // Error expected: Bool to Int
    )
  )
  testExpr("Let id used polymorphically",
    Let("f", Lambda("x", Var("x")),
      RecordCons(Map("a" -> App(Var("f"), LitInt(10)), "b" -> App(Var("f"), LitBool(true))))
    )
  )

  testExpr("Apply id to Int", App(Var("id"), LitInt(5)))
  testExpr("Apply id to Bool", App(Var("id"), LitBool(true)))
  testExpr("Apply add", App(App(Var("add"), LitInt(3)), LitInt(4)))

  testExpr("If expression", If(LitBool(true), LitInt(1), LitInt(2)))
  testExpr("If with type mismatch", If(LitInt(0), LitInt(1), LitInt(2))) // Error in cond
  testExpr("If with branch mismatch", If(LitBool(true), LitInt(1), LitBool(false))) // Error in branches

  testExpr("Binary Op +", BinaryOp("+", LitInt(5), LitInt(3)))
  testExpr("Binary Op ==", BinaryOp("==", LitString("a"), LitString("b")))
  
  testExpr("Record Construction", RecordCons(Map("name" -> LitString("Alice"), "age" -> LitInt(30))))
  testExpr("Record Projection", RecordProj(RecordCons(Map("name" -> LitString("Bob"), "id" -> LitInt(101))), "name"))
  testExpr("Record Projection from var",
    Let("rec", RecordCons(Map("val" -> LitInt(42))),
      RecordProj(Var("rec"), "val")
    )
  )
  testExpr("Record Projection missing field", RecordProj(RecordCons(Map("name" -> LitString("Carol"))), "age"))


  testExpr("Apply const", App(App(Var("const"), LitInt(7)), LitBool(true))) // (forall a b. a -> b -> a) 7 true ==> Int


  // K combinator: \x -> \y -> x
  val kComb = Lambda("x", Lambda("y", Var("x")))
  testExpr("K Combinator", kComb) // forall t1 t2. t1 -> t2 -> t1

  // S combinator: \f -> \g -> \x -> (f x) (g x)
  val sComb = Lambda("f", Lambda("g", Lambda("x", App(App(Var("f"), Var("x")), App(Var("g"), Var("x"))))))
  // (t1 -> t2 -> t3) -> (t1 -> t2) -> t1 -> t3
  testExpr("S Combinator (Harder)", sComb)

  // Test from paper's motivation: (λx. λy. x + y) 1 2
  val curriedAddManual = Lambda("x", Annotated(Lambda("y", BinaryOp("+", Var("x"), Var("y"))), TyFun(TyConst("Int"), TyConst("Int"))))
  val curriedApp = App(App(Annotated(curriedAddManual, TyFun(TyConst("Int"), TyFun(TyConst("Int"), TyConst("Int")))), LitInt(1)), LitInt(2))
  testExpr("Curried Add App", curriedApp)

  // Without annotations on lambda to see if context helps (harder)
  // The current `App` and `Lambda` logic might struggle with fully unannotated currying
  // without more sophisticated context propagation or bi-directional flow.
  // The `AApp` rule in the paper: Γ | e2::Σ ⊢ e1 ⇒ A→B / Γ | Σ ⊢ e1 e2 ⇒ B
  // suggests that the type of e2 is teleported to e1.
  // My App rule simplifies this a bit by inferring e1 in EmptyCtx then unifying.
  // True teleportation requires `typeCheckAlg` for `func` to directly consume ArgTeleported.
  // Let's try to simulate this with a more direct application for `(λx.λy.x+y) 1 2`
  val unannotatedCurriedAdd = Lambda("x", Lambda("y", BinaryOp("+", Var("x"), Var("y"))))
  val targetTypeForUCA = TyFun(TyConst("Int"), TyFun(TyConst("Int"), TyConst("Int")))
  testExpr("Unannotated Curried Add (with annotation on whole expr)",
    Annotated(App(App(unannotatedCurriedAdd, LitInt(1)), LitInt(2)), TyConst("Int")),
    Map.empty // Fresh env
  )

  // ((λf. f 1) : ((Int → Int) → Int)) (λx. x)
  val exampleApp = App(
    Annotated(
      Lambda("f", App(Var("f"), LitInt(1))),
      TyFun(TyFun(TyConst("Int"), TyConst("Int")), TyConst("Int"))
    ),
    Lambda("x", Var("x"))
  )
  testExpr("Pierce & Turner example (ish)", exampleApp)