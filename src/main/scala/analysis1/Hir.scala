package analysis1

import vfs.VfsNode
import parse.Ast

trait Attribute:
  var attributes: Map[String, Hir] = Map()
  def getAttribute(name: String): Option[Hir] = attributes.get(name)

  def withAttribute(name: String, value: Hir): this.type =
    attributes += (name -> value)
    this

  def withChangeSet(newAttributes: Map[String, Hir]): this.type =
    attributes = attributes ++ newAttributes
    this

trait ASTTrackable:
  var ast: Option[AstLocation] = None
  def getAst: Option[AstLocation] = ast
  def withAst(file: VfsNode, node: Ast): this.type =
    ast = Some(AstLocation(file, node))
    this

  def withAst(node: AstLocation): this.type =
    ast = Some(node)
    this

  def withAst(node: Option[AstLocation]): this.type =
    ast = node
    this

case class AstLocation(file: VfsNode, node: Ast)

enum Hir extends ASTTrackable:
  val id: Int = Hir.nextId
  // definitions that own a scope
  case ModuleDef(name: String, scope: ScopeId, clauses: List[Hir] = Nil, ty: Hir = Hir.TypeAny) extends Hir, Attribute
  case StructDef(
      name: String,
      scope: ScopeId,
      fields: List[Hir.StructDef] = Nil,
      clauses: List[Hir] = Nil,
      ty: Hir = Hir.TypeAny
  ) extends Hir, Attribute
  case EnumDef(name: String, scope: ScopeId, variants: List[Hir] = Nil, clauses: List[Hir] = Nil, ty: Hir = Hir.TypeAny)
      extends Hir, Attribute
  case UnionDef(name: String, scope: ScopeId, fields: List[Hir] = Nil, clauses: List[Hir] = Nil, ty: Hir = Hir.TypeAny)
      extends Hir, Attribute
  case ProjectDef(name: String, scope: ScopeId, ty: Hir = Hir.TypeAny) extends Hir, Attribute
  case PackageDef(name: String, scope: ScopeId, ty: Hir = Hir.TypeAny) extends Hir, Attribute

  case StructField(name: String, fieldType: Hir, default: Hir, clauses: List[Hir] = Nil, definition: Hir)
      extends Hir, Attribute

  case EnumVariantWithPattern(name: String, pattern: Hir, definition: Hir) extends Hir, Attribute

  case EnumVariantWithStruct(name: String, struct: Hir.StructDef, definition: Hir) extends Hir, Attribute

  case EnumVariantWithTuple(name: String, tuple: List[Hir], definition: Hir) extends Hir, Attribute

  case EnumVariantWithSubEnum(name: String, subEnum: Hir.EnumDef, definition: Hir) extends Hir, Attribute

  case UnionVariant(name: String, ty: Hir, definition: Hir) extends Hir, Attribute

  case FunctionDef(
      name: String,
      params: List[Hir.Param] = Nil,
      returnType: Hir = Hir.TypeVoid,
      clauses: List[Hir] = Nil,
      body: Hir = Hir.Invalid,
      ty: Hir = Hir.TypeAny
  ) extends Hir, Attribute
  case Param(name: String, paramType: Hir, default: Hir) extends Hir, Attribute
  // T
  case ClauseDeclTypeWithoutTyped(name: String) extends Hir, Attribute
  case ClauseDecl(name: String, argType: Hir, default: Hir) extends Hir, Attribute
  case ClauseOptionalDecl(name: String, argType: Hir, default: Hir) extends Hir, Attribute
  case ClauseDeclTypeWithTraitBound(name: String, traitBoundExpression: Hir) extends Hir, Attribute
  case ClauseRequires(name: String, requiresExpression: Hir) extends Hir, Attribute
  case ClauseEnsures(name: String, ensuresExpression: Hir) extends Hir, Attribute
  case ClauseDecreases
  case ClauseOutcomes

  case NewtypeDef(name: String, params: List[Hir] = Nil, typeDef: Hir, ty: Hir = Hir.TypeAny) extends Hir, Attribute
  case TypealiasDef(name: String, params: List[Hir] = Nil, typeDef: Hir, ty: Hir = Hir.TypeAny) extends Hir, Attribute

  case TraitDef(
      name: String,
      scope: ScopeId,
      methods: List[Hir.FunctionDef] = Nil,
      clauses: List[Hir] = Nil,
      ty: Hir = Hir.TypeAny
  ) extends Hir, Attribute

  case Implementation(forType: Hir, scope: ScopeId, clauses: List[Hir] = Nil, ty: Hir = Hir.TypeAny)
      extends Hir, Attribute
  case Extension(forType: Hir, scope: ScopeId, clauses: List[Hir] = Nil, ty: Hir = Hir.TypeAny) extends Hir, Attribute
  case TraitImplementation(
      implTrait: Hir,
      forType: Hir,
      scope: ScopeId,
      clauses: List[Hir] = Nil,
      ty: Hir = Hir.TypeAny
  ) extends Hir, Attribute
  case TraitExtension(extendTrait: Hir, forType: Hir, scope: ScopeId, clauses: List[Hir] = Nil, ty: Hir = Hir.TypeAny)
      extends Hir, Attribute

  // Tuple can be type or value
  case Tuple(elements: List[Hir], ty: Hir = Hir.TypeAny) extends Hir, Attribute

  // types
  case TypeVoid
  case TypeNoReturn
  case TypeAny
  case TypeVar(name: String)
  case TypeScheme(params: List[Hir], ty: Hir)
  case TypeObject
  case TypeType
  case TypeInteger
  case TypeReal
  case TypeBool
  case TypeStr
  case TypeChar
  case TypeSymbol
  case TypeFloat(size: Int)
  case TypeInt(size: Int, signed: Boolean = true)
  case TypePointer(to: Hir)
  case TypeOption(to: Hir)
  case TypePattern(to: Hir)
  case TypeWithErrors(errors: Hir, ty: Hir)
  case TypeWithEffects(effects: Hir, ty: Hir)
  case TypeArray(elementType: Hir, size: Hir)
  case TypeFunction(isPure: Boolean, isComptime: Boolean, isDiamond: Boolean, params: List[Hir], returnType: Hir)
  case TypeTraitObject(traitExpr: Hir)

  case TypeNamed(name: String, definition: Hir)
  case TypeAnonymous(ty: Hir)
  case TypeNewtype(name: String, ty: Hir)
  case TypeAlias(name: String, ty: Hir)
  // such as `Vec<T>`
  case TypeApplication(callee: Hir, args: List[Hir]) extends Hir, Attribute

  case TypeOf(term: Hir) extends Hir, Attribute

  // common values that can be used in comptime
  case Integer(value: BigInt)
  case Real(value: BigDecimal)
  case Bool(value: Boolean)
  case Str(value: String)
  case CharVal(value: Char)
  case Symbol(value: String)
  case FloatVal(value: Double, size: Int)
  case IntVal(value: Long, size: Int)
  case Type(value: Hir)
  case Object(children: List[Hir] = Nil, properties: Map[String, Hir] = Map())
  case NullVal
  case UndefinedVal

  case ListVal(elements: List[Hir], ty: Hir = Hir.TypeAny) extends Hir, Attribute
  // expressions
  case BinaryApplication(op: BinaryOp, lhs: Hir, rhs: Hir)
  case UnaryApplication(op: UnaryOp, operand: Hir)

  // 被调用者可能是函数、enum的tuple变体、union的变体、effect、函数指针、其他实现相关Fn FnOnce trait的对象
  case Application(callee: Hir, args: Hir.Object = Hir.Object())
  // target可能是元组、数组、切片、object或其他实现了Indexable trait的类型
  case Index(target: Hir, index: Hir)
  // struct的字段访问
  case FieldAccess(target: Hir, field: String)
  case AutoDerefAccess(target: Hir, field: String)

  case ObjectApplication(target: Hir, method: String, args: List[Hir], obj: Hir.Object = Hir.Object())

  // statements
  case Block(statements: List[Hir])
  case Loop(label: String, body: Hir, condition: Hir = Hir.Bool(true))
  case If(condition: Hir, thenBranch: Hir, elseBranch: Hir = Hir.Invalid)
  case Break(label: String = "")
  case Continue(label: String = "")
  case Return(value: Hir = Hir.Invalid)
  case Assign(lhs: Hir, rhs: Hir)

  case ConstDecl(pattern: Hir, optType: Option[Hir], init: Hir) extends Hir, Attribute
  case LetDecl(pattern: Hir, optType: Option[Hir], init: Hir) extends Hir, Attribute

  // patterns
  case PatternIfGuard(pattern: Hir, condition: Hir, ty: Hir = Hir.TypeAny) extends Hir, Attribute
  case PatternAndIs(left: Hir, expr: Hir, pattern: Hir, ty: Hir = Hir.TypeAny) extends Hir, Attribute
  case PatternAsBind(pattern: Hir, name: String, ty: Hir = Hir.TypeAny) extends Hir, Attribute
  case PatternNot(pattern: Hir, ty: Hir = Hir.TypeAny) extends Hir, Attribute
  case PatternOr(left: Hir, right: Hir, ty: Hir = Hir.TypeAny) extends Hir, Attribute
  case PatternOptionSome(pattern: Hir, ty: Hir = Hir.TypeAny) extends Hir, Attribute
  case PatternErrorOk(pattern: Hir, ty: Hir = Hir.TypeAny) extends Hir, Attribute
  case PatternCall(pattern: Hir, args: List[Hir], ty: Hir = Hir.TypeAny) extends Hir, Attribute
  case PatternObjectCall(pattern: Hir, fields: List[Hir], ty: Hir = Hir.TypeAny) extends Hir, Attribute
  case PatternDiamondCall(pattern: Hir, typeArgs: List[Hir], ty: Hir = Hir.TypeAny) extends Hir, Attribute

  case PatternFromExpr(expr: Hir, ty: Hir = Hir.TypeAny) extends Hir, Attribute
  case PatternRange(
      start: Option[Hir] = None,
      end: Option[Hir] = None,
      inclusive: Boolean = false,
      ty: Hir = Hir.TypeAny
  ) extends Hir, Attribute
  case PropertyPattern(name: String, pattern: Hir, ty: Hir = Hir.TypeAny) extends Hir, Attribute
  case PatternRecord(fields: List[Hir], ty: Hir = Hir.TypeAny) extends Hir, Attribute
  case PatternList(items: List[Hir], ty: Hir = Hir.TypeAny) extends Hir, Attribute
  case PatternTuple(items: List[Hir], ty: Hir = Hir.TypeAny) extends Hir, Attribute
  // case PatternBitVec0x(items: List[Hir], ty: Hir = Hir.TypeAny) extends Hir, Attribute
  // case PatternBitVec0o(items: List[Hir], ty: Hir = Hir.TypeAny) extends Hir, Attribute
  // case PatternBitVec0b(items: List[Hir], ty: Hir = Hir.TypeAny) extends Hir, Attribute
  // case PatternAsync(pattern: Hir, ty: Hir = Hir.TypeAny) extends Hir, Attribute
  // case PatternTypeBind(id: Hir, ty: Hir = Hir.TypeAny) extends Hir, Attribute
  // case Pair(id: Hir, expr: Hir, ty: Hir = Hir.TypeAny) extends Hir, Attribute

  // common
  case Invalid
  // 这通常需要根据指向的ast节点接着解析
  case AwaitingAnalysis
  case Unresolved(name: String)
  // 已解析的变量引用
  case VarRef(name: String, varType: Hir) extends Hir, Attribute
  // 变量定义（用于符号表）
  case Var(name: String, varType: Hir) extends Hir, Attribute

  // S-expression格式序列化
  override def toString: String = this match
    // 定义节点
    case ModuleDef(name, scope, clauses, _) =>
      val clausesStr = if clauses.nonEmpty then s" ${clauses.map(_.toString).mkString(" ")}" else ""
      s"(module $name $scope$clausesStr)"

    case StructDef(name, scope, fields, clauses, _) =>
      val fieldsStr = if fields.nonEmpty then s" (fields ${fields.map(_.toString).mkString(" ")})" else ""
      val clausesStr = if clauses.nonEmpty then s" ${clauses.map(_.toString).mkString(" ")}" else ""
      s"(struct $name $scope$fieldsStr$clausesStr)"

    case EnumDef(name, scope, variants, clauses, _) =>
      val variantsStr = if variants.nonEmpty then s" (variants ${variants.map(_.toString).mkString(" ")})" else ""
      val clausesStr = if clauses.nonEmpty then s" ${clauses.map(_.toString).mkString(" ")}" else ""
      s"(enum $name $scope$variantsStr$clausesStr)"

    case UnionDef(name, scope, fields, clauses, _) =>
      val fieldsStr = if fields.nonEmpty then s" (fields ${fields.map(_.toString).mkString(" ")})" else ""
      val clausesStr = if clauses.nonEmpty then s" ${clauses.map(_.toString).mkString(" ")}" else ""
      s"(union $name $scope$fieldsStr$clausesStr)"

    case ProjectDef(name, scopeId, _) => s"(project $name $scopeId)"

    case PackageDef(name, scopeId, _) => s"(package $name $scopeId)"

    // 结构体字段和枚举变体
    case StructField(name, fieldType, default, clauses, definition) =>
      val clausesStr = if clauses.nonEmpty then s" ${clauses.map(_.toString).mkString(" ")}" else ""
      s"(field $name $fieldType $default$clausesStr $definition)"

    case EnumVariantWithPattern(name, pattern, definition) => s"(variant-pattern $name $pattern $definition)"

    case EnumVariantWithStruct(name, struct, definition) => s"(variant-struct $name $struct $definition)"

    case EnumVariantWithTuple(name, tuple, definition) =>
      s"(variant-tuple $name (${tuple.map(_.toString).mkString(" ")}) $definition)"

    case EnumVariantWithSubEnum(name, subEnum, definition) => s"(variant-enum $name $subEnum $definition)"

    case UnionVariant(name, ty, definition) => s"(union-variant $name $ty $definition)"

    // 函数定义
    case FunctionDef(name, params, returnType, clauses, body, _) =>
      val paramsStr = if params.nonEmpty then s" (params ${params.map(_.toString).mkString(" ")})" else ""
      val clausesStr = if clauses.nonEmpty then s" ${clauses.map(_.toString).mkString(" ")}" else ""
      val bodyStr = if body != Hir.Invalid then s" $body" else ""
      s"(function $name$paramsStr $returnType$clausesStr$bodyStr)"

    case Param(name, paramType, default) => s"(param $name $paramType $default)"

    // 子句
    case ClauseDeclTypeWithoutTyped(name) => s"(clause-type $name)"

    case ClauseDecl(name, argType, default) => s"(clause-decl $name $argType $default)"
    case ClauseOptionalDecl(name, argType, default) => s"(clause-optional-decl $name $argType $default)"

    case ClauseDeclTypeWithTraitBound(name, traitBoundExpression) => s"(clause-trait-bound $name $traitBoundExpression)"

    case ClauseRequires(name, requiresExpression) => s"(clause-requires $name $requiresExpression)"

    case ClauseEnsures(name, ensuresExpression) => s"(clause-ensures $name $ensuresExpression)"

    case ClauseDecreases => "(clause-decreases)"
    case ClauseOutcomes => "(clause-outcomes)"

    // 类型别名
    case NewtypeDef(name, params, ty, _) =>
      val paramsStr = if params.nonEmpty then s" (${params.map(_.toString).mkString(" ")})" else ""
      s"(newtype $name$paramsStr $ty)"

    case TypealiasDef(name, params, ty, _) =>
      val paramsStr = if params.nonEmpty then s" (${params.map(_.toString).mkString(" ")})" else ""
      s"(typealias $name$paramsStr $ty)"

    // Trait定义
    case TraitDef(name, scope, methods, clauses, _) =>
      val methodsStr = if methods.nonEmpty then s" (methods ${methods.map(_.toString).mkString(" ")})" else ""
      val clausesStr = if clauses.nonEmpty then s" ${clauses.map(_.toString).mkString(" ")}" else ""
      s"(trait $name $scope$methodsStr$clausesStr)"

    // 实现和扩展
    case Implementation(forType, scope, clauses, _) =>
      val clausesStr = if clauses.nonEmpty then s" ${clauses.map(_.toString).mkString(" ")}" else ""
      s"(impl $forType $scope$clausesStr)"

    case Extension(forType, scope, clauses, _) =>
      val clausesStr = if clauses.nonEmpty then s" ${clauses.map(_.toString).mkString(" ")}" else ""
      s"(extend $forType $scope$clausesStr)"

    case TraitImplementation(implTrait, forType, scope, clauses, _) =>
      val clausesStr = if clauses.nonEmpty then s" ${clauses.map(_.toString).mkString(" ")}" else ""
      s"(impl-trait $implTrait $forType $scope$clausesStr)"

    case TraitExtension(extendTrait, forType, scope, clauses, _) =>
      val clausesStr = if clauses.nonEmpty then s" ${clauses.map(_.toString).mkString(" ")}" else ""
      s"(extend-trait $extendTrait $forType $scope$clausesStr)"

    // 元组
    case Tuple(elements, _) => s"(tuple ${elements.map(_.toString).mkString(" ")})"

    // 基础类型
    case TypeVoid => "void"
    case TypeNoReturn => "!"
    case TypeAny => "any"
    case TypeVar(name) => s"'$name"
    case TypeScheme(params, ty) => s"(forall (${params.map(_.toString).mkString(" ")}) $ty)"
    case TypeObject => "object"
    case TypeType => "type"
    case TypeInteger => "int"
    case TypeReal => "real"
    case TypeBool => "bool"
    case TypeStr => "str"
    case TypeChar => "char"
    case TypeSymbol => "symbol"
    case TypeFloat(size) => s"f$size"
    case TypeInt(size, signed) => if signed then s"i$size" else s"u$size"
    case TypePointer(to) => s"(ptr $to)"
    case TypeOption(to) => s"(option $to)"
    case TypePattern(to) => s"(pattern $to)"
    case TypeWithErrors(errors, ty) => s"(with-errors $errors $ty)"
    case TypeWithEffects(effects, ty) => s"(with-effects $effects $ty)"
    case TypeArray(elementType, size) => s"(array $elementType $size)"
    case TypeFunction(isPure, isComptime, isDiamond, params, returnType) =>
      val flags = List(
        if isPure then Some("pure") else None,
        if isComptime then Some("comptime") else None,
        if isDiamond then Some("diamond") else None
      ).flatten
      val flagsStr = if flags.nonEmpty then s" ${flags.mkString(" ")}" else ""
      s"(fn$flagsStr (${params.map(_.toString).mkString(" ")}) $returnType)"
    case TypeTraitObject(traitExpr) => s"(trait-object $traitExpr)"
    case TypeNamed(name, definition) => s"(named $name $definition)"
    case TypeAnonymous(ty) => s"(anonymous $ty)"
    case TypeNewtype(name, ty) => s"(newtype-ref $name $ty)"
    case TypeAlias(name, ty) => s"(alias-ref $name $ty)"
    case TypeApplication(callee, args) => s"($callee ${args.map(_.toString).mkString(" ")})"

    case TypeOf(term) => s"(image.type $term)"

    // 值
    case Integer(value) => value.toString
    case Real(value) => value.toString
    case Bool(value) => value.toString
    case Str(value) => s""""$value""""
    case CharVal(value) => s"'$value'"
    case Symbol(value) => s":$value"
    case FloatVal(value, size) => s"${value}f$size"
    case IntVal(value, size) => s"${value}i$size"
    case Type(value) => s"(type $value)"
    case Object(children, properties) =>
      val childrenStr = if children.nonEmpty then s" ${children.map(_.toString).mkString(" ")}" else ""
      val propsStr = if properties.nonEmpty then s" ${properties.map((k, v) => s"($k $v)").mkString(" ")}" else ""
      s"(object$childrenStr$propsStr)"
    case ListVal(elements, ty) =>
      val elementsStr = if elements.nonEmpty then s" (${elements.map(_.toString).mkString(" ")})" else ""
      s"(list$elementsStr $ty)"
    case NullVal => "null"
    case UndefinedVal => "undefined"

    // 表达式
    case BinaryApplication(op, lhs, rhs) => s"(${op.toString.toLowerCase} $lhs $rhs)"

    case UnaryApplication(op, operand) => s"(${op.toString.toLowerCase} $operand)"

    case Application(callee, args) =>
      val argsStr = if args.children.nonEmpty || args.properties.nonEmpty then s" $args" else ""
      s"(call $callee$argsStr)"

    case Index(target, index) => s"(index $target $index)"

    case FieldAccess(target, field) => s"(field-access $target $field)"
    case AutoDerefAccess(target, field) => s"(auto-deref-access $target $field)"

    case ObjectApplication(target, method, args, obj) =>
      val argsStr = if args.nonEmpty then s" ${args.map(_.toString).mkString(" ")}" else ""
      val objStr = if obj.children.nonEmpty || obj.properties.nonEmpty then s" $obj" else ""
      s"(method-call $target $method$argsStr$objStr)"

    // 语句
    case Block(statements) => s"(block ${statements.map(_.toString).mkString(" ")})"

    case Loop(label, body, condition) =>
      val labelStr = if label.nonEmpty then s" :$label" else ""
      s"(loop$labelStr $condition $body)"

    case If(condition, thenBranch, elseBranch) =>
      val elseStr = if elseBranch != Hir.Invalid then s" $elseBranch" else ""
      s"(if $condition $thenBranch$elseStr)"

    case Break(label) =>
      val labelStr = if label.nonEmpty then s" :$label" else ""
      s"(break$labelStr)"

    case Continue(label) =>
      val labelStr = if label.nonEmpty then s" :$label" else ""
      s"(continue$labelStr)"

    case Return(value) =>
      val valueStr = if value != Hir.Invalid then s" $value" else ""
      s"(return$valueStr)"

    case Assign(lhs, rhs) => s"(assign $lhs $rhs)"

    case ConstDecl(pattern, optType, init) =>
      val typeStr = optType.map(t => s" $t").getOrElse("")
      s"(const $pattern$typeStr $init)"

    case LetDecl(pattern, optType, init) =>
      val typeStr = optType.map(t => s" $t").getOrElse("")
      s"(let $pattern$typeStr $init)"

    // 模式
    case PatternIfGuard(pattern, condition, _) => s"(pattern-if $pattern $condition)"

    case PatternAndIs(left, expr, pattern, _) => s"(pattern-and-is $left $expr $pattern)"

    case PatternAsBind(pattern, id, _) => s"(pattern-as $pattern $id)"

    case PatternNot(pattern, _) => s"(pattern-not $pattern)"

    case PatternOr(left, right, _) => s"(pattern-or $left $right)"

    case PatternOptionSome(pattern, _) => s"(pattern-some $pattern)"

    case PatternErrorOk(pattern, _) => s"(pattern-ok $pattern)"

    case PatternCall(pattern, args, _) => s"(pattern-call $pattern ${args.map(_.toString).mkString(" ")})"

    case PatternObjectCall(pattern, fields, _) => s"(pattern-obj-call $pattern ${fields.map(_.toString).mkString(" ")})"

    case PatternDiamondCall(pattern, typeArgs, _) =>
      s"(pattern-diamond-call $pattern ${typeArgs.map(_.toString).mkString(" ")})"

    case PatternFromExpr(expr, _) => s"(pattern-from-expr $expr)"

    case PatternRange(start, end, inclusive, _) =>
      val startStr = start.map(_.toString).getOrElse("")
      val endStr = end.map(_.toString).getOrElse("")
      val inclusiveStr = if inclusive then "inclusive" else "exclusive"
      s"(pattern-range $startStr $endStr $inclusiveStr)"

    case PropertyPattern(id, pattern, _) => s"(property-pattern $id $pattern)"

    case PatternRecord(fields, _) => s"(pattern-record ${fields.map(_.toString).mkString(" ")})"

    case PatternList(items, _) => s"(pattern-list ${items.map(_.toString).mkString(" ")})"

    case PatternTuple(items, _) => s"(pattern-tuple ${items.map(_.toString).mkString(" ")})"

    // 特殊值
    case Invalid => "invalid"
    case AwaitingAnalysis => "awaiting-analysis"
    case Unresolved(name) => s"(unresolved $name)"
    case VarRef(name, varType) => s"(var-ref $name $varType)"
    case Var(name, varType) => s"(var $name $varType)"

object Hir:
  private var currentId: Int = 0
  def nextId: Int =
    val id = currentId
    currentId += 1
    id

// 二元操作
enum BinaryOp:
  case Add, Sub, Mul, Div, Mod
  case Eq, Neq, Gt, Ge, Lt, Le
  case And, Or, Implies, TypeWith, TraitBound
  case AddAdd

// 一元操作
enum UnaryOp:
  case Neg, Not, Deref, Refer
