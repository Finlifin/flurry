package parse

import lex.dumpTokens
import scala.collection.mutable

type MList[T] = mutable.ListBuffer[T]
// List[T] to mutable.ListBuffer[T]
extension [T](list: List[T]) def toMList: MList[T] = mutable.ListBuffer(list*)

case class Span(start: Int, end: Int)

enum Ast:
  var span: Span = Span(0, 0)

  // 基本字面量和标识符
  case Id(name: String)
  case Integer(value: Int)
  case Real(value: Float)
  case Str(value: String)
  case Bool(value: Boolean)
  case LitChar(value: Char)
  case Symbol(name: String)

  // 字面量扩展
  case IntExtension(method: String, left: Ast)
  case RealExtension(method: String, left: Ast)
  case StrExtension(method: String, left: Ast)
  case CharExtension(method: String, left: Ast)

  // 特殊值
  case NullVal
  case SelfVal
  case Itself
  case SelfType

  // 容器和括号表达式
  case ExprFromPattern(pattern: Ast)
  case List(elements: MList[Ast])
  case Tuple(elements: MList[Ast])
  case Parenthesis(expr: Ast)
  case Unit
  case Object(properties: MList[Ast])
  case Lambda(params: MList[String], body: Ast)
  case LambdaWithType(params: MList[String], returnType: Ast, body: Ast)

  // 布尔操作
  case BoolNot(expr: Ast)

  // 类型相关
  case EffectQualifiedType(effect: Ast, typ: Ast)
  case ErrorQualifiedType(error: Ast, typ: Ast)
  case OptionalType(typ: Ast)
  case TraitObjectType(traitExpr: Ast)
  case PointerType(typ: Ast)
  case ForallType(params: MList[Ast], expr: Ast)
  case ForType(params: MList[Ast], expr: Ast)

  // 范围操作
  case RangeTo(end: Ast)
  case RangeToInclusive(end: Ast)
  case RangeFrom(start: Ast)
  case RangeFromTo(start: Ast, end: Ast)
  case RangeFromToInclusive(start: Ast, end: Ast)

  // 算术和逻辑操作符
  case Add(left: Ast, right: Ast)
  case Sub(left: Ast, right: Ast)
  case Mul(left: Ast, right: Ast)
  case Div(left: Ast, right: Ast)
  case Mod(left: Ast, right: Ast)
  case AddAdd(left: Ast, right: Ast)
  case BoolEq(left: Ast, right: Ast)
  case BoolNotEq(left: Ast, right: Ast)
  case BoolAnd(left: Ast, right: Ast)
  case BoolOr(left: Ast, right: Ast)
  case BoolGt(left: Ast, right: Ast)
  case BoolGtEq(left: Ast, right: Ast)
  case BoolLt(left: Ast, right: Ast)
  case BoolLtEq(left: Ast, right: Ast)
  case BoolImplies(left: Ast, right: Ast)
  case BoolMatches(left: Ast, right: Ast)

  // 类型关系
  case TypeWith(expr: Ast, typ: Ast)
  case SubtypeWith(expr: Ast, supertype: Ast)
  case TraitBound(expr: Ast, traitExpr: Ast)
  case FieldMethodBound(expr: Ast, id: Ast, typ: Ast)
  case DeclarationBound(expr: Ast, id: Ast, typ: Ast)

  // 访问和链式操作
  case Select(expr: Ast, id: Ast)
  case Image(expr: Ast, id: Ast)
  case Pipe(left: Ast, right: Ast)
  case PipePrepend(expr: Ast, call: Ast)

  // 指针和转换操作
  case Deref(expr: Ast)
  case Refer(expr: Ast)
  case Await(expr: Ast)
  case HandlerApply(expr: Ast, handler: Ast)
  case TypeCast(expr: Ast, typ: Ast)
  case AsDyn(expr: Ast, typ: Ast)

  // 消除操作
  case EffectElimination(expr: Ast, branches: MList[Ast])
  case ErrorElimination(expr: Ast, branches: MList[Ast])
  case OptionElimination(expr: Ast, block: Ast)
  case EffectPropagation(left: Ast)
  case ErrorPropagation(left: Ast)
  case OptionPropagation(left: Ast)

  // 函数和调用
  case Call(func: Ast, args: MList[Ast])
  case IndexCall(array: Ast, index: Ast)
  case ObjectCall(obj: Ast, childrenOrProperties: MList[Ast])
  case DiamondCall(generic: Ast, typeArgs: MList[Ast])

  // 匹配
  case Match(expr: Ast, branches: MList[Ast])
  case PostMatch(expr: Ast, branches: MList[Ast])
  case PatternBranch(pattern: Ast, body: Ast)
  case ConditionBranch(condition: Ast, body: Ast)
  case CatchBranch(errorName: String, body: Ast)

  // 属性
  case Property(id: Ast, value: Ast)
  case PropertyAssign(id: Ast, value: Ast)

  // 模式
  case PatternIfGuard(pattern: Ast, condition: Ast)
  case PatternAndIs(left: Ast, expr: Ast, pattern: Ast)
  case PatternAsBind(pattern: Ast, id: Ast)
  case PatternOr(left: Ast, right: Ast)
  case PatternOptionSome(pattern: Ast)
  case PatternErrorOk(pattern: Ast)
  case PatternCall(pattern: Ast, args: MList[Ast])
  case PatternObjectCall(pattern: Ast, fields: MList[Ast])
  case PatternDiamondCall(pattern: Ast, typeArgs: MList[Ast])

  // 附加模式类型
  case PatternFromExpr(expr: Ast)
  case PatternRangeTo(end: Ast)
  case PatternRangeToInclusive(end: Ast)
  case PatternRangeFrom(start: Ast)
  case PatternRangeFromTo(start: Ast, end: Ast)
  case PatternRangeFromToInclusive(start: Ast, end: Ast)
  case PropertyPattern(id: Ast, pattern: Ast)
  case PatternRecord(fields: MList[Ast])
  case PatternList(items: MList[Ast])
  case PatternTuple(items: MList[Ast])
  case PatternBitVec0x(items: MList[Ast])
  case PatternBitVec0o(items: MList[Ast])
  case PatternBitVec0b(items: MList[Ast])
  case PatternAsync(pattern: Ast)
  case PatternNot(pattern: Ast)
  case PatternTypeBind(id: Ast)
  case Pair(id: Ast, expr: Ast)

  // 语句
  case ConstDecl(pattern: Ast, typ: Ast, init: Ast)
  case LetDecl(pattern: Ast, typ: Ast, init: Ast)
  case ReturnStatement(expr: Ast, guard: Ast)
  case BreakStatement(label: Ast, guard: Ast)
  case ContinueStatement(label: Ast, guard: Ast)
  case IfStatement(condition: Ast, thenBlock: Ast, elseBlock: Ast)
  case IfIsMatch(expr: Ast, pattern: Ast, body: Ast, elseBlock: Ast)
  case IfMatch(expr: Ast, branches: MList[Ast])
  case WhenStatement(branches: MList[Ast])
  case WhileLoop(label: String, condition: Ast, body: Ast)
  case WhileIsMatch(label: String, expr: Ast, pattern: Ast, body: Ast)
  case WhileMatch(label: String, expr: Ast, branches: MList[Ast])
  case ForLoop(label: String, pattern: Ast, expr: Ast, body: Ast)
  case UseStatement(expr: Ast)
  case PathSelect(path: Ast, symbol: String)
  case PathSelectMulti(path: Ast, symbols: MList[String])
  case PathSelectAll(path: Ast)
  case SuperPath(path: Ast)
  case ExcludePath(symbol: String)
  case PackagePath(path: Ast)
  case PathAsBind(path: Ast, symbol: String)

  case Block(statements: MList[Ast])

  case Assign(location: Ast, value: Ast)

  // 断言和验证
  case Asserts(expr: Ast)
  case Assumes(expr: Ast)
  case Axiom(expr: Ast)
  case Invariant(expr: Ast)
  case Decreases(expr: Ast)
  case Outcomes
  case Requires(expr: Ast)
  case Ensures(expr: Ast)

  case ParamOptional(id: String, typ: Ast, default: Ast)
  case ParamTyped(id: String, typ: Ast)
  case ParamTraitBound(id: String, traitBound: Ast)
  case ParamId(id: String)
  case ParamOptionalId(id: String)
  case ParamSelf
  case ParamSelfRef
  case ParamItself
  case ParamItselfRef
  case ParamRestBind(id: String, typ: Ast)

  case ClauseTraitBoundDecl(id: String, traitExpr: Ast)
  case ClauseDecl(id: String, typ: Ast)
  case ClauseOptionalDecl(id: String, typ: Ast, default: Ast)
  case ClauseTypeDecl(id: String)

  // 定义
  case FunctionDef(id: String, params: MList[Ast], returnType: Ast, clauses: MList[Ast], body: Ast)

  case StructDef(id: String, clauses: MList[Ast], body: MList[Ast])
  case StructField(id: String, typ: Ast, default: Ast)

  case EnumDef(id: String, clauses: MList[Ast], body: MList[Ast])
  case EnumVariantWithPattern(id: String, pattern: Ast)
  case EnumVariantWithTuple(id: String, types: MList[Ast])
  case EnumVariantWithStruct(id: String, fields: MList[Ast])
  case EnumVariantWithSubEnum(id: String, variants: MList[Ast])

  case UnionDef(id: String, clauses: MList[Ast], body: MList[Ast])
  case UnionVariant(id: String, typ: Ast)

  case TraitDef(id: String, clauses: MList[Ast], body: MList[Ast])
  case ImplDef(traitExpr: Ast, forType: Ast, clauses: MList[Ast], body: MList[Ast])
  case ExtendDef(traitExpr: Ast, forType: Ast, clauses: MList[Ast], body: MList[Ast])
  case DeriveDef(traitExprs: MList[Ast], forType: Ast, clauses: MList[Ast])

  case Typealias(id: String, params: MList[Ast], typ: Ast)
  case Newtype(id: String, params: MList[Ast], typ: Ast)

  case ModuleDef(id: String, clauses: MList[Ast], items: MList[Ast])

  // 顶层
  case FileScope(items: MList[Ast])
  // sub module 文件声明
  case ModFile(filename: String)

  case Attribute(attrs: Ast, term: Ast)
  case AttributeSetTrue(attrName: String, term: Ast)
  case Invalid

  def withSpan(span: Span): this.type =
    this.span = span
    this

  override def toString: String = this match
    case Id(name) => s"$name"
    case Integer(value) => s"$value"
    case Real(value) => s"$value"
    case Str(value) => s"$value"
    case Bool(value) => s"$value"
    case LitChar(value) => s"'$value'"
    case Symbol(name) => s".$name"

    case IntExtension(method, left) => s"(int-ext $method $left)"
    case RealExtension(method, left) => s"(real-ext $method $left)"
    case StrExtension(method, left) => s"(str-ext $method $left)"
    case CharExtension(method, left) => s"(char-ext $method $left)"

    case NullVal => "null"
    case SelfVal => "self"
    case Itself => "itself"
    case SelfType => "Self"

    case ExprFromPattern(pattern) => s"(expr-from-pattern $pattern)"
    case List(elements) => s"(list ${elements.mkString(" ")})"
    case Tuple(elements) => s"(tuple ${elements.mkString(" ")})"
    case Parenthesis(expr) => s"(parenthesis $expr)"
    case Unit => "(unit)"
    case Object(properties) => s"(record ${properties.mkString(" ")})"
    case Lambda(params, body) => s"(lambda (${params.mkString(" ")}) $body)"
    case LambdaWithType(params, returnType, body) => s"(lambda (${params.mkString(" ")}) : $returnType $body)"

    case BoolNot(expr) => s"(not $expr)"

    case EffectQualifiedType(effect, typ) => s"(effect~ $effect $typ)"
    case ErrorQualifiedType(error, typ) => s"(error! $error $typ)"
    case OptionalType(typ) => s"(optional $typ)"
    case TraitObjectType(traitExpr) => s"(trait-obj $traitExpr)"
    case PointerType(typ) => s"(ptr $typ)"
    case ForallType(params, expr) => s"(forall (${params.mkString(" ")}) $expr)"
    case ForType(params, expr) => s"(for (${params.mkString(" ")}) $expr)"

    case RangeTo(end) => s"(range-to $end)"
    case RangeToInclusive(end) => s"(range-to-inclusive $end)"
    case RangeFrom(start) => s"(range-from $start)"
    case RangeFromTo(start, end) => s"(range-from-to $start $end)"
    case RangeFromToInclusive(start, end) => s"(range-from-to-inclusive $start $end)"

    case Add(left, right) => s"(+ $left $right)"
    case Sub(left, right) => s"(- $left $right)"
    case Mul(left, right) => s"(* $left $right)"
    case Div(left, right) => s"(/ $left $right)"
    case Mod(left, right) => s"(% $left $right)"
    case AddAdd(left, right) => s"(++ $left $right)"
    case BoolEq(left, right) => s"(== $left $right)"
    case BoolNotEq(left, right) => s"(!= $left $right)"
    case BoolAnd(left, right) => s"(and $left $right)"
    case BoolOr(left, right) => s"(or $left $right)"
    case BoolGt(left, right) => s"(> $left $right)"
    case BoolGtEq(left, right) => s"(>= $left $right)"
    case BoolLt(left, right) => s"(< $left $right)"
    case BoolLtEq(left, right) => s"(<= $left $right)"
    case BoolImplies(left, right) => s"(==> $left $right)"
    case BoolMatches(left, right) => s"(matches $left $right)"

    case TypeWith(expr, typ) => s"(: $expr $typ)"
    case SubtypeWith(expr, supertype) => s"(<: $expr $supertype)"
    case TraitBound(expr, traitExpr) => s"(:- $expr $traitExpr)"
    case FieldMethodBound(expr, id, typ) => s"(:~ $expr $id $typ)"
    case DeclarationBound(expr, id, typ) => s"(:: $expr $id $typ)"

    case Select(expr, id) => s"(select $expr $id)"
    case Image(expr, id) => s"(image $expr $id)"
    case Pipe(left, right) => s"(pipe $left $right)"
    case PipePrepend(expr, call) => s"(pipe-prepend $expr $call)"

    case Deref(expr) => s"(deref $expr)"
    case Refer(expr) => s"(ref $expr)"
    case Await(expr) => s"(await $expr)"
    case HandlerApply(expr, handler) => s"(handler-apply $expr $handler)"
    case TypeCast(expr, typ) => s"(as $expr $typ)"
    case AsDyn(expr, typ) => s"(dyn $expr $typ)"

    case EffectElimination(expr, branches) => s"(effect-elim $expr ${branches.mkString(" ")})"
    case ErrorElimination(expr, branches) => s"(error-elim $expr ${branches.mkString(" ")})"
    case OptionElimination(expr, block) => s"(option-elim $expr $block})"
    case EffectPropagation(left) => s"(effect-prop $left)"
    case ErrorPropagation(left) => s"(error-prop $left)"
    case OptionPropagation(left) => s"(option-prop $left)"

    case Call(caller, args) => s"(call $caller ${args.mkString(" ")})"
    case IndexCall(caller, index) => s"(index $caller $index)"
    case ObjectCall(caller, childrenOrProperties) => s"(obj-call $caller ${childrenOrProperties.mkString(" ")})"
    case DiamondCall(caller, typeArgs) => s"(diamond-call $caller ${typeArgs.mkString(" ")})"

    case Match(expr, branches) => s"(match $expr ${branches.mkString(" ")})"
    case PostMatch(expr, branches) => s"(post-match $expr ${branches.mkString(" ")})"
    case PatternBranch(pattern, body) => s"(pattern-branch $pattern $body)"
    case ConditionBranch(condition, body) => s"(condition-branch $condition $body)"
    case CatchBranch(errorName, body) => s"(catch-branch $errorName $body)"

    case Property(id, value) => s"(property $id $value)"
    case PropertyAssign(id, value) => s"(property-assign $id $value)"

    case PatternIfGuard(pattern, condition) => s"(pattern-if $pattern $condition)"
    case PatternAndIs(left, expr, pattern) => s"(pattern-and-is $left $expr $pattern)"
    case PatternAsBind(pattern, id) => s"(pattern-as $pattern $id)"
    case PatternOr(left, right) => s"(pattern-or $left $right)"
    case PatternOptionSome(pattern) => s"(pattern-some $pattern)"
    case PatternErrorOk(pattern) => s"(pattern-ok $pattern)"
    case PatternCall(pattern, args) => s"(pattern-call $pattern ${args.mkString(" ")})"
    case PatternObjectCall(pattern, fields) => s"(pattern-obj-call $pattern ${fields.mkString(" ")})"
    case PatternDiamondCall(pattern, typeArgs) => s"(pattern-diamond-call $pattern (${typeArgs.mkString(" ")}))"
    case PatternFromExpr(expr) => s"(pattern-from-expr $expr)"
    case PatternRangeTo(end) => s"(pattern-range-to $end)"
    case PatternRangeToInclusive(end) => s"(pattern-range-to-inclusive $end)"
    case PatternRangeFrom(start) => s"(pattern-range-from $start)"
    case PatternRangeFromTo(start, end) => s"(pattern-range-from-to $start $end)"
    case PatternRangeFromToInclusive(start, end) => s"(pattern-range-from-to-inclusive $start $end)"
    case PropertyPattern(id, pattern) => s"(property-pattern $id $pattern)"
    case PatternRecord(fields) => s"(pattern-record ${fields.mkString(" ")})"
    case PatternList(items) => s"(pattern-list ${items.mkString(" ")})"
    case PatternTuple(items) => s"(pattern-tuple ${items.mkString(" ")})"
    case PatternBitVec0x(items) => s"(pattern-bitvec-0x ${items.mkString(" ")})"
    case PatternBitVec0o(items) => s"(pattern-bitvec-0o ${items.mkString(" ")})"
    case PatternBitVec0b(items) => s"(pattern-bitvec-0b ${items.mkString(" ")})"
    case PatternAsync(pattern) => s"(pattern-async $pattern)"
    case PatternNot(pattern) => s"(pattern-not $pattern)"
    case PatternTypeBind(id) => s"(pattern-type-bind $id)"
    case Pair(id, expr) => s"(pair $id $expr)"

    case ConstDecl(pattern, typ, init) =>
      val typeStr = if typ == null then "" else s" $typ"
      s"(const $pattern$typeStr $init)"
    case LetDecl(pattern, typ, init) =>
      val typeStr = if typ == null then "" else s" $typ"
      s"(let $pattern$typeStr $init)"
    case ReturnStatement(expr, guard) => if guard == null then s"(return $expr)" else s"(return $expr $guard)"
    case BreakStatement(label, guard) =>
      if label == null then if guard == null then "(break)" else s"(break $guard)"
      else if guard == null then s"(break $label)"
      else s"(break $label $guard)"
    case ContinueStatement(label, guard) =>
      if label == null then if guard == null then "(continue)" else s"(continue $guard)"
      else if guard == null then s"(continue $label)"
      else s"(continue $label $guard)"
    case IfStatement(condition, thenBlock, elseBlock) =>
      if elseBlock == null then s"(if $condition $thenBlock)" else s"(if $condition $thenBlock $elseBlock)"
    case IfIsMatch(expr, pattern, body, else_) => s"(if-is $expr $pattern $body $else_)"
    case IfMatch(expr, branches) => s"(if-match $expr ${branches.mkString(" ")})"
    case WhenStatement(branches) => s"(when ${branches.mkString(" ")})"
    case WhileLoop(label, condition, body) =>
      if label == null || label.isEmpty then s"(while $condition $body)" else s"(while:$label $condition $body)"
    case WhileIsMatch(label, expr, pattern, body) =>
      if label == null || label.isEmpty then s"(while-is $expr $pattern $body)"
      else s"(while-is:$label $expr $pattern $body)"
    case WhileMatch(label, expr, branches) =>
      if label == null || label.isEmpty then s"(while-match $expr ${branches.mkString(" ")})"
      else s"(while-match:$label $expr ${branches.mkString(" ")})"
    case ForLoop(label, pattern, expr, body) =>
      if label == null || label.isEmpty then s"(for $pattern in $expr $body)"
      else s"(for:$label $pattern in $expr $body)"
    case UseStatement(expr) => s"(use $expr)"
    case PathSelect(path, symbol) => s"(path-select $path $symbol)"
    case PathSelectMulti(path, symbols) => s"(path-select-multi $path (${symbols.mkString(" ")}))"
    case PathSelectAll(path) => s"(path-select-all $path)"
    case SuperPath(path) => s"(super-path $path)"
    case ExcludePath(symbol) => s"(exclude-path $symbol)"
    case PackagePath(path) => s"(package-path $path)"
    case PathAsBind(path, symbol) => s"(path-as-bind $path $symbol)"
    case Block(statements) => s"(block ${statements.mkString(" ")})"

    case Assign(location, value) => s"(assign $location $value)"

    case Asserts(expr) => s"(asserts $expr)"
    case Assumes(expr) => s"(assumes $expr)"
    case Axiom(expr) => s"(axiom $expr)"
    case Invariant(expr) => s"(invariant $expr)"
    case Decreases(expr) => s"(decreases $expr)"
    case Outcomes => "(outcomes)"
    case Requires(expr) => s"(requires $expr)"
    case Ensures(expr) => s"(ensures $expr)"

    case ClauseTraitBoundDecl(id, traitExpr) => s"(clause-trait-bound $id $traitExpr)"
    case ClauseDecl(id, typ) => s"(clause-decl $id $typ)"
    case ClauseOptionalDecl(id, typ, default) =>
      val defaultStr = if default == null then "" else s" = $default"
      s"(clause-optional-decl $id $typ$defaultStr)"
    case ClauseTypeDecl(id) => s"(clause-type-decl $id)"

    case ParamOptional(id, typ, default) =>
      val defaultStr = if default == null then "" else s" = $default"
      s"(param-optional $id $typ$defaultStr)"
    case ParamTyped(id, typ) => s"(param-typed $id $typ)"
    case ParamTraitBound(id, traitBound) => s"(param-trait-bound $id $traitBound)"
    case ParamId(id) => s"(param-id $id)"
    case ParamOptionalId(id) => s"(param-optional-id $id)"
    case ParamSelf => "(param-self)"
    case ParamSelfRef => "(param-self-ref)"
    case ParamItself => "(param-itself)"
    case ParamItselfRef => "(param-itself-ref)"
    case ParamRestBind(id, typ) => s"(param-rest-bind $id $typ)"

    case FunctionDef(id, params, returnType, clauses, body) =>
      val clausesStr = if clauses.isEmpty then "" else s" (${clauses.mkString(" ")})"
      s"(fn $id (${params.mkString(" ")}) $returnType$clausesStr $body)"
    case StructDef(id, clauses, body) =>
      val clausesStr = if clauses.isEmpty then "" else s" (${clauses.mkString(" ")})"
      s"(struct $id$clausesStr (${body.mkString(" ")}))"
    case StructField(id, typ, default) =>
      val defaultStr = if default == null then "" else s" = $default"
      s"(field $id $typ$defaultStr)"
    case EnumDef(id, clauses, body) =>
      val clausesStr = if clauses.isEmpty then "" else s" (${clauses.mkString(" ")})"
      s"(enum $id$clausesStr (${body.mkString(" ")}))"
    case EnumVariantWithPattern(id, pattern) => s"(variant-pattern $id $pattern)"
    case EnumVariantWithTuple(id, types) => s"(variant-tuple $id ${types.mkString(" ")})"
    case EnumVariantWithStruct(id, fields) => s"(variant-struct $id ${fields.mkString(" ")})"
    case EnumVariantWithSubEnum(id, variants) => s"(variant-enum $id ${variants.mkString(" ")})"
    case UnionDef(id, clauses, body) =>
      val clausesStr = if clauses.isEmpty then "" else s" (${clauses.mkString(" ")})"
      s"(union $id$clausesStr (${body.mkString(" ")}))"
    case UnionVariant(id, typ) => s"(union-variant $id $typ)"
    case TraitDef(id, clauses, body) =>
      val clausesStr = if clauses.isEmpty then "" else s" (${clauses.mkString(" ")})"
      s"(trait $id$clausesStr (${body.mkString(" ")}))"
    case ImplDef(traitExpr, forType, clauses, body) =>
      val clausesStr = if clauses.isEmpty then "" else s" (${clauses.mkString(" ")})"
      s"(impl $traitExpr $forType$clausesStr (${body.mkString(" ")}))"
    case ExtendDef(traitExpr, forType, clauses, body) =>
      val clausesStr = if clauses.isEmpty then "" else s" (${clauses.mkString(" ")})"
      s"(extend $traitExpr $forType$clausesStr (${body.mkString(" ")}))"
    case DeriveDef(traitExprs, forType, clauses) =>
      val clausesStr = if clauses.isEmpty then "" else s" (${clauses.mkString(" ")})"
      s"(derive ${traitExprs.mkString(" ")} $forType$clausesStr)"
    case Typealias(id, params, typ) =>
      val paramsStr = if params.isEmpty then "" else s" (${params.mkString(" ")})"
      s"(typealias $id$paramsStr $typ)"
    case Newtype(id, params, typ) =>
      val paramsStr = if params.isEmpty then "" else s" (${params.mkString(" ")})"
      s"(newtype $id$paramsStr $typ)"
    case ModuleDef(id, clauses, items) =>
      val clausesStr = if clauses.isEmpty then "" else s" (${clauses.mkString(" ")})"
      s"(module $id$clausesStr ${items.mkString(" ")})"
    case FileScope(items) => s"(file-scope ${items.mkString(" ")})"
    case ModFile(filename) => s"(mod-file $filename)"

    case Attribute(attrs, term) => s"(attribute $attrs $term)"
    case AttributeSetTrue(attrName, term) => s"(attribute-set-true $attrName $term)"
    case Invalid => "(invalid)"
