package comptime

trait Type extends Value

/*
struct Node {
    next: *Node,
    value: i32,
}
--> NamedType("Node", Struct(StructDefinition(...)))
 */

case class ApplicationType(fn: Value, args: List[Value], optional_args: Map[String, Value]) extends Type
case class NamedType(name: String, ty: Type) extends Type
case class Struct(definition: StructDefinition) extends Type
case class Enum(definition: EnumDefinition) extends Type
case class Union(definition: UnionDefinition) extends Type
case class FunctionType(args: List[Value], return_type: Value, properties: FuntionProperties) extends Type
case class TupleType(elements: List[Type]) extends Type
case class Newtype(name: String, definition: NewtypeDefinition) extends Type
case class TypeAlias(name: String, definition: TypeAliasDefinition) extends Type

case class SliceType(ty: Type) extends Type
case class ArrayType(ty: Type, size: Int) extends Type
case class PointerType(ty: Type) extends Type
case class OptionalType(ty: Type) extends Type
case class EffectQualifiedType(effects: List[Value], ty: Type) extends Type
case class ErrorQualifiedType(errors: List[Value], ty: Type) extends Type

case class SymbolType() extends Type
case class IntType(size: Int) extends Type
case class FloatType(size: Int) extends Type
case class IntegerType() extends Type
case class RealType() extends Type
case class BoolType() extends Type
case class CharType() extends Type
case class StringType() extends Type
case class VoidType() extends Type
case class NoReturnType() extends Type
case class AnyType() extends Type
case class ListType(ty: Type) extends Type
case class ObjectType() extends Type
case class TypeType() extends Type
case class TraitType() extends Type
case class ImplementationType(ty: Type, _trait: Option[Value]) extends Type
case class ExtensionType(ty: Type, _trait: Option[Value]) extends Type

case class BitVecType(size: Int) extends Type
case class PatternType(ty: Type) extends Type

case class ForallType(definition: ForallDefinition) extends Type
case class TypeVariable(name: String) extends Type

// class TypePool {
//     private var types: Set[Type] = Set()

// }
