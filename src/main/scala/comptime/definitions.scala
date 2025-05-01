// package comptime

// trait Definition extends Value {
//   def lookup(name: String): Option[Value] = None
// }

// case class StructField(ty: Type, default: Option[Value] = None, attributes: Map[String, Value] = Map()) extends Value

// case class StructDefinition(
//     attributes: Map[String, Value],
//     fields: Map[String, StructField],
//     children: Map[String, Value],
//     parent: Option[Value] = None
// ) extends Definition

// enum EnumVariant {
//   case Identifer(name: String, attributes: Map[String, Value] = Map())
//   case WithTuple(name: String, fields: List[Type], attributes: Map[String, Value] = Map())
//   case WithStruct(name: String, struct: StructDefinition, attributes: Map[String, Value] = Map())
//   case WithSubEnum(name: String, sub: EnumDefinition, attributes: Map[String, Value] = Map())
//   case PatternDefined(name: String, pattern: Value, attributes: Map[String, Value] = Map())
// }

// case class EnumDefinition(
//     attributes: Map[String, Value],
//     variants: Map[String, EnumVariant],
//     children: Map[String, Value],
//     parent: Option[Value] = None
// ) extends Definition

// case class UnionDefinition(
//     attributes: Map[String, Value],
//     variants: Map[String, Type],
//     children: Map[String, Value],
//     parent: Option[Value] = None
// ) extends Definition

// enum UseImport {
//   case UseAll(namespace: Value)
//   case UseAllExcept(namespace: Value, except: List[String])
//   case UseOnly(namespace: Value, only: List[String])
//   case UseThis(namespace: Value, thisName: String, asName: String)
// }

// enum Param {
//   case Simple(id: String, ty: Type, attributes: Map[String, Value] = Map())
//   case Named(id: String, ty: Type, default: Value, attributes: Map[String, Value] = Map())
//   case Vararg(id: String, ty: Type, attributes: Map[String, Value] = Map())
//   case Comptime(id: String, ty: Type, attributes: Map[String, Value] = Map())
//   case Implicit(id: String, ty: Type, attributes: Map[String, Value] = Map())
//   case Id(id: String, attributes: Map[String, Value] = Map())
// }

// enum Clause {
//   case TypeDeclaration(id: String)
//   case TypedDeclaration(id: String, ty: Type, attributes: Map[String, Value] = Map())
//   case TraitBound(id: String, trait_bound: Value, attributes: Map[String, Value] = Map())
//   // TODO
//   case Requires()
//   case Ensures()
//   case Outcomes()
// }
