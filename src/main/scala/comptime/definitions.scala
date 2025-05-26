// package comptime

// import parse.Tag

// case class StructField(ty: Type, default: Option[Value] = None, attributes: Map[String, Value] = Map()) extends Value

// case class StructDefinition(fields: Map[String, StructField] = Map()) extends Definition {
//   override def getType: Type = TypeType()
// }

// case class FunctionDefinition(
//     params: List[Param] = List(),
//     returnType: Type = VoidType(),
//     clauses: List[Clause] = List(),
//     body: Block = Block(List())
// ) extends Definition

// // TODO: override lookup and resolve
// case class GenericWrapper(clauses: List[Clause], defintion: Definition) extends Definition {
//   override def lookup(name: String): Option[Value] = defintion.lookup(name)
// }

// enum EnumVariant {
//   case Identifer(name: String, attributes: Map[String, Value] = Map())
//   case WithTuple(name: String, fields: List[Type], attributes: Map[String, Value] = Map())
//   case WithStruct(name: String, struct: StructDefinition, attributes: Map[String, Value] = Map())
//   case WithSubEnum(name: String, sub: EnumDefinition, attributes: Map[String, Value] = Map())
//   case PatternDefined(name: String, pattern: Value, attributes: Map[String, Value] = Map())
// }

// case class EnumDefinition(variants: Map[String, EnumVariant] = Map()) extends Definition {
//   override def getType: Type = TypeType()
// }

// case class UnionDefinition() extends Definition {
//   override def getType: Type = TypeType()
// }

// case class ModDefinition() extends Definition, ASTTrackable {
//   override def getType: Type = TypeType()
// }

// case class TraitDefinition() extends Definition {
//   override def getType: Type = TraitType()
// }

// case class ImplementationDefinition(forType: Type, withTrait: Option[Value]) extends Definition {
//   override def getType: Type = ImplementationType(forType, withTrait)
// }

// case class ExtensionDefinition(forType: Type, withTrait: Option[Value]) extends Definition {
//   override def getType: Type = ExtensionType(forType, withTrait)
// }

// // TODO
// case class NewtypeDefinition(expr: Value) extends Definition {
//   override def getType: Type = expr.getType
// }

// case class TypeAliasDefinition(expr: Value) extends Attribute, ASTTrackable, Symbol, Value {
//   override def getType: Type = expr.getType
// }

// case class ProjectDefinition(builtins: Map[String, Value]) extends Definition {
//   override def getType: Type = TypeType()

//   override def lookup(name: String): Option[Value] = builtins.get(name) match
//     case Some(value) => Some(value)
//     case None => super.lookup(name)

//   override def dumpNamespace(): String =
//     val bs = builtins.map { case (k, v) => s"${v.asInstanceOf[Definition].dumpNamespace()}" }.mkString(" ")
//     val builtinsStr = s"(builtins $bs)"
//     val cs = children.map { case (k, v) => s"${v.asInstanceOf[Definition].dumpNamespace()}" }.mkString(" ")
//     val childrenStr = s"(children $cs)"
//     s"(${name.getOrElse("unknown")} ${this.getClass().getName()} $builtinsStr $childrenStr)"
// }

// case class PackageDefinition() extends Definition {
//   override def getType: Type = TypeType()
// }
