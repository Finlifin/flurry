package comptime

trait Definition(
    attributes: Map[String, Value] = Map(),
    children: Map[String, Value] = Map(),
    uses: List[UseImport] = List(),
    parent: Option[Definition] = None,
    name: Option[String] = None
) extends Value {
  def lookup(name: String): Option[Value] = children.get(name) match
    case Some(value) => Some(value)
    case None => uses.lookup(name)

  def resolve(name: String): Option[Value] = lookup(name) match
    case Some(value) => Some(value)
    case None => parent.flatMap(_.resolve(name))

  def resolve(path: List[String]): Option[Value] = path match
    case Nil => None
    case head :: tail => lookup(head) match
        case Some(value) => value.getNamespace.flatMap(_.resolve(tail))
        case None => parent.flatMap(_.resolve(path))
}

case class StructField(ty: Type, default: Option[Value] = None, attributes: Map[String, Value] = Map()) extends Value

case class StructDefinition(
    attributes: Map[String, Value] = Map(),
    fields: Map[String, StructField] = Map(),
    children: Map[String, Value] = Map(),
    uses: List[UseImport] = List(),
    parent: Option[Definition] = None,
    name: Option[String] = None
) extends Definition(attributes, children, uses, parent, name) {
  override def getType: Type = TypeType()
}

case class FunctionDefinition(
    attributes: Map[String, Value] = Map(),
    params: List[Param] = List(),
    returnType: Type = VoidType(),
    clauses: List[Clause] = List(),
    body: Block = Block(List()),
    parent: Option[Definition] = None,
    name: Option[String] = None
) extends Definition(attributes, Map(), Nil, parent, name)

case class EffectDefinition(
    attributes: Map[String, Value] = Map(),
    effects: List[Value] = List(),
    clauses: List[Clause] = List(),
    parent: Option[Definition] = None,
    name: Option[String] = None
) extends Definition(attributes, Map(), Nil, parent, name)

// TODO: override lookup and resolve
case class GenericDefinition(
    clauses: List[Clause],
    defintion: Definition,
    parent: Option[Definition] = None,
    name: Option[String] = None
) extends Definition(Map(), Map(), Nil, parent, name)

enum EnumVariant {
  case Identifer(name: String, attributes: Map[String, Value] = Map())
  case WithTuple(name: String, fields: List[Type], attributes: Map[String, Value] = Map())
  case WithStruct(name: String, struct: StructDefinition, attributes: Map[String, Value] = Map())
  case WithSubEnum(name: String, sub: EnumDefinition, attributes: Map[String, Value] = Map())
  case PatternDefined(name: String, pattern: Value, attributes: Map[String, Value] = Map())
}

case class EnumDefinition(
    attributes: Map[String, Value] = Map(),
    variants: Map[String, EnumVariant] = Map(),
    children: Map[String, Value] = Map(),
    uses: List[UseImport] = List(),
    parent: Option[Definition] = None,
    name: Option[String] = None
) extends Definition(attributes, children, uses, parent, name) {
  override def getType: Type = TypeType()
}

case class UnionDefinition(
    attributes: Map[String, Value] = Map(),
    variants: Map[String, Type] = Map(),
    children: Map[String, Value] = Map(),
    uses: List[UseImport] = List(),
    parent: Option[Definition] = None,
    name: Option[String] = None
) extends Definition(attributes, children, uses, parent, name) {
  override def getType: Type = TypeType()
}

case class ModDefinition(
    attributes: Map[String, Value] = Map(),
    children: Map[String, Value] = Map(),
    uses: List[UseImport] = List(),
    parent: Option[Definition] = None,
    name: Option[String] = None
) extends Definition(attributes, children, uses, parent, name) {
  override def getType: Type = TypeType()
}

case class TraitDefinition(
    attributes: Map[String, Value] = Map(),
    children: Map[String, Value] = Map(),
    uses: List[UseImport] = List(),
    parent: Option[Definition] = None
) extends Definition(attributes, children, uses, parent) {
  override def getType: Type = TraitType()
}

case class ImplementationDefinition(
    attributes: Map[String, Value] = Map(),
    children: Map[String, Value] = Map(),
    forType: Type,
    withTrait: Option[Value] = None,
    uses: List[UseImport] = List(),
    parent: Option[Definition] = None
) extends Definition(attributes, children, uses, parent) {
  override def getType: Type = ImplementationType(forType, withTrait)
}

case class ExtensionDefinition(
    attributes: Map[String, Value] = Map(),
    children: Map[String, Value] = Map(),
    forType: Type,
    withTrait: Option[Value] = None,
    uses: List[UseImport] = List(),
    parent: Option[Definition] = None,
    name: Option[String] = None
) extends Definition(attributes, children, uses, parent, name) {
  override def getType: Type = ExtensionType(forType, withTrait)
}

// TODO
case class NewtypeDefinition(attributes: Map[String, Value] = Map(), expr: Value)
    extends Definition(attributes, Map(), Nil, None, None) {
  override def getType: Type = expr.getType
}

case class TypeAliasDefinition(attributes: Map[String, Value] = Map(), expr: Value)
    extends Definition(attributes, Map(), Nil, None, None) {
  override def getType: Type = expr.getType
}

enum UseImport {
  case UseAll(namespace: Definition)
  case UseAllExcept(namespace: Definition, except: List[String])
  case UseOnly(namespace: Definition, only: List[String])
  case UseThis(namespace: Definition, thisName: String, asName: String)
}

extension (uses: List[UseImport]) {
  def lookup(name: String): Option[Value] = uses.flatMap {
    case UseImport.UseAll(namespace) => namespace.lookup(name)
    case UseImport.UseAllExcept(namespace, except) if !except.contains(name) => namespace.lookup(name)
    case UseImport.UseOnly(namespace, only) if only.contains(name) => namespace.lookup(name)
    case UseImport.UseThis(namespace, thisName, asName) if asName == name => namespace.lookup(thisName)
    case _ => None
  }.headOption
}

enum Param {
  case Simple(id: String, ty: Type, attributes: Map[String, Value] = Map())
  case Named(id: String, ty: Type, default: Value, attributes: Map[String, Value] = Map())
  case Vararg(id: String, ty: Type, attributes: Map[String, Value] = Map())
  case Comptime(id: String, ty: Type, attributes: Map[String, Value] = Map())
  case Implicit(id: String, ty: Type, attributes: Map[String, Value] = Map())
  case Self(id: String, attributes: Map[String, Value] = Map())
  case RefSelf(id: String, attributes: Map[String, Value] = Map())
  case Itself(id: String, attributes: Map[String, Value] = Map())
  case RefItself(id: String, attributes: Map[String, Value] = Map())
  case Id(id: String, attributes: Map[String, Value] = Map())
}

enum Clause {
  case TypeDeclaration(id: String)
  case TypedDeclaration(id: String, ty: Type, attributes: Map[String, Value] = Map())
  case TraitBound(id: String, trait_bound: Value, attributes: Map[String, Value] = Map())
  // TODO
  case Requires()
  case Ensures()
  case Outcomes()
}
