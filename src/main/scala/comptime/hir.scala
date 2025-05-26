// package comptime

// import vfs.VfsNode
// import parse.AstNode

// trait Attribute {
//   var attributes: Map[String, Value] = Map()
//   def getAttribute(name: String): Option[Value] = attributes.get(name)

//   def withAttribute(name: String, value: Value): this.type = {
//     attributes += (name -> value)
//     this
//   }

//   def withChangeSet(new_attributes: Map[String, Value]): this.type = {
//     attributes = attributes ++ new_attributes
//     this
//   }
// }

// trait Symbol {
//   var name: Option[String] = None
//   def getName: Option[String] = name
//   def withName(newName: String): this.type = {
//     name = Some(newName)
//     this
//   }
// }

// trait ASTTrackable {
//   var ast: Option[AstLocation] = None
//   def getAstNode: Option[AstLocation] = ast
//   def withAstNode(newAst: AstLocation): this.type = {
//     ast = Some(newAst)
//     this
//   }
// }

// trait Namespace {
//   var children: Map[String, Value] = Map()
//   var uses: List[UseImport] = List()
//   // 检查命名空间中的符号
//   def getChildren: Map[String, Value] = children
//   def withChildren(newChildren: Map[String, Value]): this.type = {
//     children = newChildren
//     this
//   }
//   def withChild(name: String, value: Value): this.type = {
//     children += (name -> value)
//     this
//   }
//   def withMoreChildren(newChildren: Map[String, Value]): this.type = {
//     children = children ++ newChildren
//     this
//   }
//   def withUseImport(use: UseImport): this.type = {
//     uses = use :: uses
//     this
//   }
//   def withUseImports(newUses: List[UseImport]): this.type = {
//     uses = newUses ++ uses
//     this
//   }
//   def removeChild(name: String): Unit = children -= name
//   def removeChildren(names: List[String]): Unit = names.foreach(removeChild)
//   def removeUseImport(use: UseImport): Unit = uses = uses.filterNot(_ == use)
//   def removeUseImports(newUses: List[UseImport]): Unit = uses = uses.filterNot(newUses.contains)
//   def removeAllUseImports(): Unit = uses = List()

//   def lookup(name: String): Option[Value] = children.get(name) match
//     case Some(value) => Some(value)
//     case None => uses.lookup(name)

//   def lookup(path: List[String]): Option[Value] =
//     var result: Option[Value] = None
//     var index = 0
//     while (index < path.length && result.isEmpty) {
//       val name = path(index)
//       result = lookup(name)
//       index += 1
//     }
//     result
// }

// enum UseImport {
//   case UseAll(namespace: Definition)
//   case UseAllExcept(namespace: Definition, except: List[String])
//   case UseOnly(namespace: Definition, only: List[String])
//   case UseThis(namespace: Definition, thisName: String, asName: String)
// }

// extension (uses: List[UseImport]) {
//   def lookup(name: String): Option[Value] = uses.flatMap {
//     case UseImport.UseAll(namespace) => namespace.lookup(name)
//     case UseImport.UseAllExcept(namespace, except) if !except.contains(name) => namespace.lookup(name)
//     case UseImport.UseOnly(namespace, only) if only.contains(name) => namespace.lookup(name)
//     case UseImport.UseThis(namespace, thisName, asName) if asName == name => namespace.lookup(thisName)
//     case _ => None
//   }.headOption
// }

// trait Parent {
//   var parent: Option[Definition] = None;

//   def getParent: Option[Definition] = parent
//   def withParent(newParent: Definition): this.type = {
//     parent = Some(newParent)
//     this
//   }
// }

// trait Definition extends Namespace, Parent, Symbol, Value {
//   def dumpNamespace(): String =
//     val cs = getChildren.map { case (k, v) => s"${v.asInstanceOf[Definition].dumpNamespace()}" }.mkString(" ")
//     val childrenStr = s"(children $cs)"
//     s"(${getName.getOrElse("unknown")} ${this.getClass().getName()} $childrenStr)"

//   def dumpNamespace(writer: java.io.PrintWriter): Unit = writer.write(dumpNamespace())

//   // 命名空间中解析符号
//   def resolve(name: String): Option[Value] = lookup(name) match
//     case Some(value) => Some(value)
//     case None => getParent.flatMap(_.resolve(name))

//   def absolutePath: List[String] = Nil
// }

// enum Param {
//   case Simple(id: String, ty: Type, attributes: Map[String, Value] = Map())
//   case Named(id: String, ty: Type, default: Value, attributes: Map[String, Value] = Map())
//   case Vararg(id: String, ty: Type, attributes: Map[String, Value] = Map())
//   case Comptime(id: String, ty: Type, attributes: Map[String, Value] = Map())
//   case Implicit(id: String, ty: Type, attributes: Map[String, Value] = Map())
//   case Self(id: String, attributes: Map[String, Value] = Map())
//   case RefSelf(id: String, attributes: Map[String, Value] = Map())
//   case Itself(id: String, attributes: Map[String, Value] = Map())
//   case RefItself(id: String, attributes: Map[String, Value] = Map())
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

// trait Expr(ast: Option[AstLocation] = None) extends Value {
//   def eval(): ExprEvalResult = throw new NotImplementedError("eval() not implemented")
// }

// enum ExprEvalResult {
//   case Comptime(value: Value)
//   case Runtime(expr: Expr)
// }

// trait Pattern(yields: List[Declaration] = List(), ast: Option[AstLocation] = None) extends Value

// trait Statement(attributes: Map[String, Value] = Map(), ast: Option[AstLocation] = None) extends Expr {
//   override def getType: Type = VoidType()
// }

// case class AstLocation(file: VfsNode, node: AstNode)
