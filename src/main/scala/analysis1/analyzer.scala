// package analysis1

// import vfs.VfsNode
// import comptime.Definition
// import scala.collection.mutable.Stack
// import comptime.ProjectDefinition
// import comptime.PackageDefinition
// import comptime.UseImport
// import vfs.Vfs
// import vfs.VfsNodeKind
// import comptime.ModDefinition
// import errors.FlurryError
// import parse.*
// import comptime.Value
// import comptime.StructDefinition

// class Analyzer {
//   var project: Definition = ProjectDefinition(builtins = Map())
//   var stack: Stack[Context] = Stack()

//   def initBuiltins() =
//     val math = PackageDefinition().withName("math")
//     val builtin = PackageDefinition().withName("builtin")
//     val build = PackageDefinition().withName("build")
//     val std = PackageDefinition().withName("std")
//     val meta = PackageDefinition().withName("meta")

//     val package_main = PackageDefinition().withName("main").withUseImports(List(
//       UseImport.UseAll(math),
//       UseImport.UseAll(builtin),
//       UseImport.UseAll(build),
//       UseImport.UseAll(std),
//       UseImport.UseAll(meta)
//     ))

//     project =
//       ProjectDefinition(Map("math" -> math, "builtin" -> builtin, "build" -> build, "std" -> std, "meta" -> meta))
//         .withChild("main", package_main)

//   def fsSymbolAllocation(vfs: Vfs): Unit =
//     val srcDirectory = vfs.root.children.flatMap(_.find(_.kind == VfsNodeKind.SrcDirectory)) match
//       case Some(src) => src
//       case None => throw new Exception("No src directory found")

//     val main = project.lookup("main").get
//     val mainPackage = main.asInstanceOf[PackageDefinition]
//     // Process source directory and add it to main package
//     val srcModule = fsSymbolAllocation(srcDirectory)
//     mainPackage.children = mainPackage.children ++ srcModule.children

//   def fsSymbolAllocation(file: VfsNode): ModDefinition = file.kind match
//     case VfsNodeKind.File => ModDefinition().withName(file.name.dropPostfix(".fl"))
//     case VfsNodeKind.Directory =>
//       val children = file.children.map(_.filter(_.name != "mod.fl").map(child => fsSymbolAllocation(child)))
//       val children_map = children.getOrElse(List.empty).map(child => child.name.get -> child).toMap
//       ModDefinition().withName(file.name.dropPostfix(".fl")).withChildren(children_map)
//     case VfsNodeKind.Root =>
//       val children = file.children.map(_.map(child => fsSymbolAllocation(child)))
//       val children_map = children.getOrElse(List.empty).map(child => child.name.get -> child).toMap
//       ModDefinition().withName(file.name.dropPostfix(".fl")).withChildren(children_map)
//     case VfsNodeKind.SrcDirectory =>
//       val children = file.children.map(_.filter(_.name != "main.fl").map(child => fsSymbolAllocation(child)))
//       val children_map = children.getOrElse(List.empty).map(child => child.name.get -> child).toMap
//       ModDefinition().withName(file.name.dropPostfix(".fl")).withChildren(children_map)

//   def resolveFileScope(file: VfsNode, definition: Definition): Unit =
//     enter(file, definition)
//     namespaceSymbolAllocation(file.ast.get.root.asInstanceOf[AstNodeN].getChildren, definition)
//     exit()

//   def namespaceSymbolAllocation(nodes: List[AstNode], definition: Definition): Unit = nodes.foreach(node =>
//     node match
//       case two: AstNode2 => two.getTag match
//           case Tag.mod =>
//             val mod_name = two.getLeft.getString match
//               case Some(name) => name
//               case None => throw ResolusionError.UnnamedTopLevelDefinition(node)
//             val nodes = two.getRight.asInstanceOf[AstNodeN].getChildren
//             val mod_definition = ModDefinition().withName(mod_name)
//             definition.withMoreChildren(Map(mod_name -> mod_definition))
//             namespaceSymbolAllocation(nodes, mod_definition)
//           case _ => ()
//       // case AstNode.UseImport(name, alias) => definition.withUseImport(UseImport(name, alias))
//       case three: AstNode3 => three.getTag match
//           case Tag.struct_def =>
//             val struct_name = three.getFirst.getString match
//               case Some(name) => name
//               case None => throw ResolusionError.UnnamedTopLevelDefinition(node)
//             val struct_definition = StructDefinition().withName(struct_name)
//             definition.withMoreChildren(Map(struct_name -> struct_definition))
//             namespaceSymbolAllocation(three.getThird.asInstanceOf[AstNodeN].getChildren, struct_definition)
//           case Tag.enum_def =>
//             val enum_name = three.getFirst.getString match
//               case Some(name) => name
//               case None => throw ResolusionError.UnnamedTopLevelDefinition(node)
//             val enum_definition = StructDefinition().withName(enum_name)
//             definition.withMoreChildren(Map(enum_name -> enum_definition))
//             namespaceSymbolAllocation(three.getThird.asInstanceOf[AstNodeN].getChildren, enum_definition)
//           case Tag.union_def =>
//             val union_name = three.getFirst.getString match
//               case Some(name) => name
//               case None => throw ResolusionError.UnnamedTopLevelDefinition(node)
//             val union_definition = StructDefinition().withName(union_name)
//             definition.withMoreChildren(Map(union_name -> union_definition))
//             namespaceSymbolAllocation(three.getThird.asInstanceOf[AstNodeN].getChildren, union_definition)
//           case _ => ()

//       case _ => ()
//   )

//   def enter(file: VfsNode, definition: Definition) =
//     if stack.exists(_.definition eq definition) then throw new Exception(s"Cycle detected")
//     file.ensureAst() match
//       case Left(e) => throw ResolusionError.ParseFailed(e)
//       case _ => ()
//     stack.push(Context(file, definition))
//   def exit() = stack.pop()
//   def current(): Context = stack.headOption.getOrElse(null)
// }

// extension (str: String) {
//   def dropPostfix(postfix: String): String =
//     if (str.endsWith(postfix)) str.substring(0, str.length - postfix.length) else str
// }

// case class Context(file: VfsNode, definition: Definition)

// enum ResolusionError extends FlurryError {
//   case ParseFailed(err: ParseError)
//   case UnnamedTopLevelDefinition(node: AstNode)
// }
