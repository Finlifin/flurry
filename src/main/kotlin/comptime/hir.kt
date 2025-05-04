package comptime

import vfs.VfsNode
import parse.AstNode

interface Attribute {
    var attributes: MutableMap<String, Value>

    fun getAttribute(name: String): Value? = attributes[name]

    fun setAttribute(name: String, value: Value) {
        attributes[name] = value
    }

    fun updateChangeSet(newAttributes: Map<String, Value>) {
        attributes.putAll(newAttributes)
    }
}

interface Symbol {
    var name: String?

    fun getName(): String? = name
    fun setName(newName: String) {
        name = newName
    }
}

interface ASTTrackable {
    var ast: AstLocation?

    fun getAst(): AstLocation? = ast
    fun setAst(newAst: AstLocation) {
        ast = newAst
    }
}

interface Namespace {
    var children: MutableMap<String, Value>
    var uses: MutableList<UseImport>

    // 检查命名空间中的符号
    fun getChildren(): Map<String, Value> = children
    fun setChildren(newChildren: Map<String, Value>) {
        children = newChildren.toMutableMap()
    }
    fun addChild(name: String, value: Value) {
        children[name] = value
    }
    fun addChildren(newChildren: Map<String, Value>) {
        children.putAll(newChildren)
    }
    fun addUseImport(use: UseImport) {
        uses.add(0, use)
    }
    fun addUseImports(newUses: List<UseImport>) {
        uses.addAll(0, newUses)
    }
    fun removeChild(name: String) {
        children.remove(name)
    }
    fun removeChildren(names: List<String>) {
        names.forEach { removeChild(it) }
    }
    fun removeUseImport(use: UseImport) {
        uses.removeAll { it == use }
    }
    fun removeUseImports(newUses: List<UseImport>) {
        uses.removeAll { newUses.contains(it) }
    }
    fun removeAllUseImports() {
        uses.clear()
    }

    fun lookup(name: String): Value? = children[name] ?: uses.lookup(name)

    fun lookup(path: List<String>): Value? {
        var result: Value? = null
        var index = 0
        while (index < path.size && result == null) {
            val name = path[index]
            result = lookup(name)
            index++
        }
        return result
    }
}

sealed class UseImport {
    class UseAll(val namespace: Definition) : UseImport()
    class UseAllExcept(val namespace: Definition, val except: List<String>) : UseImport()
    class UseOnly(val namespace: Definition, val only: List<String>) : UseImport()
    class UseThis(val namespace: Definition, val thisName: String, val asName: String) : UseImport()
}

// Kotlin扩展函数替代Scala的extension
fun List<UseImport>.lookup(name: String): Value? {
    return this.firstNotNullOfOrNull { use ->
        when (use) {
            is UseImport.UseAll -> use.namespace.lookup(name)
            is UseImport.UseAllExcept -> if (!use.except.contains(name)) use.namespace.lookup(name) else null
            is UseImport.UseOnly -> if (use.only.contains(name)) use.namespace.lookup(name) else null
            is UseImport.UseThis -> if (use.asName == name) use.namespace.lookup(use.thisName) else null
        }
    }
}

interface Parent {
    var parent: Definition?

    fun getParent(): Definition? = parent
    fun setParent(newParent: Definition) {
        parent = newParent
    }
}

interface Definition : Namespace, Parent, Symbol, Value {
    fun dumpNamespace(): String {
        println("Dumping namespace for ${this.javaClass.name}")
        val cs = getChildren().entries.joinToString(" ") { (k, v) ->
            "($k ${v.javaClass.name} ${(v as Definition).dumpNamespace()})"
        }
        val childrenStr = "(children $cs)"
        return "(${getName() ?: "unknown"} $childrenStr)"
    }

    fun dumpNamespace(writer: java.io.PrintWriter) {
        writer.write(dumpNamespace())
    }

    // 命名空间中解析符号
    fun resolve(name: String): Value? = lookup(name) ?: getParent()?.resolve(name)

    fun absolutePath(): List<String> = emptyList()
}

sealed class Param {
    class Simple(val id: String, val ty: Type, val attributes: Map<String, Value> = emptyMap()) : Param()
    class Named(val id: String, val ty: Type, val default: Value, val attributes: Map<String, Value> = emptyMap()) : Param()
    class Vararg(val id: String, val ty: Type, val attributes: Map<String, Value> = emptyMap()) : Param()
    class Comptime(val id: String, val ty: Type, val attributes: Map<String, Value> = emptyMap()) : Param()
    class Implicit(val id: String, val ty: Type, val attributes: Map<String, Value> = emptyMap()) : Param()
    class Self(val id: String, val attributes: Map<String, Value> = emptyMap()) : Param()
    class RefSelf(val id: String, val attributes: Map<String, Value> = emptyMap()) : Param()
    class Itself(val id: String, val attributes: Map<String, Value> = emptyMap()) : Param()
    class RefItself(val id: String, val attributes: Map<String, Value> = emptyMap()) : Param()
    class Id(val id: String, val attributes: Map<String, Value> = emptyMap()) : Param()
}

sealed class Clause {
    class TypeDeclaration(val id: String) : Clause()
    class TypedDeclaration(val id: String, val ty: Type, val attributes: Map<String, Value> = emptyMap()) : Clause()
    class TraitBound(val id: String, val traitBound: Value, val attributes: Map<String, Value> = emptyMap()) : Clause()
    // TODO
    class Requires : Clause()
    class Ensures : Clause()
    class Outcomes : Clause()
}

// 在Kotlin中，抽象类更接近Scala的traits
abstract class Expr(open val ast: AstLocation? = null) : Value

abstract class Pattern(val yields: List<Declaration> = emptyList(), override val ast: AstLocation? = null) : Value

abstract class Statement(val attributes: Map<String, Value> = emptyMap(), override val ast: AstLocation? = null) : Expr(ast) {
    override fun getType(): Type = VoidType()
}

data class AstLocation(val file: VfsNode, val node: AstNode)