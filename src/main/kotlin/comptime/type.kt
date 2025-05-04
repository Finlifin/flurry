package comptime

// 基础类型接口
interface Type

// 基本类型实现
class AnyType : Type
class VoidType : Type
class TypeType : Type
class TraitType : Type
class ImplementationType(val forType: Type, val withTrait: Value?) : Type
class ExtensionType(val forType: Type, val withTrait: Value?) : Type

// 声明类
abstract class Declaration