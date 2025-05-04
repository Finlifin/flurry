package comptime

// 基础 Value 接口，所有编译时值的基础接口
interface Value {
    // 获取该值的类型
    fun getType(): Type = AnyType()

    // 获取命名空间定义
    fun getNamespace(): Definition? = null
}