// package comptime

// // 基础 Value trait，所有编译时值的基础接口
// trait Value {
//   // 获取该值的类型
//   def getType: Type = AnyType()

//   // // 编译时值计算方法，可以在编译期求值
//   // def evaluate: Value = this

//   // // 尝试将值转换为特定类型
//   // def castTo(targetType: Type): Option[Value]

//   // // 检查两个值是否相等
//   // def equals(other: Value): Boolean

//   // // 输出人类可读的字符串表示
//   // def toString: String

//   def getNamespace: Option[Definition] = None // 获取命名空间定义
// }

// // // 基本值类型实现

// // // 整数值类型
// // case class IntValue(value: Long, size: Int = 64) extends Value {
// //   override def getType: Type = Int(size)
// //   override def castTo(targetType: Type): Option[Value] = targetType match {
// //     case Int(_) => Some(this)
// //     case Float(_) => Some(FloatValue(value.toDouble))
// //     case Bool() => Some(BoolValue(value != 0))
// //     case String() => Some(StringValue(value.toString))
// //     case _ => None
// //   }
// //   override def equals(other: Value): Boolean = other match {
// //     case IntValue(otherVal, _) => value == otherVal
// //     case _ => false
// //   }
// //   override def toString: String = value.toString

// //   // 整数运算操作
// //   def +(other: IntValue): IntValue = IntValue(value + other.value)
// //   def -(other: IntValue): IntValue = IntValue(value - other.value)
// //   def *(other: IntValue): IntValue = IntValue(value * other.value)
// //   def /(other: IntValue): IntValue = IntValue(value / other.value)
// //   def %(other: IntValue): IntValue = IntValue(value % other.value)
// //   def &(other: IntValue): IntValue = IntValue(value & other.value)
// //   def |(other: IntValue): IntValue = IntValue(value | other.value)
// //   def ^(other: IntValue): IntValue = IntValue(value ^ other.value)
// //   def <<(other: IntValue): IntValue = IntValue(value << other.value)
// //   def >>(other: IntValue): IntValue = IntValue(value >> other.value)
// // }

// // // 浮点数值类型
// // case class FloatValue(value: Double, size: Int = 64) extends Value {
// //   override def getType: Type = Float(size)
// //   override def castTo(targetType: Type): Option[Value] = targetType match {
// //     case Float(_) => Some(this)
// //     case Int(_) => Some(IntValue(value.toLong))
// //     case Bool() => Some(BoolValue(value != 0))
// //     case String() => Some(StringValue(value.toString))
// //     case _ => None
// //   }
// //   override def equals(other: Value): Boolean = other match {
// //     case FloatValue(otherVal, _) => value == otherVal
// //     case _ => false
// //   }
// //   override def toString: String = value.toString

// //   // 浮点数运算
// //   def +(other: FloatValue): FloatValue = FloatValue(value + other.value)
// //   def -(other: FloatValue): FloatValue = FloatValue(value - other.value)
// //   def *(other: FloatValue): FloatValue = FloatValue(value * other.value)
// //   def /(other: FloatValue): FloatValue = FloatValue(value / other.value)
// // }

// // // 布尔值类型
// // case class BoolValue(value: Boolean) extends Value {
// //   override def getType: Type = Bool()
// //   override def castTo(targetType: Type): Option[Value] = targetType match {
// //     case Bool() => Some(this)
// //     case Int(_) => Some(IntValue(if (value) 1 else 0))
// //     case String() => Some(StringValue(value.toString))
// //     case _ => None
// //   }
// //   override def equals(other: Value): Boolean = other match {
// //     case BoolValue(otherVal) => value == otherVal
// //     case _ => false
// //   }
// //   override def toString: String = value.toString

// //   // 布尔逻辑运算
// //   def &&(other: BoolValue): BoolValue = BoolValue(value && other.value)
// //   def ||(other: BoolValue): BoolValue = BoolValue(value || other.value)
// //   def unary_! : BoolValue = BoolValue(!value)
// // }

// // // 字符串值类型
// // case class StringValue(value: String) extends Value {
// //   override def getType: Type = String()
// //   override def castTo(targetType: Type): Option[Value] = targetType match {
// //     case String() => Some(this)
// //     case _ => None
// //   }
// //   override def equals(other: Value): Boolean = other match {
// //     case StringValue(otherVal) => value == otherVal
// //     case _ => false
// //   }
// //   override def toString: String = value

// //   // 字符串操作
// //   def +(other: StringValue): StringValue = StringValue(value + other.value)
// //   def length: IntValue = IntValue(value.length)
// //   def slice(start: IntValue, end: IntValue): StringValue =
// //     StringValue(value.substring(start.value.toInt, end.value.toInt))
// // }

// // // 数组值类型
// // case class ArrayValue(elements: List[Value], elementType: Type) extends Value {
// //   override def getType: Type = ArrayType(elementType, elements.size)
// //   override def castTo(targetType: Type): Option[Value] = targetType match {
// //     case ArrayType(ty, size) if size == elements.size && ty == elementType => Some(this)
// //     case _ => None
// //   }
// //   override def equals(other: Value): Boolean = other match {
// //     case ArrayValue(otherElements, _) => elements.size == otherElements.size && elements.zip(otherElements)
// //         .forall { case (a, b) => a.equals(b) }
// //     case _ => false
// //   }
// //   override def toString: String = elements.mkString("[", ", ", "]")

// //   // 数组操作
// //   def length: IntValue = IntValue(elements.size)
// //   def get(index: IntValue): Value = elements(index.value.toInt)
// // }

// // // 元组值类型
// // case class TupleValue(elements: List[Value]) extends Value {
// //   override def getType: Type = TupleType(elements.map(_.getType))
// //   override def castTo(targetType: Type): Option[Value] = targetType match {
// //     case TupleType(types) if types.size == elements.size =>
// //       val castedElements = elements.zip(types).map { case (el, ty) => el.castTo(ty) }
// //       if (castedElements.forall(_.isDefined)) { Some(TupleValue(castedElements.map(_.get))) }
// //       else { None }
// //     case _ => None
// //   }
// //   override def equals(other: Value): Boolean = other match {
// //     case TupleValue(otherElements) => elements.size == otherElements.size && elements.zip(otherElements)
// //         .forall { case (a, b) => a.equals(b) }
// //     case _ => false
// //   }
// //   override def toString: String = elements.mkString("(", ", ", ")")

// //   // 元组操作
// //   def get(index: IntValue): Value = elements(index.value.toInt)
// // }

// // // 函数值类型 - 用于编译时计算的函数
// // case class FunctionValue(parameters: List[(String, Type)], body: (List[Value]) => Value, returnType: Type)
// //     extends Value {
// //   override def getType: Type = FunctionType(
// //     parameters.map { case (name, ty) => ??? }, // 需要从参数创建适当的 Value
// //     ???, // 需要从 returnType 创建适当的 Value
// //     ??? // 需要提供 FunctionProperties
// //   )

// //   override def castTo(targetType: Type): Option[Value] = targetType match {
// //     case _: FunctionType => Some(this) // 可能需要更复杂的检查
// //     case _ => None
// //   }

// //   override def equals(other: Value): Boolean = this eq other // 函数值比较引用相等性

// //   override def toString: String = s"function(${parameters.map(_._1).mkString(", ")})"

// //   // 调用函数
// //   def apply(args: List[Value]): Value = body(args)
// // }

// // // 符号值类型
// // case class SymbolValue(name: String) extends Value {
// //   override def getType: Type = SymbolType()
// //   override def castTo(targetType: Type): Option[Value] = targetType match {
// //     case SymbolType() => Some(this)
// //     case String() => Some(StringValue(name))
// //     case _ => None
// //   }
// //   override def equals(other: Value): Boolean = other match {
// //     case SymbolValue(otherName) => name == otherName
// //     case _ => false
// //   }
// //   override def toString: String = s"'$name"
// // }

// // // 结构体实例值
// // case class StructValue(structType: Struct, fields: Map[String, Value]) extends Value {
// //   override def getType: Type = structType
// //   override def castTo(targetType: Type): Option[Value] = targetType match {
// //     case st: Struct if st == structType => Some(this)
// //     case _ => None
// //   }
// //   override def equals(other: Value): Boolean = other match {
// //     case StructValue(otherType, otherFields) => structType == otherType && fields.keys == otherFields.keys &&
// //       fields.forall { case (k, v) => v.equals(otherFields(k)) }
// //     case _ => false
// //   }
// //   override def toString: String = s"${structType.toString}{${fields.map { case (k, v) => s"$k: $v" }.mkString(", ")}}"

// //   // 结构体字段访问
// //   def get(fieldName: String): Option[Value] = fields.get(fieldName)
// // }

// // // 空值类型
// // case object VoidValue extends Value {
// //   override def getType: Type = Void()
// //   override def castTo(targetType: Type): Option[Value] = targetType match {
// //     case Void() => Some(this)
// //     case _ => None
// //   }
// //   override def equals(other: Value): Boolean = other == VoidValue
// //   override def toString: String = "void"
// // }

// // // 类型值 - 表示类型本身作为编译时的值
// // case class TypeValue(typeValue: Type) extends Value {
// //   override def getType: Type = ??? // 这里需要一个表示"类型的类型"的概念
// //   override def castTo(targetType: Type): Option[Value] = None // 类型值通常不能转换为其他值
// //   override def equals(other: Value): Boolean = other match {
// //     case TypeValue(otherType) => typeValue == otherType
// //     case _ => false
// //   }
// //   override def toString: String = s"typeof(${typeValue.toString})"
// // }

// // // 编译时计算错误值
// // case class ErrorValue(message: String) extends Value {
// //   override def getType: Type = ErrorQualifiedType(List(this), Void())
// //   override def castTo(targetType: Type): Option[Value] = None // 错误值不能转换
// //   override def equals(other: Value): Boolean = other match {
// //     case ErrorValue(otherMessage) => message == otherMessage
// //     case _ => false
// //   }
// //   override def toString: String = s"error($message)"
// // }

// // // 编译时计算引擎，用于执行编译时表达式
// // object ComptimeEvaluator {
// //   def evaluate(expr: Any): Value = ??? // 实现编译时表达式的求值

// //   // 二元操作符执行
// //   def executeBinaryOp(left: Value, right: Value, op: String): Value = ???

// //   // 一元操作符执行
// //   def executeUnaryOp(value: Value, op: String): Value = ???
// // }
