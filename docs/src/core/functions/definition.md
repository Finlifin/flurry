# 定义与调用 (Definition & Invocation)

## 函数定义 (Function Definition)

使用 `fn` 关键字定义一个函数。其基本语法结构如下：

```flurry
pub? (comptime|async|...)? fn function_name(parameter*) ReturnType? clause? block
```
**示例:**

```flurry
-- 简单的加法函数
fn add(a: i32, b: i32) -> i32 {
    a + b -- 函数体最后是表达式，隐式返回其值
}

-- 无返回值的函数
pub fn print_greeting(name: String) { -- 等同于 -> void
    println("Hello, {}!", name);
}

-- print_value: for<T:- Display> fn(T)
fn print_value(value: T) where T:- Display {
    println("Value: {}", value);
}
```

## 函数调用 (Function Invocation)

调用函数使用函数名，后跟圆括号 `()`，括号内提供实际参数（也称为实参，arguments）。

```flurry
let sum = add(5, 3); -- 调用 add 函数，传递 5 和 3 作为参数
print_greeting("Flurry".to_string()); -- 调用 print_greeting

-- 调用泛型函数时，编译时参数可能需要显式提供或由编译器推断
-- print_value<i32>(10);
print_value(10); -- 编译器通常可以推断 T 为 i32
```

参数传递的规则取决于函数签名中参数的类型（详见 [参数详解](parameters.md)）。

## 方法 (Methods)

函数也可以直接**关联**到某个类型（如 `struct`, `enum`, `union`, `mod`）上。定义在类型内部的函数称为**关联函数**。

**定义:**

关联函数直接在类型定义的 `{}` 内部使用 `fn` 定义。

```flurry
struct Point {
    x: f64,
    y: f64,

    pub fn origin() -> Self { -- Self 是当前类型的别名
        Point { .x 0.0, .y 0.0 }
    }

    -- 通常称为“方法”
    pub fn distance_from_origin(*self) -> f64 {
        -- self 指向调用该方法的实例
        math.sqrt(self.x * self.x + self.y * self.y)
    }

    -- 接收可变引用的方法
    pub fn translate(*self, dx: f64, dy: f64) {
        self.x += dx;
        self.y += dy;
    }

     -- 接收所有权的方法
    -- pub fn consume(self) { {- ... -} }
}
```

**调用:**

```flurry
let p1 = Point.origin(); -- 调用静态方法 origin
let p2 = Point { .x 3.0, .y 4.0 }

let dist = p2.distance_from_origin(); -- 调用实例方法 distance_from_origin
println("Distance: {}", dist);       -- 输出: Distance: 5.0

let p3 = Point.origin();
p3.translate(1.0, 2.0); -- 调用可变实例方法 translate
println("Translated: ({}, {})", p3.x, p3.y); -- 输出: Translated: (1.0, 2.0)
```

方法调用 `instance.method(args...)` 通常是 `TypeName.method(instance.ref, args...)`等形式的语法糖。

**实现块 (`impl`) 中的方法:**

除了直接在类型定义中，也可以在单独的 `impl TypeName { ... }` 或 `impl TraitName for TypeName { ... }` 块中为类型定义函数、子成员等。

```flurry
struct Counter { value: i32 }

-- 在 impl 块中为 Counter 添加方法
impl Counter {
    fn new() -> Self { Counter { .value 0 } }
    fn increment(*self) { self.value += 1; }
    fn get(*self) -> i32 { self.value }
}

test {
    let c = Counter.new();
    c.increment();
    println("Count: {}", c.get()); -- 输出: Count: 1
}
```

这种方式有助于将类型的定义和其行为实现分开