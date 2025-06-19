# Trait 定义与实现 (impl, derive)

Trait 用于定义一组类型可以共享的方法签名。它描述了某种抽象行为或能力，而不涉及具体的实现。

**定义 Trait**

使用 `trait` 关键字定义一个 Trait，并在其花括号 `{}` 内声明方法签名。方法签名指定了方法名、参数（包括 `self` 参数）和返回类型，但没有具体实现。

```flurry
-- 定义一个描述可被绘制行为的 Trait
trait Drawable {
    -- 方法签名，需要一个不可变引用指向实例
    fn draw(*self, canvas: *Canvas);
}

-- 定义一个可以计算面积的 Trait
trait Area {
    fn area(*self) -> f64;
}

-- Trait 也可以包含关联类型或常量 (如果 Flurry 支持)
trait Container {
    -- type Item; -- (假设) 关联类型
    -- const DEFAULT_CAPACITY: usize; -- (假设) 关联常量
    fn add(*mut self, item: Item); -- 使用关联类型
}
```

**实现 Trait (`impl`)**

要让一个具体类型（如 `struct` 或 `enum`）拥有 Trait 定义的行为，你需要为该类型**实现 (implement)** 这个 Trait。使用 `impl TraitName for TypeName` 语法，并在花括号内提供 Trait 中必要方法的具体实现。

```flurry
struct Circle { radius: f64 }
struct Rectangle { width: f64, height: f64 }

-- 为 Circle 实现 Area Trait
impl Area for Circle {
    fn area(*self) -> f64 {
        3.1415926535 * self.radius * self.radius
    }
}

-- 为 Rectangle 实现 Area Trait
impl Area for Rectangle {
    fn area(*self) -> f64 {
        self.width * self.height
    }
}

-- (假设 Drawable 和 Canvas 已定义)
impl Drawable for Circle {
    fn draw(*self, canvas: *Canvas) {
        -- ... 具体绘制圆形的逻辑 ...
        println("Drawing a circle with radius {}", self.radius);
    }
}

impl Drawable for Rectangle {
    fn draw(*self, canvas: *Canvas) {
        -- ... 具体绘制矩形的逻辑 ...
        println("Drawing a rectangle {}x{}", self.width, self.height);
    }
}

test {
    let c = Circle { .radius 5.0 }
    let r = Rectangle { .width 4.0, .height 6.0 }

    println("Circle area: {}", c.area());   -- 调用 Circle 的 area 实现
    println("Rectangle area: {}", r.area()); -- 调用 Rectangle 的 area 实现
}
```

**孤儿原则 (Orphan Rule)**

为了保证全局实现的一致性，Flurry（类似 Rust）遵循**孤儿原则**：

> 对于一个 Trait `T` 和一个类型 `Type`，`impl T for Type` 这个实现必须定义在**定义 `T` 的那个包**中，或者定义在**定义 `Type` 的那个包**中。不允许在完全独立的第三方包中为外部定义的 Trait 和外部定义的 Type 提供实现。

这个规则确保了任何 `impl` 都有一个明确的“负责人”，防止不同的库对同一对 Trait/Type 提供冲突的实现。

**自动派生 (`derive`)**

对于一些常见的、具有标准实现模式的 Trait（例如，用于调试输出的 `Debug`、用于比较的 `Eq`/`Ord`、用于复制的 `Copy`/`Clone` 等），Flurry 可能提供 `derive` 属性，让编译器自动为你的类型生成这些 Trait 的实现代码。

```flurry
struct Point {
    x: i32,
    y: i32,
}

derive Debug, Clone, Copy for Point;

test {
    let p1 = Point { .x 1, .y 2 }
    let p2 = p1.clone();
    println("Debug point: {debug}", p2);
}
```
`derive` 大大减少了编写样板实现代码的工作量。可派生的 Trait 通常由语言或标准库预定义。

Trait 和 `impl` 构成了 Flurry 静态抽象和代码共享的基础。通过它们，你可以编写泛型代码，约束泛型参数必须实现特定的 Trait，从而在编译时实现类型安全的多态。
