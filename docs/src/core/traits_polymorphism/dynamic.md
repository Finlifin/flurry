# 动态多态 (`dyn Trait` vs `tagged_polymorphic`)

动态多态允许在运行时根据对象的实际类型来决定调用哪个方法实现。这对于处理异构集合、实现回调或插件系统等场景非常有用。Flurry 提供了两种主要的动态多态机制，它们各有优劣，适用于不同的场景。

## 1. Trait Object (`dyn Trait`)

Trait Object 是实现动态多态的标准、开放的方式，类似于 Rust 的 `dyn Trait` 或 C++ 的虚函数机制（通过指针或引用）。

**概念:**

-   一个 `dyn TraitName` 类型的值是一个**胖指针 (fat pointer)**。它包含两部分：
    1.  一个指向实际对象数据的指针。
    2.  一个指向该对象类型为 `TraitName` 实现的**虚函数表 (vtable)** 或等效结构（如接口表 itable）的指针。
-   vtable 包含了一系列函数指针，指向该类型为 Trait 定义的每个方法的具体实现。

**用法:**

你可以创建一个 `dyn Trait` 类型胖指针，它可以在运行时指向任何实现了该 Trait 的类型的实例。

```flurry
trait Speaker {
    fn speak(*self);
}

struct Dog { name: String }
impl Speaker for Dog {
    fn speak(*self) { println!("{} says Woof!", self.name); }
}

struct Cat { name: String }
impl Speaker for Cat {
    fn speak(*self) { println!("{} says Meow!", self.name); }
}

test {
    let dog = Dog { .name "Buddy".to_string() }
    let cat = Cat { .name "Whiskers".to_string() }

    -- 创建 Trait Object (通过 Box 指针)
    let animals: Vec<dyn Speaker> = Vec.new();
    animals.push(Box.new(dog).*.dyn(Speaker));
    animals.push(Box.new(cat).*.dyn(Speaker));

    -- 动态调用方法
    for animal in animals.iter() {
        -- animal 是 dyn Speaker 类型
        -- 调用 speak 时，会通过 vtable 查找并执行 Dog 或 Cat 的实现
        animal.speak();
    }
    -- 输出:
    -- Buddy says Woof!
    -- Whiskers says Meow!
}

-- 作为函数参数
fn make_speak(speaker: dyn Speaker) {
    speaker.speak(); -- 动态分派
}
```

**优点:**

-   **开放集合 (Open Set)**: 任何类型，在任何地方（遵守孤儿原则或使用 `extend`），只要实现了 `TraitName`，就可以在运行时被视为 `dyn TraitName`。库可以定义 Trait，使用者可以自由实现并传递给库，扩展性极好。
-   **解耦**: 调用者只需要知道 `dyn Trait` 接口，无需关心具体实现类型。

**缺点:**

-   **运行时开销**:
    -   **间接调用**: 方法调用需要通过 vtable 进行间接查找，比静态分派慢。
    -   **无法内联**: 编译器无法内联 `dyn Trait` 的方法调用。
    -   **胖指针开销**: `dyn Trait` 指针（或引用）本身比普通指针占用更多空间（通常是两倍）。
-   **类型信息部分丢失**: 在 `dyn Trait` 上下文中，对象的具体类型信息在编译时丢失了（虽然运行时可以通过 RTTI 查询，但通常不鼓励）。

## 2. Tagged Polymorphism (基于枚举的半自动多态)

Flurry 提供了一种替代的、基于标签枚举的动态多态机制，通过在 `enum` 定义上使用 `.tagged_polymorphic TraitName` 属性来启用。

**概念:**

-   **载体**: 一个枚举（标记为 `.dst true` 和 `.tagged_polymorphic TraitName`）包含所有需要参与此多态的**固定**类型集合作为其变体。
-   **实现**: 每个枚举变体对应的具体类型（如 `Dog`, `Cat`）必须**分别**实现目标 Trait (`Speaker`)。
-   **分派**: 当通过枚举实例（通常是指针 `*EnumName` 或 `Box<EnumName>`）调用 Trait 方法时，分派逻辑**基于枚举的内部标签 (tag)**。编译器（或运行时）检查标签，确定当前是哪个变体，然后**直接调用**该变体类型对应的 Trait 实现。这通常通过编译时生成的 `match`/`switch` 或跳转表完成。

**用法:**

```flurry
trait Greeter {
    fn greet(*self) -> String;
}

struct EnglishGreeter {}
impl Greeter for EnglishGreeter { fn greet(*self) -> String { "Hello!".to_string() } }

struct SpanishGreeter {}
impl Greeter for SpanishGreeter { fn greet(*self) -> String { "¡Hola!".to_string() } }

-- 定义 Tagged Polymorphic 枚举
enum AnyGreeter {
    .dst true, -- 表明是动态大小
    .tagged_polymorphic Greeter, -- 启用基于标签的多态，目标 Trait 是 Greeter

    english: EnglishGreeter, -- 包含具体类型作为变体
    spanish: SpanishGreeter,
}

test {
    -- 创建实例 (需要通过指针或 Box)
    let greeter1 = Box.new(AnyGreeter.english(EnglishGreeter {}));
    let greeter2 = Box.new(AnyGreeter.spanish(SpanishGreeter {}));

    let greeters: Vec<Box<AnyGreeter>> = Vec.new();
    greeters.push(greeter1);
    greeters.push(greeter2);

    for g in greeters.iter() {
        -- 调用 greet 方法。分派基于 g 指向的 AnyGreeter 实例的 tag
        println!("{}", g.greet());
    }
    -- 输出:
    -- Hello!
    -- ¡Hola!
}

-- 作为函数参数 (注意类型是具体的枚举指针)
fn perform_greeting(g: *AnyGreeter) {
    println!("{}", g.greet()); -- 基于 tag 分派
}
```

**优点:**

-   **潜在性能优势**:
    -   分派后通常是**直接调用**，避免了 vtable 的间接性。
    *   **无 vptr 开销**: 对象本身不存储额外的 vptr。
    -   分支预测可能更友好。
-   **类型信息保留**: 运行时可以轻易检查枚举 tag，获知具体变体类型。

**缺点:**

-   **封闭集合 (Closed Set)**: 最大的限制。所有需要参与多态的类型**必须预先在枚举中定义**。无法在枚举之外添加新的实现类型。
-   **修改不便**: 添加新类型需要修改枚举定义。
-   **适用性**: 主要适用于类型集合已知且稳定的场景。

**`comptime` 的作用**: Flurry 强大的 `comptime` 能力可以缓解封闭集合的问题。库可以提供 `comptime` 元函数，允许使用者在编译时**动态生成**一个包含他们所需类型列表的 `tagged_polymorphic` 枚举，从而在终点代码中获得性能优势，而库本身接口可能仍然使用 `dyn Trait`。

**总结: 何时选择？**

-   **库接口 / 开放扩展性**: 优先选择 **`dyn Trait`**。它提供了必要的灵活性和解耦。
-   **性能关键 / 类型集合固定**: 在应用程序内部或性能瓶颈处，如果涉及的类型集合是已知的、有限的，可以考虑使用 **`tagged_polymorphic` 枚举**以获得潜在的性能提升和更明确的类型信息。
-   **编译时定制**: 结合 `comptime`，可以在编译时生成定制的 `tagged_polymorphic` 枚举，为特定场景提供优化。

Flurry 同时提供这两种机制，让开发者可以根据具体需求在开放性和性能/类型精确性之间做出权衡。