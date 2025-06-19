# 组合与委托 (`using`)

面向对象编程中，通常推荐“组合优于继承”。组合是指一个类型通过包含其他类型的实例来复用其功能。然而，简单组合后，外部类型需要手动编写大量的“转发”或“委托”方法才能将内部对象的功能暴露出来。

Flurry 提供了 `using` 关键字，作为一种便捷的语法糖，用于将结构体内部字段的**公开 (public)** 成员（方法和可能的字段）自动“提升”或“委托”到外部结构体的接口上。

**基本语法:**

在 `struct` 定义内部使用：

```flurry
using <field_name>.*;
```

-   `<field_name>`: 必须是当前结构体的一个字段。
-   `.*`: 表示将该字段类型的所有**公开**成员引入到当前结构体的接口中。

**示例:**

```flurry
struct Engine {
    horsepower: u32,
    pub fn start(*self) { println("Engine started!"); {- ... -} }
    pub fn stop(*self) { println("Engine stopped."); {- ... -} }
}

struct Transmission {
    gear_count: u8,
    pub fn shift_up(*self) { {- ... -} println("Shifted up."); }
    pub fn shift_down(*self) { {- ... -} println("Shifted down."); }
}

struct Car {
    engine: Engine,
    transmission: Transmission,
    color: String,

    -- 将 engine 的所有 pub 方法 (start, stop) 引入 Car 的接口
    using engine.*;
    -- 将 transmission 的所有 pub 方法 (shift_up, shift_down) 引入 Car 的接口
    using transmission.*;

    -- Car 也可以有自己的方法
    pub fn drive(*self) {
        self.start(); -- 直接调用来自 Engine 的 start 方法
        println("Driving a {} car.", self.color);
    }
}

test {
    let my_car = Car {
        .engine Engine { .horsepower 200 },
        .transmission Transmission { .gear_count 6 },
        .color "Red".to_string(),
    }

    my_car.drive();      -- 调用 Car 自己的方法，内部调用了 engine.start()
    my_car.shift_up();   -- 直接调用来自 Transmission 的方法
    my_car.stop();       -- 直接调用来自 Engine 的方法
}
-- 可能输出:
-- Engine started!
-- Driving a Red car.
-- Shifted up.
-- Engine stopped.
```

**工作机制:**

`using field.*` 并不实际复制成员。它更像是在编译时为外部结构体自动生成了转发方法（对于方法）或代理访问（对于字段，如果字段也是 pub 的话）。当调用 `my_car.start()` 时，编译器知道 `start` 来自 `engine` 字段，并生成调用 `my_car.engine.start()` 的代码。

**优点:**

-   **减少样板代码**: 极大地简化了将内部组件功能暴露给外部的过程。
-   **促进组合**: 使基于组合的设计模式更加易于实现和使用。
-   **接口清晰 (某种程度上)**: 调用点的代码更简洁。

**注意事项与潜在问题:**

-   **命名冲突**: 如果 `using` 了多个字段，并且它们导出的成员有同名，或者外部结构体本身定义了同名成员，Flurry编译器将会报错并拒绝编译。解决方法是使用 `using field.{member1, member2}` 语法来选择性导出成员，或者使用不同的名称。
-   **可读性**: 调用者可能需要查看结构体定义中的 `using` 语句才能确定某个方法或字段的实际来源。IDE 的支持对于缓解这个问题很重要。
-   **选择性导出**: 语法 `using field.*` 导出了所有公开成员。flurry还支持更精细的控制，例如 `using field.{member1, method2}`, 这可以减少不必要的接口暴露和命名冲突。
-   **访问权限**: `using` 只导出源字段类型的**公开 (public)** 成员。

**总结**

`using` 关键字是 Flurry 提供的一种简化组合模式的有效工具。它通过自动委托内部对象的公开成员，减少了样板代码，使得基于组合的设计更加便捷。在使用时，需要注意潜在的命名冲突和对代码可读性的影响，并依赖语言清晰的规则来管理这些问题。