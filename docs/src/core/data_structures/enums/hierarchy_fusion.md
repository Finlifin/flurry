# 层级化与融合
Flurry 的枚举 (`enum`) 不仅仅是像 C 或 Java 中那样简单的标签列表，也不完全等同于 Rust 中强大的代数数据类型 (ADT)。Flurry 在枚举的设计上引入了两个极具特色的创新：**层级化定义 (Hierarchical Definition)** 和 **枚举融合 (Enum Fusion)**。这两个特性相互配合，极大地增强了枚举的组织能力和组合性，尤其是在处理复杂状态空间或像错误类型这样需要组合的场景时。

**1. 层级化枚举：给变体一个“家谱”**

想象一下，你在为编译器定义抽象语法树 (AST) 节点类型。你可能会有各种二元操作符（加、减、乘、除、与、或、比较等）、一元操作符（取反、负号）、字面量（整数、浮点数、字符串）等等。如果把它们都平铺在一个巨大的枚举里，列表会很长，而且逻辑分组也不明显。

Flurry 的**层级化枚举**允许你像组织文件目录一样来组织枚举的变体：

```flurry
enum AstNodeKind {
    -- 使用 '.' 来创建层级
    unary.{ -- 'unary' 组
        bool_not, -- 路径是 unary.bool_not
        neg,      -- 路径是 unary.neg
    },
    binary.{ -- 'binary' 组
        add, sub, mul, div, -- 直接成员：binary.add 等
        bool.{ -- 'binary' 下的 'bool' 子组
            and, -- 路径是 binary.bool.and
            or,
            eq, ne, lt, le, gt, ge, -- binary.bool.eq 等
        },
        -- 可以继续嵌套其他二元操作符分组...
    },
    literal.{ -- 'literal' 组
        int, float, string, char, -- literal.int 等
    },
    variable_ref, -- 顶层变体
    function_call, -- 顶层变体
    -- ... 其他节点类型 ...
}

-- 如何使用？
test {
    let node_type = AstNodeKind.binary.bool.eq; -- 使用完整的层级路径访问变体

    if node_type is {
        -- 可以在模式匹配中使用层级
        .binary.bool.eq => println("Equality comparison"),
        -- 使用通配符匹配整个分组
        .binary.bool.* => println("Some boolean binary operation"),
        .unary.* => println("Some unary operation"),
        .literal.int => println("Integer literal"),
        _ => println("Other node type"),
    }
}
```

*   **语法**: 使用点 `.` 和花括号 `{}` 来创建层级。点号用于分隔层级名称，花括号用于包含子层级或同一层级的多个变体。
*   **优点**:
    *   **组织性**: 极大地提高了大型枚举的可读性和可维护性。
    *   **命名空间**: 天然地为变体提供了命名空间，减少命名冲突（例如，可以有 `binary.add` 和 `vector.add` 而不冲突）。
    *   **逻辑分组**: 枚举的结构可以直接反映概念上的分类。
    *   **模式匹配增强**: 可以在 `match` 或 `if is` 中使用层级路径和通配符 (`*`) 进行更结构化的匹配。
*   **本质**: 尽管看起来有层级，但在底层实现中，每个最终的变体（如 `binary.bool.and`）仍然是一个唯一的标签或标识符。层级化主要体现在**语法层面**的组织和访问方式上。

**2. 枚举融合：将不同的世界合并**

现在，假设不同的库或模块定义了各自的枚举，比如网络库定义了 `NetErr`，文件系统库定义了 `FsErr`。在某个函数中，你可能同时遇到这两种错误。你需要一种方法来统一处理它们，但又不想手动创建一个包含所有可能错误的新枚举（这很繁琐且难以维护）。

这就是**枚举融合 (Enum Fusion)** 发挥作用的地方。Flurry 允许你在编译时将多个独立的枚举**“融合”**成一个新的、统一的枚举类型。

```flurry
-- --- 在库 A 中定义 ---
mod lib_a {
    pub enum ErrorA {
        Timeout,
        ConnectionFailed,
    }

    derive Eq for ErrorA;
}

-- --- 在库 B 中定义 ---
mod lib_b {
    pub enum ErrorB {
        NotFound,
        PermissionDenied,
    }

    derive Eq for ErrorB;
}

-- --- 在使用代码中融合 ---
use lib_a.ErrorA;
use lib_b.ErrorB;

-- 使用编译时元函数 (假设语法) 创建一个新的融合枚举类型 C
-- C 现在包含了来自 ErrorA 和 ErrorB 的所有变体，并保留了它们的“来源”信息
-- 自动实现相关Into与From
newtype C = meta.Enum.from([ErrorA, ErrorB]);
derive Eq for C;

-- 演示融合后的类型
fn might_fail_combined() -> C {
    if condition1 {
        ErrorA.Timeout.into()
    } else if condition2 {
        ErrorB.NotFound.into();
    } else {
        -- ... 其他情况 ...
    }
}

test {
    let result: C = might_fail_combined();

    -- 可以使用原始枚举的层级路径 (加上来源) 来匹配融合后的枚举
    if result is {
        .ErrorA.Timeout => println("Got timeout from Lib A"),
        .ErrorA.ConnectionFailed => println("Got connection failed from Lib A"),

        .ErrorB.NotFound => println("Got not found from Lib B"),
        .ErrorB.PermissionDenied => println("Got permission denied from Lib B"),

        -- 也可以用通配符匹配来自特定源的所有错误
        .ErrorA.* => println("Some other error from Lib A"),
        .ErrorB.* => println("Some other error from Lib B"),
    }

    -- 验证身份：融合后的值与原始值不同
    let original_a = ErrorA.Timeout;
    let fused_a: C = original_a.into();
    asserts original_a != fused_a;
}
```

*   **机制**: 通过一个编译时计算接收一个枚举类型列表，并在**编译时生成一个新的枚举类型**。
*   **保留来源**: 生成的融合枚举不仅包含所有源枚举的变体，还**保留了每个变体来自哪个源枚举的信息**。这体现在访问和匹配融合枚举时，通常需要带上源枚举的名称或路径（如 `.ErrorA.Timeout`）。
*   **类型转换**: 提供了从源枚举类型到融合枚举类型的转换方法（如 `.into()`）。
*   **组合性**: 极大地提高了代码的组合性。不同的模块可以独立定义自己的枚举，然后在需要统一处理的地方将它们融合起来，而**无需修改原始定义**。
*   **错误处理应用**: 枚举融合在错误处理 (`![Err1, Err2] T`) 中的应用最为典型。编译器自动为你执行这种融合，创建匿名的错误联合类型来无缝地处理来自不同源的错误，并能通过层级路径或通配符精确匹配。

**层级化与融合的协同:**

这两个特性可以完美结合。如果源枚举本身就是层级化的，那么融合后的枚举也会保留这种层级结构，只是在最外层增加了一个表示来源的层级。例如，如果 `ErrorA` 有 `.network.timeout`，融合后可能匹配 `.ErrorA.network.timeout`。

**总结:**

Flurry 通过**层级化枚举**提供了强大的内部组织能力，使得大型枚举易于管理和理解。通过**枚举融合**，它又提供了无与伦比的外部组合能力，允许在不修改原始定义的情况下合并不同的枚举类型。这两个特性共同作用，特别是在构建复杂的错误类型、状态机、AST 节点等方面，提供了极大的便利性和表达力，是 Flurry 类型系统设计中的一大亮点。
