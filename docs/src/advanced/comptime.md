## 编译时计算 (`comptime`)

Flurry 的一个核心且强大的特性是**编译时计算 (Compile-Time Computation)**，通常以关键字 `comptime` 体现。该机制允许开发者在编译阶段执行代码，从而实现高级别的元编程、优化和类型安全保证。

**Flurry 的两级计算模型**

理解 `comptime` 的基础在于认识 Flurry 的**两级计算模型 (Two-Level Computation Model)**，该模型明确区分了两个不同的执行阶段：

1.  **编译时 (Comptime)**: 编译器工作的阶段。在此阶段，Flurry 不仅执行传统的编译任务（如语法分析、类型检查、代码生成），还提供了一个功能强大的**编译时运行时 (Comptime Runtime)**，用于执行标记为 `comptime` 的代码。
2.  **运行时 (Runtime)**: 编译产物（即可执行程序）被最终用户执行的阶段。

这种设计使得许多传统上必须在运行时完成的操作得以在编译时执行。其主要优势包括：实现零成本抽象、提升运行时性能、增强类型安全以及提供强大的元编程能力。

**编译时与运行时上下文的区分**

准确识别代码执行的上下文至关重要：

*   **运行时上下文 (Runtime Context)**:
    *   常规函数（未使用 `comptime` 标记的 `fn`）的函数体内部。
    *   传递给常规函数的运行时参数的值。
*   **编译时上下文 (Comptime Context)**:
    *   **所有类型表达式 (`Type`) 和 Trait 表达式**: 类型和 Trait 在 Flurry 中是一等公民，它们在编译时被定义、操作和求值。
    *   **所有属性 (`.` 或 `^`)**: 附加到类型、函数、字段等的属性，其求值发生在编译时。
    *   **顶级命名空间 (Top-level Namespace) 中的定义**: 全局作用域下的声明（如 `const`, `fn`, `struct` 定义）通常在编译时处理。
    *   **常量 (`const`) 的初始化表达式**: `const` 声明要求其值必须在编译时确定。
    *   **`comptime` 函数的参数和函数体**: 标记为 `comptime` 的函数及其参数完全在编译时处理和执行。
    *   **常规函数的 `comptime` 参数**: 常规函数可以接收 `comptime` 参数，这些参数的值在编译时确定并传入。
    *   **特定的语法结构**: 某些语法结构，如 `pattern_from_expr` 中的 `<expr>` 部分、结构体字段的默认值表达式等，要求其表达式必须是编译时可求值的。
    *   **`inline` 控制流结构的条件/控制部分**: 对于 `inline if`, `inline for`, `inline when` 等结构，其**条件表达式**或**迭代控制逻辑**在编译时求值。其**内部代码块**的执行上下文（编译时或运行时）则取决于 `inline` 结构自身所处的上下文。

**Flurry 结构：作为编译时值的语言构件**

一个核心概念是：在 Flurry 的编译时环境中，许多语言的基本构件——例如**结构体定义 (`struct`)、函数定义 (`fn`)、枚举定义 (`enum`)、Trait 定义 (`trait`)** 等——本身被视为**编译时值 (comptime values)**。

这意味着这些定义不仅是声明，更是可以在编译时被操作和传递的数据。

示例：

```flurry
const Student = struct {
    name: String,
    age: u8,
}

-- greet: pure comptime fn<T:- ToString> -> fn(T) -> String,
-- 可简写为 for<T:- ToString> fn(T) -> String
fn greet(name: T) -> String where T:- ToString {
    "Hello, " + name.to_string()
}

```

Flurry 可能还提供更底层的编译时 API，用于以编程方式构建这些语言构件：

```flurry
-- 使用假设的编译时 API 动态构建结构体类型
let DynamicStruct = Type.Struct.new(
    .fields = { -- 一个编译时 object (异构映射)
        .id    { .type Uuid }, -- 字段名和类型
        .value { .type str, .default "empty" }, -- 包含默认值
    }
);
-- 在编译时对生成的类型进行内省 (Introspection)
let fields = Type.Struct.fields(DynamicStruct); -- 获取字段信息
assert(fields[1].get(.name, String)? == "value"); -- 安全地获取字段名
assert(fields[1].get(.default, String)? == "empty"); -- 安全地获取默认值
```

这种将语言构件视为编译时值的范式，使 `comptime` 超越了简单的常量折叠，成为一个强大的**类型和代码生成引擎**。

**编译时类型系统的特性：依赖类型与渐进类型**

Flurry 的编译时类型系统具备两个关键特性，增强了其表达能力和灵活性：

1.  **依赖类型 (Dependent Types)**: 函数（或其他接受参数的结构）的**后续参数类型**可以**依赖于先前 `comptime` 参数的值**。这允许创建高度灵活且类型安全的接口。`println` 函数是典型的例子，其可变参数 `varargs` 的具体类型由 `comptime` 参数 `format` 字符串的值在编译时确定。

    ```flurry
    -- varargs的类型是一个元组，从fmt.Parse(format)求值而出
    fn println(comptime format: str, ...varargs: fmt.Parse(format)) {
        ...
        inline for v in varargs {
            const T = v'type;
            inline when {
                T: str => stdio.write_all(v),
                T:- Display => stdio.write_all(c.to_string().as_str()),
                T: usize => stdio.write_all(fmt.format_int(v)),
                ...
            }
        }
    }
    ```

2.  **渐进类型 (Gradual Typing)**: 编译时的容器类型，如 `object`（异构键值对）和 `List<Any>`（异构列表），允许存储**不同类型的值**。Flurry 提供了渐进类型的访问机制来处理这种异构性：
    *   **类型安全的获取 (`.get(key, ExpectedType: Type) -> ?ExpectedType`)**: 尝试获取指定键且类型为 `ExpectedType` 的值。

    ```flurry
        let config = { .port 8080, .host "localhost", .enable_tls true } -- 编译时 object

        -- 类型安全获取
        let port: ?i32 = config.get(.port, i32);
        assert(port? == 8080);
        let host: ?String = config.get(.host, String);
        assert(host? == "localhost");
        let timeout: ?u64 = config.get(.timeout, u64);
        assert(timeout == null);
    ```
    渐进类型为编译时处理结构未知或异构的数据提供了必要的灵活性。

**编译时与运行时的交互机制**

尽管编译时和运行时是不同的阶段，Flurry 提供了明确的机制允许它们交互：

*   **沉降 (Lowering)**: 指将**编译时可知的值**作为常量嵌入到运行时代码中作为运行时值的过程。这通常是隐式发生的。例如，在运行时代码 `let x = 10;` 中，字面量 `10` 是一个编译时整数值，被沉降为运行时的整数值。
*   **`comptime { ... }` 块**: 允许在**运行时上下文**中嵌入一段需要在**编译时**执行的代码。此代码块通常用于计算一个编译时值，该值随后可以通过沉降被外围的运行时代码使用。

    ```flurry
    fn get_platform_info() -> String {
        -- os_name 在此被沉降为运行时字符串常量
        let os_name: str = inline if build.target.os == .linux {
            "Linux" -- 编译时字符串
        } else if build.target.os == .windows {
            "Windows" -- 编译时字符串
        } else {
            "Unknown OS" -- 编译时字符串
        }

        fmt.format("Running on: {}", os_name)
    }
    ```

*   **`inline` 控制流 (`inline if`/`when`/`for`/`match`)**: 这些是关键的**编译时控制流**结构。它们根据编译时条件或数据，在编译阶段选择性地生成、省略或重复代码片段。
    *   `inline if`/`when`: 用于条件编译，根据编译时布尔表达式或类型匹配决定包含哪个代码分支。
    *   `inline for`: 用于编译时循环展开（例如，遍历编译时已知的数组或元组）或根据编译时集合生成代码。


**总结：`comptime` 的核心价值**

Flurry 的 `comptime` 机制提供了一个功能完备的编译时编程环境，其核心特征包括：

*   **两级计算模型**: 清晰分离编译时与运行时。
*   **语言构件作为编译时值**: 类型、函数等可在编译时被操作。
*   **依赖类型与渐进类型**: 增强了编译时类型系统的表达力和灵活性。
*   **编译时控制流 (`inline`)**: 实现代码生成和特化。
*   **类型谓词**: 提供强大的编译时约束能力。

通过 `comptime`，Flurry 实现了：

*   **零成本抽象**: 高级编程构造在编译时被解析和优化，不引入运行时开销。
*   **强大的元编程能力**: 支持在编译阶段生成代码、构建类型、处理配置等。
*   **增强的类型安全**: 允许在编译时执行更深入的检查（例如，格式化字符串的类型安全验证）。
*   **潜在的性能优化**: 通过编译时特化、常量计算和代码生成，可以产出高度优化的运行时代码。

掌握 `comptime` 是充分利用 Flurry 语言潜力的关键，它为开发者提供了在编译阶段深度塑造程序结构和行为的能力。
