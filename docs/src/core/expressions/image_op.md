## 取像操作 (expr ' image)

Flurry 引入了一种独特且强大的后缀操作符——**取像操作 (`'`)**，用于获取一个表达式在不同抽象层面或元数据维度的“像 (Image)”。这个操作符提供了一个统一的语法入口，访问与表达式相关的编译时信息、运行时表示或其他关联属性。

### 语法

取像操作的基本语法形式为：

```flurry
expression ' image_identifier
```

*   **`expression`**: 任何有效的 Flurry 表达式。
*   **`'` (单引号)**: 取像操作符。
*   **`image_identifier`**: 一个标识符，指定想要获取的“像”的类型。这个标识符**不是**变量或关键字，而是由 Flurry 编译器内部识别的、用于特定取像操作的名称。

根据您提供的优先级表 (`OpTable`)，取像操作符 `'` 具有很高的优先级 (100)，确保它紧密绑定其左侧的 `expression`。

### 概念与用途

“取像”可以理解为获取表达式 `expression` 的某个特定方面或表示：

1.  **元数据 (Metadata)**: 获取与表达式相关的元信息。
    *   `expression ' type`: 获取表达式在编译时确定的**类型**。结果本身是一个编译时 `Type` 类型的值。
        ```flurry
        let x = 10;
        comptime let type_of_x = x'type; -- type_of_x 的值是代表 i32 (或默认整数类型) 的 Type 对象
        ```
    *   `enum_value ' tag_name`: 获取枚举值当前变体的**名称**（作为一个编译时常量字符串）。
        ```flurry
        enum Status { ok, loading, error }
        let current = Status.loading;
        comptime let name = current'tag_name; -- name 的值是 "loading"
        ```

2.  **抽象表示 (Abstract Representation)**: 获取表达式底层数据的一种抽象视图。
    *   `value ' bitvec`: 获取值 `value` 在内存中的**位向量 (Bit Vector)** 表示。结果是一个编译时可知长度（基于 `value` 的类型）的 `BitVec<N>` 抽象类型，可以用于编译时的位操作（编译器会优化为高效的机器指令）。
        ```flurry
        let flags: u8 = 0b1010_0101;
        comptime let bv = flags'bitvec;
        -- comptime let first_nybble = bv.slice(0..4); -- 假设 bitvec 支持切片
        ```

3.  **关联操作或转换 (Associated Operation / Transformation)**: 触发某种与表达式相关的特殊操作。
    *   `dst_pointer ' dst_deref`: (用于动态大小类型 DST) 这不是获取一个简单的值，而是触发一种特殊的**解引用操作**，允许对 DST 指针指向的内容进行模式匹配或访问，编译器会根据指针指向的实际数据（例如 `tagged_polymorphic` 枚举的 tag）来确定如何解释内存。
    *   `tuple ' enumerate`: (用于元组) 可能返回一个包含 `(元素, 索引)` 对的迭代器或编译时可展开的序列，用于 `inline for`。

### 编译时与运行时

取像操作的结果可能是：

*   **编译时常量**: 如 `'type` 的结果是编译时 `Type`，`'tag_name` 的结果是编译时 `str`。这些结果可以直接用于 `comptime` 代码块和编译时条件判断。
*   **运行时值**: 如 `'bitvec` 得到的是一个抽象表示，但最终的操作会生成运行时代码。`'dst_deref` 也是在运行时执行的特殊解引用。`'enumerate` 可能产生运行时迭代器。
*   **操作触发器**: 某些“像”可能不直接返回值，而是触发编译器执行特定的分析或代码生成过程。

编译器负责根据 `image_identifier` 确定具体的语义和结果类型。

### 内置的 "像"

Flurry 语言会预定义一系列有用的 `image_identifier`。一些我们已经讨论过的或可能的例子包括：

*   `'type`: 获取编译时类型。
*   `'tag_name`: 获取枚举变体名（字符串）。
*   `'bitvec`: 获取位向量抽象视图。
*   `'dst_deref`: 用于 DST 指针的特殊解引用/匹配。
*   `'enumerate`: 用于元组或其他序列的带索引迭代。
*   *（可能还有 `'size` 获取类型大小, `'alignment` 获取对齐, `'address` 获取地址等）*

这个列表是**由语言定义和编译器内置的**，开发者不能自定义新的 `'image_identifier`。

### 设计优势

*   **统一语法**: 为多种不同的元信息查询和底层操作提供了一致的后缀语法。
*   **简洁**: `expr'type` 比 `get_type(expr)` 或 `typeof(expr)` 更简洁。
*   **与编译时系统集成**: 许多“像”直接产生编译时值，无缝融入 `comptime` 计算。
*   **表达力**: 提供了访问语言底层信息和触发特殊操作的强大能力。

### 总结

取像操作 (`expr ' image`) 是 Flurry 中一个富有创新性的特性。它通过一个简洁的后缀操作符 `'` 和编译器内置的 `image_identifier`，提供了一个统一的接口来访问表达式的类型、元数据、抽象表示或触发特殊操作。这是 Flurry 强大的编译时元编程和底层控制能力的又一体现。