# 符号

## 符号 (Symbols)

符号 (Symbol) 是 Flurry 语言中一种独特的**编译时常量类型**，它提供了一种表示**唯一标识符**的轻量级方式。符号在 Flurry 的元编程、属性系统以及某些 DSL 构建场景中扮演着重要角色。

### 语法

符号字面量以一个点 `.` 开始，后跟一个合法的 Flurry 标识符：

```flurry
.ok
.error
.pending
.my_custom_identifier
.color
.width
```

### 特性

1.  **唯一性 (Uniqueness)**: 在整个编译单元或特定上下文中（具体作用域规则待定），每个具有相同名称的符号字面量（例如 `.ok`）都代表**同一个唯一的**符号值。
2.  **编译时常量 (Compile-time Constant)**: 符号是**编译时**的概念。它们的值在编译期间完全确定。这意味着它们可以自由地用于 `comptime` 计算、模式匹配、以及作为编译时 `object` 的键。
3.  **轻量级 (Lightweight)**: 符号通常被实现为整数或指针大小的值，使得比较操作（判断两个符号是否相等）非常快速，通常只是一个简单的整数比较。
4.  **非字符串 (Not Strings)**: 尽管符号看起来像带前缀的标识符，但它们**不是**字符串。它们不直接支持字符串操作（如拼接、取子串）。它们的核心目的是作为唯一的原子标识符。可以通过取像操作 `symbol'tag_name` 来获取其对应的字符串表示（编译时常量字符串）。

### 用途

符号在 Flurry 中有多种重要用途：

1.  **枚举变体的简化表示 (Enum Variants)**: 虽然 Flurry 的层级化枚举使用 `EnumName.Variant` 形式，但在模式匹配或某些上下文中，可以直接使用符号来匹配简单的、无数据的枚举变体（如果语法支持这种简写）。
    ```flurry
    -- 假设 Status 是 enum { ok, error, pending }
    match current_status {
        .ok => println("Status is OK"), -- 使用符号匹配
        .error => handle_error(),
        .pending => wait_more(),
    }
    ```
    *(注意：这种用法是否可行取决于 Flurry 模式匹配的具体规则。)*

2.  **编译时对象/映射的键 (Keys in Compile-time Objects/Maps)**: 正如在 `comptime object` 中看到的，符号是其属性键的标准形式。
    ```flurry
    comptime let config = {
        .host "localhost",
        .port 8080,
        .log_level .debug -- 使用符号作为值
    }
    let level = config.get(.log_level, Symbol)?; -- 使用符号作为键
    ```

3.  **属性系统的基础 (Foundation of Attribute System)**: Flurry 的属性系统（无论是内部定义 `.attr value` 还是外部应用 `^attr term`）都依赖符号来标识属性的名称。`.packed`, `.no_mangle`, `.route` 等都是符号。

4.  **轻量级标签/状态表示 (Lightweight Tags/State Representation)**: 在需要表示一组固定、互斥的状态或标签，并且不需要携带额外数据时，符号提供了一种比字符串或完整枚举更轻量级的方式。

5.  **元编程与反射 (Metaprogramming & Reflection)**: 在编译时操作类型信息时，字段名、方法名等可能被表示为符号。

### 与其他语言的比较

*   **Ruby/Elixir Symbols/Atoms**: Flurry 的符号与 Ruby 的 Symbol (`:symbol`) 和 Elixir 的 Atom (`:atom`) 非常相似，都用作轻量级、唯一的标识符常量。
*   **Julia Symbols**: Julia 也有类似的 `Symbol` 类型 (`:symbol`)。
*   **Lisp Symbols**: Lisp 中的符号功能更强大，既是标识符也是数据，但 Flurry 的符号似乎更侧重于作为编译时常量标识符。

### 总结

符号是 Flurry 编译时系统中的一个重要构件。通过提供一种轻量级、唯一且编译时确定的标识符表示，符号简化了元编程、属性访问和某些模式匹配场景，并与 Flurry 的 `comptime object` 和属性系统紧密集成。理解符号的概念对于深入利用 Flurry 的编译时能力和编写惯用的 Flurry 代码非常重要。