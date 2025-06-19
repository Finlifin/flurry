## 后缀风格与链式调用

Flurry 语言在表达式语法设计上一个显著的特点是广泛采用**后缀 (Suffix)** 形式的操作符和语法结构。这种设计使得**链式调用 (Method Chaining / Fluent Interface)** 变得极其自然和流畅，提高了代码的可读性，使得代码逻辑能够从左到右平滑地展开。

核心的后缀操作符是**选择运算符 (`.`)**，但它的作用远不止访问字段。

### 选择运算符 (`.`) 的多重角色

点号 `.` 是 Flurry 中最繁忙的操作符之一，它作为连接符，统一了多种常见的后缀操作：

1.  **访问结构体字段 (Field Access)**:
    ```flurry
    struct Point { x: i32, y: i32 }
    let p = Point { .x 10, .y 20 }
    let x_coord = p.x; -- 访问字段 x
    ```

2.  **调用方法 (Method Call)**:
    ```flurry
    struct Greeter { name: String }
    impl Greeter {
        fn greet(*self) { println("Hello, {str}!", self.name); }
    }
    let g = Greeter { .name "Flurry" }
    g.greet(); -- 调用 greet 方法
    ```
    注意，方法调用本身 `()` 也是一种后缀操作，与 `.` 结合使用。

3.  **访问层级化枚举变体 (Enum Variant Access)**:
    ```flurry
    enum Color { rgb.{ r: u8, g: u8, b: u8 }, hsv.{...} }
    let red = Color.rgb.{ .r 255, .g 0, .b 0 } -- 使用 . 选择层级
    ```

4.  **限定模块路径 (Module Path Qualification)**: 在 `use` 语句或直接引用时，用于分隔模块路径。
    ```flurry
    use std.io.println;
    std.math.sqrt(4.0); -- 使用 . 限定路径
    ```

5.  **连接特殊后缀操作 (Connecting Special Suffix Operations)**: 这是 Flurry 后缀风格的关键体现。`.` 用于连接表达式和语言内建的特殊后缀操作：
    *   **解引用 (`.*`)**:
        ```flurry
        let ptr = data.ref;
        let value = ptr.*; -- 使用 . 连接 *
        ```
    *   **取引用 (`.ref`)**:
        ```flurry
        let ptr = data.ref; -- 使用 . 连接 ref 关键字
        ```
    *   **类型转换/断言 (`.as(TypeExpr)`)**:
        ```flurry
        let any_value: Any = get_value();
        let specific_value = any_value.as(MyType)?; -- 使用 . 连接 as(...)
        ```
    *   **Trait 对象转换 (`.dyn(TraitExpr)`)**: (如果支持)
        ```flurry
        let concrete_value = MyStruct {}
        let trait_object = concrete_value.dyn(MyTrait); -- 使用 . 连接 dyn(...)
        ```
    *   *(可能还有其他类似 `.some_op(...)` 的内置操作)*

### 链式调用的优势

这种统一的后缀风格使得链式调用非常自然：

```flurry
let result = get_data_source() -- 1. 获取数据源
    .ref                 -- 2. 获取其指针 (假设返回指针的方法)
    .process_intermediate()? -- 3. 调用处理方法 (可能返回 Result/Optional)
    .*                   -- 4. 解引用得到结果 (假设 process 返回指针)
    .filter_items(|item| item.is_valid()) -- 5. 调用过滤方法
    .map_values(|item| item.transform())   -- 6. 调用映射方法
    .collect<Vec<_>>(); -- 7. 调用收集方法，注意泛型调用也是后缀 `<...>()`

-- 如果没有后缀风格，代码可能看起来像：
-- let source = get_data_source();
-- let ptr = &source; -- 或者 source.get_ref()
-- let intermediate_ptr = process_intermediate(ptr)?;
-- let intermediate_value = *intermediate_ptr; -- 或者 intermediate_ptr.deref()
-- let filtered = filter_items(intermediate_value, |item| item.is_valid());
-- ... 等等，更加嵌套和分散
```

链式调用使得数据处理流水线和连续操作的逻辑能够以线性的、从左到右的方式表达，更符合阅读习惯，减少了嵌套和临时变量。

### 其他后缀操作

除了通过 `.` 连接的操作外，Flurry 还有其他重要的后缀操作：

*   **取像操作 (`expr ' image`)**: 获取表达式的某个“像”，如类型、标签名、位向量表示等。
    ```flurry
    let type_info = my_variable'type;
    let tag_name = my_enum_value'tag_name;
    ```
*   **调用操作符**:
    *   `()`: 函数/方法调用。
    *   `[]`: 索引访问/调用。
    *   `{}`: `expr object` 调用 / 记录调用。
    *   `<>`: 泛型参数传递 / 钻石调用。
*   **错误/可选/效应处理**: `!`, `?`, `#` 通常也紧跟在表达式之后。
*   **字面量拓展**: `10px`, `"s"c` 可以视为对字面量表达式的后缀操作。
*   **`match` 表达式 (后缀形式)**: `expr match { ... }`（如果 `match` 是后缀关键字）。

### 优先级

根据您提供的 `OpTable`，这些后缀操作（`.`, `'`, `()`, `[]`, `{}`, `<>`) 具有非常高的优先级（90-100），确保它们会紧密地绑定到其左侧的操作数（表达式），这对于链式调用的正确解析至关重要。字面量拓展对应的 `id` 甚至有更高的优先级 (110)，确保 `10px` 被视为一个整体而非 `10` 和 `px` 分开。

### 总结

Flurry 对后缀操作符和语法的广泛采用，特别是万能的**选择运算符 (`.`)**，是其语法设计的一大特色。它极大地促进了**链式调用**的流畅性和代码的线性可读性，并为多种不同的操作（字段访问、方法调用、内置操作如解引用/取引用/转换）提供了一致的语法入口。这种设计选择使得 Flurry 代码在处理连续操作和数据流时显得尤为优雅和简洁。