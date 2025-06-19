# 模式语法 (Pattern Syntax)

模式 (Pattern) 是 Flurry 中用于匹配值的语法结构。以下是 Flurry 支持的主要模式类型：

**1. 基本模式 (Basic Patterns)**

这些是最简单的模式，用于匹配字面量或进行简单的绑定/忽略。

-   **字面量模式 (Literal Patterns)**: 直接匹配整数、浮点数、字符、字符串、布尔值或 `null`。
    ```flurry
    if value is {
        0 => println("Zero"),
        'a' => println("The character 'a'"),
        "hello" => println("Got hello"),
        true => println("It's true"),
        null => println("It's null"),
        _ => println("Something else"), -- 通配符模式
    }
    ```
-   **标识符模式 (Identifier Pattern) / 变量绑定**: 匹配任何值，并将其绑定到一个新的变量上。该变量在匹配分支的作用域内可用。
    ```flurry
    if some_option is {
        data? => println("Got data: {}", data), -- data 绑定了 some 内部的值
        null => println("No data"),
    }
    ```
-   **通配符模式 (`_`)**: 匹配任何值，但不将其绑定到变量。用于忽略不关心的值或部分结构。
    ```flurry
    let (x, _, z) = (1, 2, 3); -- 忽略元组的第二个元素
    ```
-   **符号模式 (`.symbol`)**: 匹配特定的符号字面量。常用于匹配枚举变体或`Symbol`类型。
    ```flurry
    if status is {
        .ok => println("Status is Ok"),
        .error => println("An error occurred"),
        -- ...
    }
    ```

**2. 容器模式 (Container Patterns)**

用于解构和匹配复合数据结构。

-   **列表/数组/切片模式 (`[pattern1, pattern2, ...]`)**: 匹配列表、数组或切片。
    -   可以匹配固定长度：`[first, second]`
    -   可以使用 `...rest_binding` 捕获剩余元素（Rest Pattern）：`[head, ...tail]`
    -   可以匹配空列表：`[]`
    ```flurry
    if items is {
        [] => println("Empty list"),
        [one] => println("One item: {}", one),
        [first, second, ...rest] => println("First: {}, Second: {}, Rest: {any}", first, second, rest),
        -- ...
    }
    ```
-   **元组模式 (`(pattern1, pattern2, ...)`)**: 按位置解构元组。
    ```flurry
    let (id, name, _) = get_user_info(); -- 解构元组，忽略第三个元素
    ```
-   **记录/结构体模式 (`{ field1: pattern1, field2, ... }`)**: 按字段名解构结构体或 `object`。
    -   `field: pattern`: 将字段 `field` 的值与 `pattern` 匹配。
    -   `field`: 字段名简写，等同于 `field: field`，即将字段值绑定到同名变量。
    -   `...`: （可能支持）用于表示匹配剩余字段，但通常结构体匹配是精确的或显式忽略。
    ```flurry
    struct Point { x: i32, y: i32 }
    let p = Point { .x 10, .y 20 }
    if p is {
        -- 使用字段名简写和显式模式
        { x: 0, y } => println("On Y axis at {}", y),
        { x, y: 0 } => println("On X axis at {}", x),
        { x, y }    => println("Point at ({}, {})", x, y),
    }
    ```

**3. 调用模式 (Call Patterns)**

用于匹配枚举变体或构造器调用的结果，常用于解构带有数据的枚举。

-   **圆括号调用模式 (`pattern (pattern1, ...)` 或 `pattern (pattern1, ...)`?)**: 匹配特定的枚举变体（带有关联数据）、代数效应调用。
    ```flurry
    enum Message { quit, write(String), move { x: i32, y: i32 } }
    if msg is {
        -- 匹配不带数据的变体
        .quit => println("Quit message"),
        -- 匹配 Write 变体，并将内部 String 绑定到 text
        .write(text) => println("Write: {}", text),
        -- 匹配 Move 变体，并使用记录模式解构其关联数据
        .move { x, y: 0 } => println("Move on X axis to {}", x),
        .move { x, y } => println("Move to ({}, {})", x, y),
    }
    ```
-   **尖括号调用模式 (`pattern<...>(...)` 或 `pattern<...>(...)`)**: 用于匹配带有泛型参数的代数效应调用、类型表达式。
-   **花括号调用模式 (`pattern{...}` 或 `pattern{...}`):** 用于匹配使用记录语法的变体或构造器（如上例中的 `move`）。

**4. 范围模式 (Range Patterns)**

用于匹配数值或字符是否落在某个范围内。

-   **开区间上限 (`..end`)**: 匹配小于 `end` 的值。（之前讨论过，可能需要确认语法）
-   **闭区间 (`start..=end`)**: 匹配大于等于 `start` 且小于等于 `end` 的值。
-   **开区间 (`start..end`)**: 匹配大于等于 `start` 且小于 `end` 的值。
-   **闭区间下限 (`start..`)**: 匹配大于等于 `start` 的值。
    ```flurry
    if value is {
        0..=9 => println("Single digit"),
        10..=99 => println("Two digits"),
        100.. => println("Three or more digits"),
        ..0 => println("Negative or zero? Check exact rules"), -- 语法和含义需确认
    }
    if character is {
        'a'..='z' => println("Lowercase letter"),
        'A'..='Z' => println("Uppercase letter"),
        _ => println("Not a letter"),
    }
    ```

**5. 组合模式 (Combination Patterns)**

允许将多个模式组合起来。

-   **或模式 (`pattern1 or pattern2`)**: 如果值匹配 `pattern1` **或** `pattern2`，则匹配成功。用于合并相似的分支。
    ```flurry
    if character is {
        'a' or 'e' or 'i' or 'o' or 'u' => println("Lowercase vowel"),
        'A' or 'E' or 'I' or 'O' or 'U' => println("Uppercase vowel"),
        _ => println("Consonant or other"),
    }
    ```
    **注意**: 在 `or` 模式中，所有分支绑定的变量必须**名称和类型都相同**，或者只使用 `_` 忽略。
-   **与模式 (`pattern1 and expr is pattern2`)**: 如果值匹配 `pattern1`，并且 `expr` 的值匹配 `pattern2`，则匹配成功。用于在模式中嵌套其他表达式。
    ```flurry
    -- 选课逻辑展示
    fn select_course(student: Student, course: Course) {
		if student is { class_id: class_id? }
			and get_class(class_id) is class?
			and get_major(class.major_id) is major?
			if course.satified(major) {
			println("选课成功")
		}
    }
    ```

**6. 绑定模式 (`pattern as identifier`)**

在匹配 `pattern` 的同时，将**整个匹配到的值**（未解构的）绑定到一个新的变量 `identifier` 上。

```flurry
if message is {
    -- 匹配 move 变体，解构 x，并将整个 move 变体绑定到 m
    .move { x: 10, y } as msg => {
        println("Move to x=10 at y={}", y);
        process_move_message(msg); -- 使用绑定的整个消息 msg
    }
    -- ... 其他分支
}
```

**7. 条件守卫模式 (`pattern if condition_expr`)**

只有当值匹配 `pattern` **并且** `condition_expr`（一个运行时布尔表达式）求值为 `true` 时，模式才算匹配成功。`condition_expr` 可以使用 `pattern` 中绑定的变量。

```flurry
if maybe_point is {
    { x, y }? if x == y => { -- 匹配结构体 Point
        println("Point on diagonal: ({}, {})", x, y);
    },
    { x, y }? => { -- 其他结构体情况
        println("Point not on diagonal: ({}, {})", x, y);
    }
    _ => println("Not a point"),
}
```

**8. 其他模式**

-   **可选模式 (`pattern?`)**: 用于匹配可空类型的非空部分，并解构其内部值。（如 `if opt is data? { ... }`）
-   **正确模式**: 用于匹配结果类型的成功部分，并解构其内部值。（如 `if result is data! { ... }`）
-   **错误模式**: 用于匹配结果类型的错误部分，并解构其内部值。（如 `if result is error err { ... }`）
-   **表达式模式 (`<expr>`)**: 将编译时表达式 `expr` 的求值结果作为模式进行匹配。
-   **位向量模式 (`0x(...)`, `0o(...)`, `0b(...)`)**: 匹配和解构位向量字面量。
-   **字符串前缀模式 (`"prefix" ++ rest_binding`)**: 匹配以特定前缀开头的字符串。
-   **否定模式 (`not pattern`)**: 匹配不符合 `pattern` 的值。
-   **类型绑定模式 (`'id`)**: 用于匹配类型表达式，为了方便起见，类型模式中的id通常会被解析为表达式，因此变量绑定需要新的语法，如`Array<'T, 128>`。

Flurry 提供了极为丰富和灵活的模式语法，为数据检查、解构和控制流提供了强大的支持。
