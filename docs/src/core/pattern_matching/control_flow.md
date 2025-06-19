# 匹配控制流 (Matching Control Flow)

模式匹配不仅限于解构数据，它还深度集成到 Flurry 的控制流语句中，提供了比传统条件判断更强大、更安全的流程控制方式。主要体现在 `match` 表达式、`if is` 语句和 `while is` 循环中。

## `match` 表达式

`match` 表达式是 Flurry 中进行模式匹配的核心结构。它接收一个表达式的值，然后按顺序尝试将其与一系列模式进行匹配。一旦找到第一个成功匹配的模式，就会执行该模式对应的代码块，并且 `match` 表达式本身会返回该代码块的求值结果。

**基本语法:**

```flurry
<value_expr> match {
    <pattern_1> => <result_expr_1>,
    <pattern_2> => <result_expr_2>,
    -- ... more branches
    <pattern_n> => <result_expr_n>,
    -- 可以提供一个通配符或变量绑定来确保穷尽性
    _ => <default_result_expr>,
}
```

-   `<value_expr>`: 需要被匹配的值。
-   `<pattern_i> => <result_expr_i>`: 一个匹配分支，由模式和对应的结果表达式（或代码块）组成。
-   `match` 按顺序尝试匹配 `<pattern_i>`。第一个成功匹配的分支会被选中。
-   模式中绑定的变量在对应的 `<result_expr_i>` 中可用。
-   **穷尽性 (Exhaustiveness)**: Flurry 编译器通常会检查 `match` 表达式的分支是否覆盖了 `<value_expr>` 类型的所有可能情况。如果不是穷尽的，并且没有通配符 `_` 或变量绑定分支来处理剩余情况，编译器会报错，以防止运行时遗漏某些情况。
-   **返回值**: `match` 作为一个表达式，其本身会有一个值，即被选中分支的 `<result_expr_i>` 的值。所有分支的结果表达式必须具有兼容的类型。

**示例:**

```flurry
enum Message { ping, pong, text(String) }

fn process_message(msg: Message) -> String {
    let response = msg match {
        .ping => "Pong response".to_string(),
        .pong => "Ping response".to_string(),
        .text(content) if content.len() > 10 => { -- 使用模式守卫
            println("Received long text: {}", content);
            "Text OK (long)".to_string()
        },
        .Textextt(content) => {
            println("Received short text: {}", content);
            "Text OK (short)".to_string()
        } -- 这个 match 是穷尽的，覆盖了所有 Message 变体
    }
    response
}
```

## `if is` 语句

`if is` 语句不仅可以可以像match一样匹配多个分支，还提供了一种简洁的方式来检查一个值是否匹配**单个**模式，并在匹配成功时执行一个代码块。
**基本语法:**

```flurry
if <value_expr> is <pattern> {
    -- Code to execute if value_expr matches pattern
    -- pattern 中绑定的变量在此可用
} else if <value_expr> is <another_pattern> { -- 可选 else if is
    -- ...
} else { -- 可选 else
    -- Code to execute if no preceding pattern matched
}
```

-   它尝试将 `<value_expr>` 与 `<pattern>` 匹配。
-   如果匹配成功，执行第一个 `{...}` 块，并且模式中绑定的变量在该块内可用。
-   可以链式地使用 `else if is` 来检查其他模式。
-   可选的 `else` 块处理所有前面模式都不匹配的情况。

**示例 (处理可选类型):**

```flurry
fn print_optional(opt: ?i32) {
    if opt is some? { -- 使用可选模式 '?'
        println("Value is: {}", some); -- 'some' 自动绑定内部值
    } else {
        println("Value is None/null.");
    }
}

print_optional(Some(10)); -- 输出: Value is: 10
print_optional(None);     -- 输出: Value is None/null.
```

**对比 `match` 与 `if is`**:

-   `match`为后缀风格，适合在复杂链式表达式中使用。
-   当只关心是否匹配一两种特定模式时，`if is` 通常更简洁。

## `while is` 循环

`while is` 循环将模式匹配与 `while` 循环结合，允许在循环的每次迭代开始时对一个表达式的值进行模式匹配。只有当匹配成功时，循环体才会执行，并且模式绑定的变量可在循环体中使用。

**基本语法:**

```flurry
while <value_expr> is <pattern> {
    -- Loop body, executed if value_expr matches pattern
    -- pattern 中绑定的变量在此可用
}
```

-   在每次循环迭代前，求值 `<value_expr>` 并尝试与 `<pattern>` 匹配。
-   如果匹配成功，执行循环体。
-   如果匹配失败，循环终止。

**示例 (处理迭代器返回可选类型):**

```flurry
let some_iterator = create_iterator();

while some_iterator.next() is item? {
    process_item(item);
}

println("Iterator finished.");
```

`while is` 对于消耗迭代器、处理消息队列或其他在循环中需要检查并解构值的场景非常方便和安全。

**总结**

Flurry 将模式匹配与核心控制流结构（`match`, `if is`, `while is`）深度集成，提供了一种强大、安全且表达力强的方式来根据数据的结构和值来引导程序流程。穷尽性检查（主要在 `match` 中）进一步增强了代码的健壮性。
