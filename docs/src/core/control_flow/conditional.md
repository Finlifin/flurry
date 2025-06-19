# 条件语句 (Conditional Statements)

条件语句允许程序根据特定条件的值来选择性地执行代码块。Flurry 主要提供 `if` 和 `when` 两种基本条件语句。

## `if` 语句

`if` 语句是最基础的条件控制结构。它评估一个布尔表达式，如果结果为 `true`，则执行紧随其后的代码块；否则，可以选择性地执行 `else` 或 `else if` 分支。

**基本语法:**

```flurry
if <condition_expr> {
    -- Code to execute if condition_expr is true
} else if <another_condition_expr> {
    -- Code to execute if another_condition_expr is true
} else {
    -- Code to execute if all preceding conditions are false
}
```

-   `<condition_expr>`: 必须是一个求值为布尔值 (`bool`) 的表达式。
-   `{ ... }`: 花括号定义了与条件关联的代码块。
-   `else if` 和 `else` 子句是可选的。可以有零个或多个 `else if` 子句，最多一个 `else` 子句。

**示例:**

```flurry
let temperature = 25;

if temperature > 30 {
    println("It's hot!");
} else if temperature < 10 {
    println("It's cold!");
} else {
    println("Temperature is moderate.");
}
-- 输出: Temperature is moderate.

let is_active = false;
if is_active {
    -- This block will not be executed
    println("User is active.");
}
```

**`if` 表达式**: `if` 语句也可以作为表达式使用，其每个分支必须返回兼容类型的值。

```flurry
let access_level = if user.is_admin {
    "admin"
} else if user.is_moderator {
    "moderator"
} else {
    "guest"
}
-- access_level 的类型会被推断为 str (或 String)
```

**条件守卫 (`if_guard`)**: 在其他语句（如 `return`, `break`, `continue`）后紧跟 `if <condition_expr>`，可以在满足条件时才执行该语句。

```flurry
fn find_first_even(numbers: Slice<i32>) -> ?i32 {
    for number in numbers {
        return number if number % 2 == 0; -- 如果 number 是偶数，则立即返回
    }
    return null; -- 没有找到偶数
}
```
这提供了一种简洁的方式来表达提前退出的逻辑。

## `when` 语句

`when` 语句提供了一种更结构化的多分支选择方式，类似于其他语言中的 `switch` 或 `match` 语句，但其分支条件是任意布尔表达式。它依次评估每个分支的条件，并执行**第一个**条件为 `true` 的分支所对应的代码。

**基本语法:**

```flurry
when {
    <condition_expr_1> => <statement_or_block_1>,
    <condition_expr_2> => <statement_or_block_2>,
    -- ... more branches
    -- 可选的 else 分支 (相当于 true => ...)
    _ => <default_statement_or_block>,
}
```

-   `=>`: 用于分隔条件和要执行的语句或代码块。
-   `when` 语句会按顺序检查 `<condition_expr>`，一旦找到第一个为 `true` 的条件，就执行其对应的代码，然后**退出 `when` 语句**（不会继续检查后续分支）。
-   可选的 `_ => ...` 分支相当于 `true => ...`，当前面所有条件都不满足时执行。如果没有 `_` 且所有条件都为 `false`，则 `when` 语句不执行任何分支。

**示例:**

```flurry
let status_code = 404;

when {
    status_code == 200 => println("OK"),
    status_code == 404 => println("Not Found"),
    status_code >= 500 => println("Server Error"),
    else => println("Unknown Status: {}", status_code),
}
-- 输出: Not Found
```

**`when` 表达式**: 与 `if` 类似，`when` 也可以作为表达式使用，所有分支（如果存在）必须返回兼容类型的值。

```flurry
let status_category = when {
    status_code >= 200 and status_code < 300 => "Success",
    status_code >= 400 and status_code < 500 => "Client Error",
    status_code >= 500 => "Server Error",
    else => "Other",
}
-- status_category 将是 "Client Error"
```

**对比 `if`/`else if` 与 `when`**:

-   `if`/`else if` 链更通用，可以嵌套。
-   `when` 对于多个并列条件的检查通常更清晰、更结构化，避免了深层嵌套的 `else if`。

选择哪种取决于具体的逻辑和代码风格偏好。
