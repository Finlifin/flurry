# 控制转移语句 (Control Transfer Statements)

控制转移语句允许程序非顺序地改变其执行流程，例如提前退出循环或函数。Flurry 提供了 `break`, `continue`, 和 `return` 三种主要的控制转移语句。

## `break`

`break` 语句用于立即**终止**其所在的**最内层**循环（`for` 或 `while`）的执行。程序控制流将跳转到该循环语句之后的下一条语句。

**示例:**

```flurry
let numbers = [1, 5, -3, 8, 2];
let found_negative = false;

for num in numbers {
    if num < 0 {
        found_negative = true;
        break; -- 找到第一个负数，立即退出 for 循环
    }
    println("Checking positive number: {}", num);
}

if found_negative {
    println("Found a negative number.");
} else {
    println("All numbers are non-negative.");
}
-- 输出:
-- Checking positive number: 1
-- Checking positive number: 5
-- Found a negative number.
```

**带标签的 `break`**: 如果存在嵌套循环，可以使用标签来指定 `break` 要终止的是**哪个**循环。

```flurry
for:outer i in 0..5 {
    for j in 0..5 {
        if i * j > 10 {
            println("Found i*j > 10 at i={}, j={}. Breaking outer loop.", i, j);
            break outer; -- 终止名为 'outer' 的循环
        }
    }
}
```

## `continue`

`continue` 语句用于**跳过**当前循环迭代中**剩余**的代码，并立即开始**下一次**迭代。

-   在 `while` 循环中，控制流会跳转回循环条件的判断处。
-   在 `for` 循环中，控制流会跳转到下一次迭代（获取迭代器的下一个元素）。

**示例:**

```flurry
for i in 0..5 {
    if i % 2 == 0 {
        continue; -- 如果 i 是偶数，跳过本次迭代的 println
    }
    println("Found odd number: {}", i);
}
-- 输出:
-- Found odd number: 1
-- Found odd number: 3
```

**带标签的 `continue`**: 与 `break` 类似，可以使用标签来指定 `continue` 要作用于哪个嵌套循环。

```flurry
for:outer i in 0..3 {
    for j in 0..3 {
        if i == 1 {
            println("Skipping outer iteration for i=1");
            continue outer; -- 跳过外层循环 i=1 的剩余部分，直接开始 i=2
        }
        println("Processing i={}, j={}", i, j);
    }
}
```

## `return`

`return` 语句用于从当前**函数**中退出，并可选地将一个值返回给函数的调用者。

**语法:**

```flurry
return;             -- 从返回 void 的函数退出
return <value_expr>; -- 从返回特定类型的函数退出，并返回值
```

-   `<value_expr>`: 其类型必须与函数签名中声明的返回类型兼容。
-   一旦执行 `return` 语句，函数立即终止，控制权交还给调用点。

**示例:**

```flurry
fn check_age(age: u32) -> bool {
    if age < 18 {
        println("Too young.");
        return false; -- 提前返回 false
    }
    println("Age is sufficient.");
    return true; -- 返回 true
}

fn process() {
    println("Processing start.");
    if some_condition() {
        return; -- 提前退出 process 函数 (假设 process 返回 void)
    }
    println("Processing continued.");
    -- ...
    println("Processing end.");
}
```

**`return` 与 `if_guard`**: `return` 可以与条件守卫结合使用，形成简洁的提前返回模式。

```flurry
fn get_user(id: Uuid) -> ?User {
    let data = database.fetch(id)?; -- 假设 ? 用于错误传播或 Option 解包
    return null if data.is_empty(); -- 如果数据为空，提前返回 null
    -- ... process non-empty data ...
    User.from(data)
}
```

**总结**: `break`, `continue`, 和 `return` 是控制程序非线性执行流程的基本工具。`break` 和 `continue` 主要用于控制循环，而 `return` 用于退出函数。标签的使用为嵌套结构提供了更精确的控制能力。
