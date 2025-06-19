# 循环语句 (Looping Statements)

循环语句允许程序重复执行一段代码，直到满足特定条件为止。Flurry 提供了 `for` 和 `while` 两种主要的循环结构。

## `for` 循环

`for` 循环主要用于**迭代**一个序列或集合中的元素。它可以遍历数组、切片、范围、迭代器或其他可迭代对象。

**基本语法 (遍历迭代器):**

```flurry
for <pattern> in <iterable_expr> {
    -- Loop body, executed for each item
}
```

-   `<iterable_expr>`: 必须是一个实现了IntoIterator或Iterator的表达式。
-   `<pattern>`: 用于在每次迭代中绑定从迭代器获取的当前元素。可以使用简单的变量名，也可以使用更复杂的模式（如元组 `(index, value)`）。
-   循环体 `{ ... }`: 对迭代器产生的每个元素执行一次。当迭代器耗尽时，循环结束。

**示例:**

```flurry
let numbers = [10, 20, 30];

-- 遍历数组元素
for num in numbers {
    println("Number: {}", num);
}
-- 输出:
-- Number: 10
-- Number: 20
-- Number: 30

-- 遍历范围
for i in 0..3 { -- 遍历 0, 1, 2
    println("Index: {}", i);
}

-- 遍历并获取索引
for (index, value) in numbers.into_iter().enumerate() {
    println("Item at {}: {}", index, value);
}
```

**循环标签 (Loop Labels)**: 可以为 `for` 循环（以及 `while` 循环）添加标签，用于在嵌套循环中精确控制 `break` 和 `continue` 的目标。标签以 `:` 结尾。

```flurry
for:outer i in 0..3 {
    for:inner j in 0..3 {
        continue outer if i == 1 and j == 1; -- 跳过外层循环的剩余部分，开始下一次外层迭代
        break outer if i == 2 and j == 0; -- 完全跳出外层循环
        println("i={}, j={}", i, j);
    }
}
```

## `while` 循环

`while` 循环会在其条件表达式求值为 `true` 时重复执行代码块。

**基本语法:**

```flurry
while <condition_expr> {
    -- Loop body, executed as long as condition_expr is true
}
```

-   `<condition_expr>`: 在每次循环迭代**之前**进行求值。如果为 `true`，则执行循环体；如果为 `false`，则退出循环。

**示例:**

```flurry
let count = 0;
while count < 3 {
    println("Count is: {}", count);
    count += 1;
}
-- 输出:
-- Count is: 0
-- Count is: 1
-- Count is: 2

-- 无限循环 (通常与 break 结合使用)
while true {
    let input = read_input()?; -- 假设 read_input 可能返回错误
    break if input == "quit"; -- 退出无限循环
    process(input);
}
```

**`while is` (模式匹配循环)**: Flurry 还提供了将模式匹配与 `while` 结合的循环方式，允许在每次迭代时对某个表达式的值进行模式匹配，并在匹配成功时继续循环。

**基本语法:**

```flurry
while <expr> is <pattern> {
    -- Loop body, executed if expr matches pattern
    -- pattern 中绑定的变量在此可用
}
```

-   `<expr>`: 每次循环前求值的表达式。
-   `<pattern>`: 用于匹配 `<expr>` 结果的模式。
-   只有当 `<expr>` 的值成功匹配 `<pattern>` 时，循环体才会执行。模式中绑定的变量可以在循环体中使用。当匹配失败时，循环终止。

**示例 (处理 Option):**

```flurry
let optional_value = 5;

while optional_value is some? { -- 只要 optional_value 是 some? 就继续
    println("Processing value: {}", some);
    -- 更新 optional_value 以便最终退出循环
    optional_value = if some > 0 { some - 1 } else { null }
}
-- 输出:
-- Processing value: 5
-- Processing value: 4
-- Processing value: 3
-- Processing value: 2
-- Processing value: 1
-- Processing value: 0

println("Loop finished.");
```

这对于处理迭代器、队列或其他可能返回“哨兵值”（如 `null`, `EndOfFile`）来表示结束的数据源非常有用。