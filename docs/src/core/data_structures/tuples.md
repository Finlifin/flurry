# 元组 (Tuples)

元组（Tuple）是一种简单、匿名的复合数据类型，用于将**固定数量**的、**可能不同类型**的值组合在一起。元组的大小（元素数量）和每个元素的类型在编译时确定。

**定义与实例化:**

元组使用圆括号 `()` 将其元素括起来，元素之间用逗号 `,` 分隔。元组的类型由其包含的元素类型和顺序决定。

```flurry
-- 创建一个包含 i32 和 f64 的元组
let pair: (i32, f64) = (10, 3.14);

-- 类型可以省略，编译器会自动推断
let another_pair = (-5, true); -- 类型是 (i32, bool)

-- 包含不同类型的元组
let mix = (1, "hello", false, 3.0f32); -- 类型是 (i32, str, bool, f32)

-- 单元元组 (Unit Tuple) / 空元组
let empty: () = (); -- 类型是 `void`，只有一个值 ()
```

**访问元素 (解构与索引):**

访问元组成员主要有两种方式：

1.  **解构绑定 (Destructuring Bind)**: 使用 `let` 和模式匹配将元组的元素直接绑定到变量。这是最常用的方式。

    ```flurry
    let (integer_part, float_part) = pair; -- 解构 pair
    println("Integer: {}, Float: {}", integer_part, float_part); -- 输出: Integer: 10, Float: 3.14

    let (status_code, _, message, _) = mix; -- 使用 _ 忽略不需要的元素
    println("Status: {}", status_code);
    ```

2.  **索引访问 (Index Access)**: 访问特定位置的元素。

    ```flurry
    let first_element = pair[0];  -- 访问第一个元素 (10)
    let second_element = pair[1]; -- 访问第二个元素 (3.14)
    println("Access via index: {}, {}", first_element, second_element);
    ```

**元组作为函数返回值:**

元组非常适合用作函数的返回值，以一次性返回多个不同类型的值。

```flurry
fn divide_with_remainder(dividend: i32, divisor: i32) -> (i32, i32) {
    let quotient = dividend / divisor;
    let remainder = dividend % divisor;
    (quotient, remainder) -- 返回一个包含商和余数的元组
}

test {
    let (q, r) = divide_with_remainder(10, 3);
    println("10 / 3 = {} remainder {}", q, r); -- 输出: 10 / 3 = 3 remainder 1
}
```

**单元类型/空类型 `void`**:

空类型 `void` 也称为单元类型 (Unit Type)。它只有一个值 `()`。当函数不返回任何有意义的信息时，通常隐式或显式地返回 `()` 类型。

**总结**

元组提供了一种轻量级的方式来组合固定数量、可能不同类型的值。它们在需要临时组合数据或从函数返回多个值时非常有用。通过解构绑定和索引访问可以方便地使用元组成员。