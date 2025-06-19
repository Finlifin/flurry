# 数组与切片 (Arrays & Slices)

数组（Array）和切片（Slice）是 Flurry 中用于处理**连续内存序列**的核心数据结构。它们都包含相同类型的元素。

## 数组 (Array)

数组是一组**固定大小**的、存储在**栈上或作为其他数据结构一部分**的同质元素序列。数组的大小是其**类型**的一部分。

**定义与实例化:**

数组类型通常表示为 `Array<T, N>`，其中 `T` 是元素类型，`N` 是数组长度（一个编译时usize）。

```flurry
-- 定义一个包含 5 个 i32 的数组
let numbers: Array<i32, 5> = undefined; -- `undefined`不进行初始化

-- 使用字面量初始化数组
let first_primes: Array<i32, 5> = [2, 3, 5, 7];

-- 创建一个包含 100 个 0 的数组
let zeros: Array<i8, 100> = Array.fill(0);
```

-   数组的大小 (`N`) 必须在编译时确定。
-   数组的所有元素在内存中是连续存储的。

**访问元素:**

使用方括号 `[]` 和索引（从 0 开始）来访问数组元素。

```flurry
let third_prime = first_primes[2]; -- 访问索引为 2 的元素 (值为 5)
println("Third prime: {}", third_prime);

let array = [10, 20, 30];
array[0] = 15; -- 修改第一个元素
```

**数组长度:**

```flurry
let len = first_primes.len();
println("Length of first_primes: {}", len); -- 输出: 4
```

## 切片 (Slice)

切片是对数组（或其他连续内存区域，如 `Vec` 的内部缓冲区）的一个**引用或视图**。切片本身**不拥有**数据，它只是“借用”了一段连续的元素。切片的**大小在运行时确定**。

**切片类型:**

切片类型通常表示为 `Slice<T>` 或类似的泛型类型，其中 `T` 是元素类型。一个切片实例内部通常包含一个指向序列起始元素的**指针**和一个**长度**信息。

**创建切片:**

切片通常通过对数组或其他支持切片操作的数据结构（如 `Vec`）进行“切片”操作来创建。

```flurry
let array = [1, 2, 3, 4, 5];

-- 创建一个包含所有元素的切片
let full_slice: Slice<i32> = array[..]; -- 引用整个数组

-- 创建一个包含索引 1 到 3 (不含 3) 的元素的切片
let partial_slice: Slice<i32> = array[1..3]; -- 引用 [2, 3]

-- 创建从索引 2 到末尾的切片
let tail_slice: Slice<i32> = array[2..]; -- 引用 [3, 4, 5]

-- 创建从开始到索引 3 (不含 3) 的切片
let head_slice: Slice<i32> = array[..3]; -- 引用 [1, 2, 3]
```

**使用切片:**

切片可以像数组一样使用索引访问其元素，但边界检查是在**运行时**进行的。

```flurry
println("First element of partial_slice: {}", partial_slice[0]); -- 输出: 2
-- partial_slice[2] -- 这会导致运行时错误 (越界)

let slice_len = partial_slice.len(); -- 获取切片长度
println("Length of partial_slice: {}", slice_len); -- 输出: 2
```

切片经常用作函数参数，以接受任何长度的同质序列，而无需知道其底层是数组还是 `Vec`。

```flurry
-- 这个函数可以接受任何 i32 的切片
fn sum(numbers: Slice<i32>) -> i32 {
    let total = 0;
    for num in numbers { -- 可以直接迭代切片
        total += num;
    }
    total
}

test {
    let arr = [1, 2, 3];
    let vec_data = Vec<i32>.from([4, 5, 6]); -- 假设 Vec 可以创建
    println("Sum of arr slice: {}", sum(arr[..]));       -- 传递数组切片
    println("Sum of vec slice: {}", sum(vec_data.slice())); -- 假设 Vec 提供切片方法
}
```

**总结**

数组是固定大小的、拥有数据的序列，而切片是动态大小的、借用数据的视图。数组的大小是类型的一部分（编译时确定），切片的大小是运行时确定的。切片提供了处理不同来源连续序列的统一、灵活的方式。