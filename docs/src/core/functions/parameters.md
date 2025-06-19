# 参数详解 (Parameter Details)

函数通过参数接收输入数据。Flurry 提供了多种参数类型，以满足不同的编程需求和代码风格。

## 1. 普通参数 (Positional Parameters)

这是最基本的参数形式，通过位置进行传递。

**定义:** `parameter_name: Type`

```flurry
fn process_data(data: Slice<u8>, length: usize) { {- ... -} }
```

**调用:** 按定义顺序提供值。

```flurry
let buffer = [1, 2, 3];
process_data(buffer[..], buffer.len());
```

## 2. 可选参数 (Optional Parameters)

参数类型为`option type`, 则可以选择性传递该参数，不传递则传递`null`。

**定义:** `parameter_name: ?Type`

```flurry
-- timeout 是一个可选的毫秒数
fn connect(address: String, timeout: ?u64) -> bool {
    let resolved_timeout = timeout.unwrap_or(5000); -- 使用默认值 5000ms
    -- ... 连接逻辑使用 resolved_timeout ...
    true -- 假设连接成功
}
```

**调用:**

```flurry
connect("example.com".to_string()); -- 不提供超时
connect("example.com".to_string(), 10000); -- 提供 10 秒超时
```

## 3. 具名参数 (Named Parameters)

用于提高函数调用的可读性，尤其是在参数较多或参数意义不明显时。

**定义:** `.parameter_name: Type = default_value`

-   必须以点 `.` 开头。
-   必须提供一个默认值。

```flurry
-- 配置选项通常使用具名参数
fn configure_service(
    .retries: usize = 3,
    .use_tls: bool = true,
    .log_level: LogLevel = LogLevel.info, 
) { {- ... -} }
```

**调用:** 调用时**必须**使用参数名，顺序任意。可以省略使用默认值的参数。

```flurry
-- 使用部分默认值
configure_service(.log_level = LogLevel.Debug);

-- 指定多个参数，顺序随意
configure_service(.use_tls = false, .retries = 5);

-- 使用所有默认值
configure_service();

-- configure_service(5, false); -- 错误！必须使用名称
```

## 4. 变长参数 (Variadic Parameters)

允许函数接受不定数量的参数。

**定义:** `...parameter_name: TupleType` (通常放在最后)

-   使用 `...` 前缀。
-   调用时传递给该部分的所有参数会被收集到一个**元组 (tuple)** 中。
-   `TupleType` 指定了期望的元组类型。

**调用:** 正常传递参数，它们会被自动收集。

```flurry
-- 接受任意数量 i32 的函数
fn sum_all(...numbers: T) -> i32 
where T, requires Type.is_tuple(T)
{
    let total = 0;
    inline for num in numbers {
        inline if num'type != i32 {
            build.error("All arguments must be i32");
        } else {
            total += num;
        }
    }
    total
}

test {
    println("Sum: {}", sum_all(1, 2, 3, 4)); -- numbers 是 (1, 2, 3, 4)
    println("Sum: {}", sum_all(10));         -- numbers 是 (10,)
    println("Sum: {}", sum_all());           -- numbers 是 ()
}
```

**编译时类型安全的变长参数**: 结合 `comptime` 参数，可以实现一些依赖类型特性。

```flurry
-- format 在编译时确定，varargs 的类型也随之确定
fn println(comptime format: str, ...varargs: fmt.Args(format)) {
    -- 内部使用 inline for 安全处理
    -- ...
}
```

**双变参调用**: Flurry 支持一种特殊的双变参函数和调用语法，用于构建 DSL。（详见相关章节）

## 5. 隐式参数 (Implicit Parameters)

由编译器自动提供的参数，无需在调用点显式传递。

**定义:** `implicit parameter_name: Type`

```flurry
-- 获取源代码位置
fn log(message: String, implicit __src__: builtin.SourceLocation) {
    println("{}:{}: {}", src.file, src.line, message);
}
```

**调用:** 调用者像调用普通函数一样，编译器负责查找并传入合适的隐式参数。

```flurry
-- let memory = allocate_memory(1024); -- 编译器自动传入默认分配器
log("Initialization complete.");    -- 编译器自动传入调用点的源代码位置
```
隐式参数的解析规则（如何查找、优先级等）需要语言明确定义。它们是实现上下文传递、依赖注入等模式的有力工具。

## 6. 编译时参数 (`comptime`)

参数值必须在编译时可知。

**定义:** `comptime parameter_name: Type`

-   可以与其他参数种类（普通、可选、具名、变长）结合。
-   值在编译时确定。
-   后续参数的类型或值可以依赖于前面的 `comptime` 参数（**依赖类型**）。

```flurry
-- T 和 N 都是编译时参数
fn create_array(initial_value: T) -> Array<T, N>
    where T:- Copy, N: usize -- 约束 T 必须是可复制的，N 必须是无符号整数
{
    Array<T, N>.new(initial_value) -- 使用 T 和 N 创建数组
}

-- alignment 必须是编译时常量
fn aligned_alloc(size: usize, comptime alignment: usize) -> !AllocErr *u8 {
    -- ... 使用 alignment 进行内存分配 ...
}

test {
    let arr = create_array(0); -- T 是 i32，N 是 10
    println("Array: {:?}", arr);

    let ptr = aligned_alloc(1024, 16)!; -- alignment 是 16
    println("Aligned pointer: {:?}", ptr);
}
```

`comptime` 参数是 Flurry 实现泛型、元编程和零成本抽象的关键。每次使用不同的编译时参数调用函数时，编译器会生成一个该函数特化（单态化）的版本。
