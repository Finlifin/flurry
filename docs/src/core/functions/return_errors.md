
# 返回值与错误处理 (!)

函数通过返回值将其计算结果或状态传递给调用者。Flurry 提供了标准的返回值机制，并内置了一套用于处理可恢复错误的、基于类型系统的机制。

## 返回值 (Return Values)

使用 `-> ReturnType` 在函数签名中指定返回值的类型。

-   **隐式返回**: 如果函数体的最后一个表达式的类型与 `ReturnType` 兼容，该表达式的值将自动作为返回值，无需 `return` 关键字。
    ```flurry
    fn double(x: i32) -> i32 {
        x * 2 -- 表达式 x * 2 的值被隐式返回
    }
    ```
-   **显式返回**: 使用 `return` 关键字可以从函数体的任何位置提前返回一个值。
    ```flurry
    fn find_char(s: String, target: char) -> ?usize { -- 返回 ?usize
        for (i, c) in s.chars().enumerate() {
            return i if c == target; -- 找到，提前返回 Some(index)
        }
        null -- 循环结束都没找到，返回 null
    }
    ```
-   **`void` 返回类型**: 如果函数不返回有意义的值，返回类型为 `void`。可以省略 `-> void`。
    ```flurry
    fn print_message(msg: String) { -- 隐式返回 void
        println("{}", msg);
    }
    ```

## 错误处理 (`!Errors T`)

Flurry 提供了一种内置的、类型安全的方式来处理函数中可能发生的可恢复错误，避免了 C 的错误码、Java 的检查异常或 Go 的显式 `if err != nil` 的某些缺点。

**`!Errors T` 返回类型**:

-   当一个函数可能成功并返回 `T` 类型的值，或者失败并返回多种可能的错误类型之一时，可以使用 `!Errors T` 作为返回类型。
-   `Errors`: 通常是一个**编译时已知的类型列表**（或单个枚举类型），代表该函数可能返回的所有错误类型。

**语法:**

```flurry
-- 可能返回 u32，或 FileNotFoundErr, 或 PermissionDenied 错误
fn read_config_value(path: String) -> ![FileNotFoundErr, PermissionDenied] u32 {
    -- ... 函数体 ...
}
```

**错误传播 (`expr!`)**:

当调用一个返回 `!Errors T` 类型的函数时，可以使用后缀操作符 `!` 简洁地传播错误。

-   如果 `expr` 的结果匹配 `value!`，`expr!` 会解包得到 `value`。
-   如果 `expr` 的结果匹配 `error e`，`expr!` 会**立即从当前函数返回**这个 `e` 值。**前提是当前函数的返回类型也必须是 `!Errors' T'`，并且 `Errors'` 包含 `e'type`（或者可以自动转换/融合）**。

```flurry
struct Config { {- ... -} }

fn parse_config(content: String) -> !ParseErr Config { {- ... -} }
fn read_file(path: String) -> !IoErr String { {- ... -} }

pub const Errors = [IoErr, ParseErr, NetworkErr];
-- load_config 可能返回
fn load_config(path: String) -> !Errors Config {
    -- read_file返回值的错误负载必须是Errors的子集或元素
    let content = read_file(path)!;
    let config = parse_config(content)!;

    config
}
```
`!` 操作符极大地简化了错误传递的样板代码。

**错误处理 (`expr! { ... }`)**:

如果你想在错误发生的地方**立即处理**它，而不是直接传播，可以使用 `expr! { ... }` 块。

-   它允许你为 `expr` 可能返回的不同错误类型提供匹配分支。

```flurry
fn get_data_or_default(source: DataSource) -> Data {
    let result = source.fetch_data()! { -- 处理 fetch_data 可能的错误
        .NetworkErr(err) => log("Network failed: {}", err),
        .AuthErr => {
            source.re_authenticate()! {
                {- ... -}
            }
        },
        _ as e => log("Unhandled fetch error: {}", e'tag_name),
        catch e => return Data.default(),
    }
    result
}
```

**自动枚举融合**:

当一个函数调用多个可能返回不同错误集的函数时，编译器可以**自动推导**并**融合**所有可能的错误类型，形成当前函数签名所需的 `Errors` 集合。详见[自动枚举融合](advanced/error_handling/fusion.md)。