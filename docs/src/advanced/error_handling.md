## 错误处理 —— 可预见且可组合的健壮性 (Error Handling: Predictable and Composable Robustness)

程序在现实世界中难免会遇到各种预期之外的情况——文件没找到、网络连接中断、用户输入格式错误等等。如何优雅、健壮地处理这些“错误”情况，是衡量一门编程语言成熟度的重要标准。Flurry 在这方面下足了功夫，它利用其富有表现力的类型系统和专门的语法，提供了一套**可预见且高度可组合**的错误处理方案。

**1. 表示“可能缺失”：可选类型 (`?T`)**

最简单的“错误”形式就是值的缺失。一个函数可能找到一个结果，也可能找不到。对于这种情况，Flurry 使用**可选类型 (Optional Type)**，在类型前加上问号 `?` 来表示：

```flurry
-- 查找用户，可能找到 (User)，也可能找不到 (null)
fn find_user(id: Uuid) -> ?User {
    -- ... 查询逻辑 ...
    if found {
        user -- 自动提升为 ?User (Some(user))
    } else {
        null -- 表示缺失
    }
}

test {
    let user_opt = find_user(some_id);
    -- 处理可选值通常用模式匹配
    if user_opt is {
        user? => println("Found user: {}", user.name),
        null => println("User not found."),
    }

    let user_name = find_user(some_id)?.name.unwrap_or("Unknown");
}
```

*   `?T` 类型表示一个值要么是 `T` 类型，要么是表示“无”的 `null`。
*   处理 `?T` 通常使用模式匹配 (`if is { some? => ..., null => ... }`) 来安全地访问其内部值。
*   这避免了 C/C++ 中空指针解引用的风险，强制开发者处理值可能缺失的情况。

**2. 表示“可能失败”：错误联合类型 (`!ErrorType ResultType`)**

当一个操作不仅可能缺失结果，还可能因为**特定原因**失败时，我们需要更精确地表达“哪种错误”发生了。Flurry 使用感叹号 `!` 前缀来定义**错误联合类型 (Error Union Type)**：

```flurry
-- 读取配置文件，可能成功返回 Config，也可能因 NetErr 失败
fn load_config_from_network(url: String) -> ![NetErr, ParseErr] Config {
    let response = http.get(url)!;
    parse(response)!; -- 可能失败
}

-- 调用点处理
let config_result = load_config_from_network("...")! {
    -- this catch branch runs before all other branches
    catch e => println("Error loading config: {}", e'tag_name),

    .ParseErr.RequiredFieldMissing(info) => println("Missing required field: {}", info),
    .NetErr.Timeout => println("Network timeout!"),
    .NetErr.ConnectionRefused => println("Connection refused!"),
    .NetErr.* => println("Other network error."),
    .ParseErr.* => println("Other parse error."),

    -- this catch branch runs after all other branches
    catch e => panic("Unhandled error: {}", e'tag_name),
}
```

**3. 组合多种错误：层级化枚举与枚举融合**

一个复杂的操作往往可能因为**多种不同来源**的错误而失败。例如，处理用户上传的文件可能遇到网络错误、文件系统错误、解析错误等等。为每种组合都定义一个新的错误类型会非常繁琐。Flurry 利用其强大的**层级化枚举**和**枚举融合**特性来优雅地解决这个问题：

*   **库定义各自的错误枚举**:
    ```flurry
    -- network_lib
    enum NetErr {
        Timeout,
        ConnectionRefused,
        InvalidResponse,
        Unauthorized,
        ...
    }
    -- filesystem_lib
    enum FsErr {
        NotFound(path: fs.Path),
        PermissionDenied(path: fs.Path),
        DiskFull(path: fs.Path),
        ...
    }
    -- parser_lib
    enum ParseErr {
        InvalidFormat(location: Location),
        RequiredFieldMissing(field: String),
        FieldTypeMismatch(field: String, expected: String, actual: String),
    }
    ```
*   **函数签名中使用错误列表**: 函数签名可以列出所有**可能**发生的错误类型。

    ```flurry
    -- 这个函数可能返回 Data，或者失败并返回 FsErr 或 ParseErr 中的任何一种
    fn process_local_file(path: String) -> ![FsErr, ParseErr] Data {
        let content = fs.read(path)!; -- '!' 传播 FsErr
        let data = parser.parse(content)!; -- '!' 传播 ParseErr
        data
    }
    ```

*   **融合类型消减规则 (Error Fusion Reduction)**: 设错误处理器可处理的效应集为A, 数据的效应集为B，则将处理器应用与计算后，数据剩余的错误集为B - A。也就是说，处理器会消解掉它所处理的错误，使得最终的计算结果不再携带这些错误。

    ```flurry
    fn complex_operation() -> ![FsErr, ParseErr, NetErr] Data {
        let path = network.download_path()!;
        let data = process_local_file(path)!;
        data
    }
    ```
    这个消减规则使得错误处理流更加精确和可静态分析。

**4. 为类型定义错误处理能力 (`handles error [...] -> A { ... }`)**

类似于代数效应的预制处理器，Flurry 也可能提供一种方式，让类型能够**预定义**处理**特定错误类型集合**的方法。这可以用于实现自定义的错误恢复策略或将错误转换为另一种形式。

*   **语法**:
    ```flurry
    struct SrcManager {
        -- 实际上，我们让这个处理器更像一个中间件
        handles error [FsErr, ParseErr] -> ParseErr {
            .FsErr.NotFound(path) => println("File not found: {}", path),
            .ParseErr.UnexpectedToken(span, expected, context, actual, note) =>
                self.report(
                    .err,
                    span,
                    fmt.format("unexpected token `{}`, found {}, when parsing {}", self.src_content(actual), expected, context),
                    note,
                    .extra_lines = 3,
                )
            ...
        }
    }

    test {
        -- ![ParseErr] Ast == !ParseErr Ast
        -- parse: fn(String) -> ![ParseErr] Ast
        -- parse(src).use(SrcManager): ![ParseErr] Ast
        -- parse(src).use(SrcManager)!: Ast
        let ast = parse(src).use(SrcManager)!;
    }
    ```

**总结**

Flurry 的错误处理系统巧妙地结合了多种机制，旨在实现类型安全、可组合和富有表现力的错误管理，避免了 C 的错误码、Java 的检查异常（带来的样板代码）和 Go 的 `if err != nil`（重复代码）的许多缺点，同时提供了比 Rust（需要手动定义错误枚举或使用 `anyhow`/`thiserror`）更原生的多错误组合能力。它使得编写健壮的系统级代码变得更加容易和可靠。
