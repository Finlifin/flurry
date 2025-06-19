# 快速入门

本章旨在让您快速领略 Flurry 语言的风采。我们将通过一个简单的示例展示 Flurry 的一些核心特性，并指导您完成基本的安装和运行流程。我们的目标不是详尽解释所有细节，而是让您对 Flurry 的编程体验有一个初步的感受。

## "Hello, Infrastructure!" - 一个 Flurry 示例

让我们来看一个稍微超越传统 "Hello, World!" 的例子，它将触及 Flurry 的编译时计算和可选类型：

```flurry
-- src/main.fl

-- 使用标准库的打印功能
use std.io.println;
-- 使用构建信息模块 (假设存在)
use build;

-- 定义一个编译时常量，存储构建目标操作系统
const TARGET_OS: str = comptime build.target.os'tag_name; -- 取像操作获取 tag 名

-- 程序入口点 main 函数
pub fn main() -> void {
    let user_name: ?String = get_user_from_env(); -- 返回可选类型 ?String

    -- 编译时条件分支 (inline if)
    inline if TARGET_OS == "windows" {
        println("Detected Windows!");
    } else if TARGET_OS == "linux" {
        println("Running on Linux!");
    } else {
        println("Running on an unknown OS: {str}", TARGET_OS); -- 字符串插值
    }

    -- 处理可选类型
    if user_name is {
        some name => { -- 模式匹配解构 Some
            println("Hello, {str}!", name); -- {str} 是格式化说明符
        }
        null => { -- 匹配 null
            println("Hello, anonymous user!");
        }
    }
}

-- 假设的函数，从环境变量获取用户名，可能失败
fn get_user_from_env() -> ?String {
    -- ... (实际实现会读取环境变量) ...
    -- 模拟可能找到或找不到
    if std.random.bool() { -- 假设有随机库
         "FlurryDev".to_string() -- 返回 String，自动提升为 ?String
    } else {
         null -- 返回 null
    }
}

```

**这个简单的例子展示了:**

*   **模块导入 (`use`)**: 如何引入外部模块的功能。
*   **编译时计算 (`comptime`)**: 如何在编译时获取信息（如构建目标）并将其用于代码逻辑。
*   **取像操作 (`'tag_name`)**: 如何获取一个值（这里是编译时符号）的元信息。
*   **编译时条件 (`inline if`)**: 如何根据编译时条件选择性地包含代码。
*   **可选类型 (`?String`)**: 如何表示可能不存在的值。
*   **模式匹配 (`if is { some => ..., null => ... }`)**: 如何安全地处理可选类型。
*   **字符串插值 (`"{str}"`, `$variable`)**: (假设 `${}` 用于编译时插值，`{str}` 用于运行时格式化)。
*   **自动类型提升**: `String` 和 `null` 如何根据函数返回类型 `?String` 自动提升。

## 核心特性概览 (亮点速览)

除了上面的例子，Flurry 还拥有更多令人兴奋的特性：

*   **代数效应 (Algebraic Effects)**: 以结构化、可组合的方式处理 IO、并发、错误等副作用。
    ```flurry
    effect Ask(question: String) -> String; -- 定义效应
    fn greet() -> #Ask void { -- 标记函数可能发出 Ask 效应
        let name = perform Ask("What's your name?"); -- 发出效应
        println("Hello, {str}!", name);
    }
    greet() # { -- 提供 Handler
        Ask(q) => { println("Answering: {str}", q); resume "Flurry"; } -- 处理效应并恢复
    }
    ```
*   **强大的模式匹配**: 超越简单的值匹配，支持解构、范围、守卫、类型匹配等。
    ```flurry
    match response {
        { status: 200, body } => process_ok(body),
        { status: 404 } => log_not_found(),
        { status: 4xx } if is_client_error(status) => handle_client_error(status), -- 范围/通配符与守卫
        _ => handle_other_error(),
    }
    ```
*   **`expr object` DSL 构建**: 以极其声明式的方式构建复杂数据结构。
    ```flurry
    -- (之前 Web API 示例中的查询或响应构建)
    let query = Product.query {
        .limit 10,
        .sort "price",
        .filter <{ category: "books", price: ..100.0 }>, -- <{...}> 模式字面量
    }
    ```
*   **`extend` 与字面量拓展**:
    ```flurry
    extend u64 { fn kb(self) -> usize { self * 1024 } }
    let buffer_size = 4kb; -- 等于 4 * 1024
    ```
*   **类型即代码 (`comptime`)**: 在编译时操作类型、生成代码。
    ```flurry
    -- 生成特定大小的数组类型
    comptime let MyArrayType = Type.Array(i32, 10);
    let arr: MyArrayType = uninitialized;
    ```

这仅仅是冰山一角！Flurry 的设计充满了各种精心设计的特性，旨在让系统编程更安全、更高效、更富有表现力。

## 安装与运行 (暂定)

*TODO: 在此部分添加关于如何获取 Flurry 编译器/工具链（例如，下载预编译版本、从源码构建）、设置开发环境以及编译运行第一个示例（`hello_infrastructure`）的具体步骤。*

例如：

1.  **下载**: 前往 [Flurry 官方网站/仓库](link-to-flurry-repo-or-website) 下载最新的编译器。
2.  **安装**: 解压或运行安装程序，并将 Flurry 可执行文件路径添加到系统 `PATH`。
3.  **验证**: 打开终端，运行 `flurry --version` 确认安装成功。
4.  **创建项目**: `mkdir hello_flurry && cd hello_flurry`
5.  **创建 `src/main.fl`**: 将上面的示例代码粘贴进去。
6.  **创建 `package.fl`** (最简形式，假设需要)：
    ```flurry
    -- package.fl
    { .name "hello_flurry", .type .exe }
    ```
7.  **编译**: `flurry build`
8.  **运行**: `.build/hello_flurry` (或 Windows 上的 `.build\hello_flurry.exe`)

(以上步骤是假设性的，需要根据实际的工具链进行调整。)

现在，您已经对 Flurry 有了初步的认识。准备好深入探索了吗？接下来的章节将带您详细了解 Flurry 的各项核心特性。
