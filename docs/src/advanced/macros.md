## 宏系统

Flurry 不仅仅提供强大的编译时计算能力，还配备了一套层次丰富、功能强大的**宏系统 (Macro System)**。宏允许开发者在编译过程的不同阶段介入，对代码进行转换、生成和分析，极大地扩展了语言的表达能力和抽象能力。Flurry 的宏系统旨在提供从简单的文本替换到复杂的、基于语法的代码生成的全方位元编程支持。本章将详细介绍 Flurry 提供的五种主要宏类型。

### 1. 模板宏 (Template Macros)

**概念:**

模板宏是最基础的宏形式，其行为类似于 C/C++ 预处理器中的 `#define`，主要用于执行简单的**词法单元 (Token)** 替换。它们在**词法分析 (Lexical Analysis)** 阶段早期生效，因此其操作对象是原始的 token 流，而非结构化的语法树。

**特性:**

*   **生效阶段**: 词法分析阶段。
*   **输入/输出**: 接收 token 或 token 序列，输出替换后的 token 序列。
*   **作用域**: 模板宏的定义**仅在当前文件内有效**，且必须在使用之前定义（词法顺序）。
*   **定义方式**: 使用 `define` 关键字。

**语法形式:**

Flurry 提供了几种 `define` 形式以适应不同的替换场景：

1.  **映射单个 Token 块**:
    ```flurry
    define <macro_name>(<param_name>) { <replacement_tokens> }
    ```
    将调用时括号内的 token 序列（作为一个整体，绑定到 `<param_name>`）替换为 `<replacement_tokens>`。

    ```flurry
    -- 定义一个宏，尝试将输入的 token 序列转换为整数
    define get_int(tokens) {
        -- $tokens 代表调用时括号内的完整 token 序列
        ($tokens).value.*.to_int()?
    }

    let js_object = ... ;
    -- 调用宏，js_object 被绑定到 tokens 参数
    let value = get_int(js_object);
    -- 展开后: let value = (js_object).value.*.to_int()?;
    ```

2.  **映射固定数量的后续 Token**:
    ```flurry
    define <macro_name> <N> { <replacement_tokens> }
    ```
    将紧跟在宏名称后的 `<N>` 个 token 替换为 `<replacement_tokens>`。在替换体中，可以使用 `$1`, `$2`, ..., `$N` 来引用捕获的第 n 个 token。

    ```flurry
    -- 定义一个宏，用于生成 getter 方法
    define getter 2 { -- 捕获后续 2 个 token
        -- $1 是第一个 token (方法名), $2 是第二个 token (字段名)
        fn $1(*self) {
            self.$2
        }
    }

    struct Student {
        name: String,
        age: u32,

        -- 应用宏
        $getter get_name name
        $getter get_age age

        -- 展开后: 
        -- fn get_name(*self) { self.name }
        -- fn get_age(*self) { self.age }
    }
    ```

3.  **映射后续 Token 块 (按组)**:
    ```flurry
    define <macro_name> ...<N> { <replacement_tokens_per_group> }
    ```
    将宏名称后花括号 `{}` 内的所有 token，按照每 `<N>` 个一组进行分组，对每一组应用 `<replacement_tokens_per_group>` 进行替换。在替换体中，同样使用 `$1` 到 `$N` 引用组内的 token。

    ```flurry
    enum HttpStatus {
        .pattern_defined true,
        .base_type u32,

        -- 定义宏，每 2 个 token 为一组进行处理
        define status ...2 { $1: $2, } -- $1 是状态名, $2 是状态码

        -- 应用宏
        $status {
            ok 200
            not_found 404
            internal_server_error 500
            unauthorized 401
            forbidden 403
        }
        -- 展开后:
        -- ok: 200,
        -- not_found: 404,
        -- internal_server_error: 500,
        -- unauthorized: 401,
        -- forbidden: 403,
    }
    ```

**适用场景:** 模板宏适用于非常简单的、模式化的代码替换，例如定义常量别名、生成简单的重复代码结构（如 getter）或简化字面量列表的编写。由于它们在词法阶段工作，无法理解语法结构，因此不适用于复杂的代码转换。

### 2. 一类与二类构造宏 (Construction Macros: Type 1 & Type 2)

**概念:**

构造宏是 Flurry 宏系统中更强大的成员，它们在**语法分析 (Parsing)** 阶段生效。与模板宏处理原始 token 不同，构造宏能够理解和操作**语法结构**。它们由**编译时 Flurry (`comptime flurry`)** 代码定义，在语法分析过程中被编译器调用执行，其输出会替换原来的宏调用点。Flurry 提供了两种主要的构造宏类型：

*   **一类构造宏 (Macro Type 1)**: 直接操作 Flurry 抽象语法树 (AST)。
*   **二类构造宏 (Macro Type 2)**: 使用 PEG 解析器定义自定义语法，然后操作该自定义语法树。

**`TokenBuffer`：宏的输入与输出媒介**

在讨论构造宏之前，需要了解 `TokenBuffer`。它是 Flurry 宏系统（特别是构造宏和后续类型）用于传递代码片段的核心数据结构，其本质是一个Token序列。

*   **构造**: 使用双花括号 `{{ ... }}` 语法构造一个 `TokenBuffer`。
*   **插值**: 可以在 `{{ ... }}` 内部使用 `$` 符号插入实现了 `TryInto<meta.ast.TokenBuffer>` 的值。这包括：
    *   Flurry AST 节点 (来自 `meta.ast` 模块)。
    *   用户通过 PEG 定义的新语法树节点。
    *   另一个 `TokenBuffer`。
    编译器会自动处理将这些值“降级”或序列化回 token 流的过程。

**一类构造宏 (`macro1`)**

*   **输入**: Flurry **抽象语法树 (AST)** 节点列表 (具体类型取决于宏调用的上下文和宏定义)。
*   **处理**: 由一个 `comptime fn` 实现，接收 AST 节点列表作为参数。
*   **输出**: 返回一个 `TokenBuffer`。
*   **定义**: 使用`meta.macro1`宏定义一个一类构造宏。
*   **调用**: 通常使用 `@macro_name(...)` 或类似语法，括号内的内容会被解析为 Flurry AST 并传递给宏。

```flurry
-- @macro_name(arg1, arg2, ...) 调用语法
const pattern_matches = macro1' {
    -- 接收一个 Pattern 节点列表，每个节点代表一个 Flurry Pattern
    fn(...patterns: Repetition<meta.ast.FlurryPattern>) -> TokenBuffer {
        -- 使用 Flurry 的 comptime list 操作构建组合模式
        let combined_pattern_ast = patterns.fold({{ not _ }}, |acc_ast, pattern_ast| {{ $acc or $pattern_ast }});

        -- {{ $ast_node }} 会将 AST 节点序列化回 token
        {{ $combined_pattern_ast => }} -- 生成 `pattern =>` 的 token 序列
    }
}

test {
    let word = "yes";
    if word is {
        -- @ 调用宏，"yes", "Y" 等会被解析为 Flurry String Literal Pattern AST
        @pattern_matches("yes", "Y", "y", "\n") println("yes"),
        -- 展开后: not _ or "yes" or "Y" or "y" or "\n" => println("yes"),
        @pattern_matches("no", "N", "n") println("no"),
        _ => println("unknown"),
    }
}
```

**二类构造宏 (`macro2`)**

*   **输入**: 宏调用点花括号 `{}` 内的**原始 `TokenBuffer`**。
*   **PEG 解析**: 宏定义需要**提供一个 PEG (Parsing Expression Grammar)** 规则来解析输入的 `TokenBuffer`。
    *   **PEG 简介**: PEG 是一种形式化的语法表示方法，特别适合用于描述编程语言语法和构建解析器。与传统的上下文无关文法 (CFG) 不同，PEG 的选择操作符 `/` 是**有序选择 (prioritized choice)**，它消除了文法的歧义性，使得解析过程更直接（通常是递归下降）。PEG 由一系列**解析表达式 (Parsing Expressions)** 组成，用于精确定义如何匹配和消耗输入序列。常见的 PEG 组合子包括：
        *   **字面量 (`'text'`)**: 匹配精确的文本。
        *   **字符类 (`[a-z]`)**: 匹配范围内的字符。
        *   **序列 (`e1 e2`)**: 顺序匹配 `e1` 和 `e2`。
        *   **有序选择 (`e1 / e2`)**: 优先尝试匹配 `e1`，如果失败则尝试 `e2`。
        *   **重复 (`e*`, `e+`, `e?`)**: 匹配零次或多次、一次或多次、零次或一次。
        *   **谓词 (`&e`, `!e`)**: 检查是否能匹配 `e`（`&e`，正向预测）或不能匹配 `e`（`!e`，负向预测），但不消耗输入。
*   **处理**: 宏定义的 `comptime fn` 接收由 PEG 解析输入 `TokenBuffer` 后生成的**自定义语法树**作为参数。
*   **输出**: 返回一个 `TokenBuffer`。
*   **定义**: 使用 `' { peg_rule; fn(parsed_ast) -> TokenBuffer { ... } }` 语法，并标记为 `macro2`。`peg_rule` 可以是内联定义的 PEG，也可以引用预定义的 PEG 规则（如 Flurry 内置的 `FlurryExtendedStatement`）。
*   **调用**: 使用 `expr' { ... }` 语法。花括号内的内容将作为 `TokenBuffer` 传递给宏。

```flurry
use meta.ast.*;
const time = macro2' {
    -- 使用类型编码peg规则
    -- newtype KPrint = Keyword<"print">,
    -- newtype MyPrint = KPrint ~ FlurryExpression,
    -- 常见的组合子有Alternative, Sequence, Repetition, Optional, `~`为Sequence的语法糖
    
    -- 复用已有的语句定义，并且是拓展语句（不只是简单语句）
    newtype Main = Repetition<FlurryExtendedStatement>,
    
    
    fn(statements: Repetition<meta.ast.FlurryExtendedStatement>) -> TokenBuffer {
        -- 可以在处理脚本中进行一些操作
        -- 通过code template来插入语法树，自动根据插入的语法树类型lower到token buffer
        {{
            let __start = Duration.now();
            $statements
            let __end = Duration.now();
            analyze(__start, __end);
        }}
    }
}

test `time macro` {
    let x = 0;
    -- 调用宏，花括号内的代码作为 TokenBuffer 输入
    time' {
        for i in 0..1000000 {
            x += i;
        }
        println("Loop finished"); -- 可以包含多条语句
    }
    -- 展开后 (概念上):
    -- {
    --     let __start = std.time.Instant.now();
    --     for i in 0..1000000 {
    --         x += i;
    --     }
    --     println("Loop finished");
    --     let __end = std.time.Instant.now();
    --     std.debug.analyze_duration(__start, __end);
    -- }
}
```

**适用场景:** 构造宏非常强大，适用于需要理解和转换代码结构的任务：

*   **一类宏**: 适用于你想直接操作标准 Flurry 语法结构的情况，例如分析或重组现有的 Flurry 代码模式。
*   **二类宏**: 极其适合创建**嵌入式领域特定语言 (Embedded DSLs)**。你可以定义自己的迷你语言语法（通过 PEG），然后将其无缝嵌入到 Flurry 代码中，宏负责将 DSL 解析并转换为底层的 Flurry 实现代码。`time' { ... }`、`query_one' { ... }` 都是二类宏的绝佳应用。

### 3. 三类构造宏 (`macro3`)

*   **输入**: 宏调用点花括号 `{}` 内的**原始 `TokenBuffer`**。
*   **处理**: 由一个标记了 `^macro3` 的 `comptime fn` 实现，该函数直接接收 `TokenBuffer`。它**不使用 PEG 解析**，而是直接对 token 流进行操作（遍历、替换等）。
*   **输出**: 返回一个处理后的 `TokenBuffer`。

```flurry
-- 定义一个三类宏，将输入 token 中的标识符转为大写
^macro3 -- 属性标记这是一个三类宏? 或者有其他机制?
comptime fn to_uppercase_macro(tokens: TokenBuffer) -> TokenBuffer {
    tokens.foreach(|t| if t is .id(str) {
        Token.id(str.to_uppercase()) -- 将标识符转换为大写
    } else {
        t
    })
}

-- 调用宏
to_uppercase_macro' {
    const message = "hello world"; -- message 会变成 MESSAGE
    let variable_name = 1;        -- variable_name 会变成 VARIABLE_NAME
}
-- 展开后 (概念上，只改标识符):
-- const MESSAGE = "hello world";
-- let VARIABLE_NAME = 1;
```

**适用场景:** 三类宏适用于需要对 token 流进行**低级别操作**的场景。

### 4. 四类构造宏 (`macro4`)

*   **输入**: 宏调用点花括号 `{}` 内的**原始源码字符串 (String)**，完全忽略 Flurry 的词法和语法规则。
*   **处理**: 由一个标记了 `^macro4` 的 `comptime fn` 实现，接收 `String` 作为输入。函数内部负责解析这个字符串（可以使用自定义解析器、外部工具等）。
*   **输出**: 返回一个 `TokenBuffer`，该 `TokenBuffer` 必须包含**语法上有效的 Flurry 代码**，后续会被 Flurry 编译器正常处理。

```flurry
-- 定义一个四类宏，处理自定义的多行字符串格式
^macro4
comptime fn multi_line_string(input: String) -> TokenBuffer { ... }

let lyris = multi_line_string' {
    say you, say me,
    say it for always,
    that s the way it should be.
}
```

**适用场景:** 四类宏是**终极武器**，用于处理那些**完全不符合 Flurry 标准语法或词法规则**的输入。它允许你：

*   完全实现自己的lexer、parser
*   嵌入对**格式敏感**（如缩进）的 DSL。
*   处理包含大量特殊字符、不需要 Flurry 转义的文本块（类似 `builtin.raw_str` 但带有自定义解析）。
*   直接从其他格式（如 CSV, JSON, YAML 等，如果需要内联且不想用运行时库）生成 Flurry 代码。

**总结**

Flurry 的宏系统提供了一个从简单到复杂的**分层结构**，允许开发者在编译过程的不同阶段、以不同的抽象层次（Token, Flurry AST, 自定义 AST, 原始字符串）进行元编程：

*   **模板宏**: 快速简单的词法替换。
*   **一类构造宏**: 基于 Flurry AST 的结构化转换。
*   **二类构造宏**: 基于 PEG 的自定义 DSL 解析和代码生成。
*   **三类构造宏**: 基于 TokenBuffer 的底层转换。
*   **四类构造宏**: 基于原始字符串的完全自定义解析和代码生成。

这套强大的宏系统，结合 Flurry 的 `comptime` 计算能力，使得开发者能够极大地提升代码的抽象层次、减少样板代码，并创建出富有表现力的嵌入式 DSL，是 Flurry 语言核心竞争力的重要组成部分。理解并恰当选用不同类型的宏，将是 Flurry 高级编程的关键。
