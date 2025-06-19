# 模块 (Modules)

在 Flurry 中，每个类型都有独立命名空间，所以都可以被视为模块。模块的成员可以是其他模块、函数、常量、类型、全局变量等。
而`mod`类型则是专门用于表示模块的类型。每个文件都是一个模块，每个目录也是一个模块，其内容由目录下的`mod.fl`文件定义。

**模块定义:**

模块使用 `mod` 关键字定义，通常与文件系统结构相关联（例如，一个目录对应一个模块，`mod.fl` 作为入口文件）。

```flurry
-- src/utils/mod.fl
mod string; -- 使用 'mod' 语句引入同目录下的 string.fl
mod math;   -- 引入 math.fl

pub fn helper_function() { {- ... -} }

const VERSION = "1.0";

-- src/utils/string.fl
pub fn trim(s: String) -> String { {- ... -} }

-- src/utils/math.fl
pub fn add(a: i32, b: i32) -> i32 { a + b }

-- --- main.fl ---
mod utils; -- 引入 src/utils/ 目录下的模块

test {
    -- 像访问类型成员一样访问模块成员
    let version = utils.VERSION;
    utils.helper_function();
    let sum = utils.math.add(1, 2);
    let trimmed = utils.string.trim(" hello ".to_string());
}
```

**总结**

Flurry 将模块提升到了与 `struct`, `enum` 等类似的“类型”地位，使其成为编译时可知、可包含各种成员的命名空间实体。这种设计统一了成员访问语法，并为强大的编译时元编程能力（如模块反射）奠定了基础。