# 扩展 (extension) 与字面量拓展

虽然孤儿原则对于维护大型项目的一致性至关重要，但在某些情况下，我们可能希望**临时性地**或者在**受限的作用域内**为一个类型添加某个 Trait 的行为，即使这违反了孤儿原则。Flurry 提供了 `extend` 关键字来实现这种受控的扩展。

`extend` 机制还巧妙地被用作 Flurry **字面量拓展**的基础。

**`extend` 关键字**

`extend Trait for Type` 语法允许你在任何作用域内为一个（可能是外部定义的）类型 `Type` 提供一个（可能是外部定义的） `Trait` 的实现。

```flurry
mod external_lib {
    pub trait Printer {
        fn print_info(*self);
    }
    pub struct Data { pub value: i32 }
}

mod my_code {
    use external_lib.{ Printer, Data }

    -- 在 my_code 模块内，为外部类型 Data 实现外部 Trait Printer
    -- 这违反了孤儿原则，但 extend 允许这样做
    extend Printer for Data {
        fn print_info(*self) {
            println!("My custom print for Data: value = {}", self.value);
        }
    }

    pub fn process_data(d: Data) {
        -- 在这个作用域内，调用 print_info 会使用上面的 extend 实现
        d.print_info();
    }
}

mod another_code {
     use external_lib.{ Printer, Data }
     -- 在这个模块，Printer for Data 的 extend 没有被导入，
     -- 所以无法调用 d.print_info() (除非 external_lib 本身提供了 impl)
     pub fn process_data_again(d: Data) {
         -- d.print_info(); -- 编译错误 (假设 external_lib 无 impl)
         println("Processing data without custom print.");
     }
}
```

**`extend` 的关键特性:**

1.  **绕过孤儿原则**: 允许在任何地方定义实现。
2.  **作用域限制**: `extend` 定义的实现**仅在其被定义的词法作用域内有效**。外部代码默认不受此影响。
3.  **遮盖 (Shadowing)**: 如果在同一个作用域内，同时存在一个类型的常规 `impl` 和一个 `extend` 实现，那么**`extend` 的实现会优先被使用**，它会“遮盖”住 `impl` 的实现。
4.  **可命名与导入**: 可以将 `extend` 块赋予一个常量标识符，然后其他模块可以通过 `use` 显式导入这个命名扩展，从而将该扩展实现引入到自己的作用域中。

    ```flurry
    mod extensions {
        use external_lib.{ Printer, Data }
        -- 将扩展命名为 PrettyPrinterForData
        pub const PrettyPrinterForData = extend Printer for Data {
             fn print_info(*self) {
                 println!("*** Pretty Data: {} ***", self.value);
             }
        }
    }

    mod user_code {
        use external_lib.Data;
        use extensions.PrettyPrinterForData; -- 显式导入扩展

        pub fn main() {
            let d = Data { .value 42 }
            -- 因为导入了扩展，这里会使用 PrettyPrinterForData 的实现
            d.print_info(); -- 输出: *** Pretty Data: 42 ***
        }
    }
    ```
    这种显式导入机制提供了精确的作用域控制。

**`extend` 用于字面量拓展**

Flurry 利用 `extend` 机制实现了一种强大且统一的**字面量拓展**功能。当你编写类似 `10px` 或 `"hello"c` 的代码时，编译器实际上是在查找一个为该字面量对应的内置类型（如 `Integer`, `u32`, `str`）定义的、包含名为 `px` 或 `c` 的方法的 `extend` 块。

**原理:**

`literal + identifier` 形式会被编译器尝试解析为 `literal.identifier()` 的方法调用。这个方法必须由一个在当前作用域内有效的 `extend` 块提供。

```flurry
-- 为内置类型 u32 扩展一个 px 方法
extend u32 {
    -- 'px' 方法接收 u32 值，返回 Pixel 类型
    comptime fn px(self) -> Pixel {
        Pixel { .value self } -- 假设 Pixel struct 已定义
    }
}

-- 为内置 str 类型扩展一个 cstr 方法
extend str {
    -- 'cstr' 方法接收编译时 str，返回某种 C 兼容字符串表示
    comptime fn cstr(self) -> CString { -- 假设 CString 类型存在
        -- ... 编译时或运行时逻辑来创建 CString ...
        CString.from_str_with_null(self)
    }
}

test {
    let length = 100px; -- 等价于 (100).px()
    asserts length.value == 100;

    let c_string = "api_key"cstr; -- 等价于 ("api_key").cstr()
    -- c_string 现在是一个 CString 实例
}
```

-   这个机制允许开发者或库为基础类型添加与领域相关的单位或转换，使代码更具表现力。
-   语言可以内置一些常用的拓展，如 `"..."cstr` 用于 FFI，`"..."bytes` 用于创建字节序列。
-   这些 `extend` 实现同样遵循作用域规则，可以被局部定义或通过 `use` 导入。

**总结**

`extend` 是 Flurry 提供的一种受控方式，用于在标准 `impl` 和孤儿原则之外为类型添加行为。它的作用域限制和显式导入机制避免了全局冲突，同时为局部行为定制和实现优雅的字面量拓展提供了强大的支持。使用 `extend` 时需要注意其作用域和对代码可读性的潜在影响。
