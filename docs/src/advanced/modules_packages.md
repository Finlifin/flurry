# 模块系统与包管理

### 1. 包 (Package) 的类型

Flurry 中的包 (Package) 主要分为两种类型，通过 `package.fl` 描述文件中的配置来区分：

- **可执行包 (Executable Package, `exe`)**:
    - 用于构建可执行程序。
    - **根文件：** 必须在 `src` 目录下包含 `main.fl` 文件，作为程序的入口点。
    - **模块组织：** `src` 目录下的子文件夹可以使用 `mod.fl` 文件作为模块入口，通过 `mod file` 语句递归地构建整个源码的虚拟文件系统 (VFS)。
- **库包 (Library Package, `lib`)**:
    - 用于构建可重用的代码库。
    - **根文件：** 必须在 `src` 目录下包含 `lib.fl` 文件，作为库的入口点。
    - **模块组织：** 模块组织方式与可执行包相同，使用 `mod.fl` 和 `mod file` 语句。

### 2. 包目录结构 (Package Directory Structure)

一个 Flurry 包通常具有以下目录结构：

```
/ (Package Root Directory)
├── src/            -- 源代码目录 (Source Code Directory)
│   ├── main.fl     -- (可执行包) 程序入口根文件 (Executable Package Entry Point)
│   │   或
│   ├── lib.fl      -- (库包) 库入口根文件 (Library Package Entry Point)
│   ├── <模块子目录>/ (Module Subdirectories)
│   │   └── mod.fl  -- 模块入口文件 (Module Entry Point)
│   └── ...         -- 其他 .fl 源代码文件
├── package.fl    -- 包描述文件 (Package Manifest File)
├── docs/            -- (可选) 文档目录 (Documentation Directory, Optional)
├── .build/         -- (可选) 构建输出目录 (Build Output Directory, Optional)
├── script/         -- (可选) 脚本目录 (Scripts Directory, Optional)
├── resources/      -- (可选) 资源文件目录 (Resources Directory, Optional)
└── ...             -- 其他包相关文件

```

- **`src/`**: **源代码目录 (Source Code Directory)**
    - **`main.fl` (可执行包) / `lib.fl` (库包)**: 包的根文件，必须存在于 `src` 目录下。
    - **`<模块子目录>/mod.fl`**: 用于组织模块的子目录，每个子目录下的 `mod.fl` 文件定义一个模块。
    - **`.fl` 文件**: Flurry 源代码文件，使用 `.fl` 扩展名。
- **`package.fl`**: **包描述文件 (Package Manifest File)**
    - 使用 `comptime flurry` 语法定义包的元数据、依赖项、构建配置等信息。 (具体 `package.fl` 的内容和格式暂定)。
- **`docs/` (可选)**: **文档目录 (Documentation Directory)**
    - 用于存放包的文档文件 (例如，Markdown 格式的文档)。
- **`.build/` (可选)**: **构建输出目录 (Build Output Directory)**
    - 用于存放编译生成的二进制文件、库文件、中间文件等。 (通常是构建工具自动创建和管理的目录，例如 `.git` 目录类似)
- **`script/` (可选)**: **脚本目录 (Scripts Directory)**
    - 用于存放与包相关的脚本文件 (例如，宏脚本、构建脚本、测试脚本等)。这部分也许可以完全使用comptime flurry完成。
- **`resources/` (可选)**: **资源文件目录 (Resources Directory)**
    - 用于存放程序运行所需的资源文件 (例如，配置文件、图片、文本文件等)。

### 3. 模块组织 (Module Organization)

Flurry 使用 **模块 (Module)** 来组织代码，实现代码的封装和命名空间管理。

- **模块入口文件**: `mod.fl` 文件是模块的入口点。
- **模块层级结构**: 通过在子目录下创建 `mod.fl` 文件，可以构建模块的层级结构。
- **`mod file` 语句**: 在 `mod.fl` 文件中使用 `mod file` 语句，可以声明模块包含的子模块或源代码文件，从而构建模块的虚拟文件系统 (VFS)。 ( `mod file` 语句的具体语法和用法将在后续章节详细介绍)。

**示例：模块层级结构**

假设我们有一个名为 `my_package` 的可执行包，其 `src` 目录结构如下：

```
src/
├── main.fl         -- 可执行包入口
├── utils/
│   ├── mod.fl      -- utils 模块入口
│   └── string.fl   -- utils.string 模块
└── network/
    ├── mod.fl      -- network 模块入口
    ├── http.fl     -- network.http 模块
    └── tcp.fl      -- network.tcp 模块

```

- `src/main.fl`: 定义可执行包的入口代码。
- `src/utils/mod.fl`: 定义 `utils` 模块，`utils` 模块可能在 `mod.fl` 中声明包含 `string.fl` 文件。
- `src/utils/string.fl`: 定义 `utils.string` 模块，提供字符串相关的工具函数。
- `src/network/mod.fl`: 定义 `network` 模块，`network` 模块可能在 `mod.fl` 中声明包含 `http.fl` 和 `tcp.fl` 文件。
- `src/network/http.fl`: 定义 `network.http` 模块，提供 HTTP 相关的网络功能。
- `src/network/tcp.fl`: 定义 `network.tcp` 模块，提供 TCP 相关的网络功能。

### 4. 包描述文件 `package.fl` (Package Manifest File)

`package.fl` 文件是 Flurry 包的 **描述文件 (Manifest)**，用于配置包的各种属性，例如：

- **包类型 (Package Type):** `exe` (可执行包) 或 `lib` (库包)。
- **包名称 (Package Name)**
- **版本 (Version)**
- **依赖项 (Dependencies)**
- **构建配置 (Build Configuration)**
- **作者 (Author)**
- **许可证 (License)**
- ...

`package.fl` 文件使用 **`comptime flurry`**  定义，这意味着包的元数据信息在 **编译时** 就是可用的，可以用于编译时的代码生成、条件编译等高级特性。

**示例：`package.fl` 文件内容 (示例)**

```
-- 暂定
```