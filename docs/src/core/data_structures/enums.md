# 枚举 (Enums)

枚举（Enum）是 Flurry 中用于定义一个“类型可以是多种可能变体之一”的数据结构。它允许你将一组相关的、但结构可能不同的数据类型聚合在一个统一的类型名下。

**定义枚举**

使用 `enum` 关键字定义枚举，并在花括号 `{}` 内列出其所有可能的**变体 (variants)**。每个变体可以：

-   只是一个简单的名字（像 C 语言的枚举）。
-   包含关联数据（像 Rust 或 Swift 的代数数据类型 ADT）。

```flurry
-- 简单的状态枚举
enum Status {
    ok,
    pending,
    error,
}

-- 带有数据的枚举 (表示不同形状)
enum Shape {
    circle(radius: f64),
    rectangle(width: f64, height: f64),
    point, -- 也可以有不带数据的变体
}

-- 层级化枚举 (之前讨论过)
enum AstNode {
    expr.{ -- 使用 '.' 定义层级
        literal(LiteralKind),
        binary_op(Op, *AstNode, *AstNode),
    },
    stmt.{
        let_binding(String, *AstNode),
        return_stmt(?*AstNode),
    },
}
```

**实例化枚举**

实例化枚举需要指定要创建的变体，并提供其所需的关联数据（如果有的话）。访问变体通常使用 `EnumName.VariantName` 的形式。

```flurry
let current_status = Status.pending;

let my_shape = Shape.rectangle(10.0, 5.5);
let origin_point = Shape.point;

let node = AstNode.expr.literal(LiteralKind.integer(10));
```

**模式匹配枚举**

枚举最常用的场景是与**模式匹配**（`match`, `if is`, `while is`）结合使用，以根据枚举实例当前持有的变体来执行不同的代码。

```flurry
fn process_status(status: Status) {
    if status is {
        .ok => println("Status is OK."),
        .pending => println("Status is Pending."),
        .error => println("Status is Error."),
        -- `match` 需要是穷尽的
    }
}

fn calculate_area(shape: Shape) -> f64 {
    if shape is {
        .circle(r) => 3.14159 * r * r, -- 解构出关联数据 r
        .rectangle(w, h) => w * h,   -- 解构出 w 和 h
        .point => 0.0,
    }
}

test {
    process_status(current_status); -- 输出: Status is Pending.
    let area = calculate_area(my_shape); -- area 会是 55.0
    println("Shape area: {}", area);
}
```

**枚举作为命名空间与关联函数**

与结构体类似，`enum` 定义本身也是一个命名空间，可以在其 `{}` 内部定义关联函数（方法）或静态函数。

```flurry
enum IpAddr {
    v4(u8, u8, u8, u8),
    v6(String), -- 简化表示

    pub fn display(*self) {
        if self is {
            .v4(a, b, c, d) => println("{}.{}.{}.{}", a, b, c, d),
            .v6(s) => println("{}", s),
        }
    }

    pub fn loopback_v4() -> Self {
        IpAddr.v4(127, 0, 0, 1)
    }
}

test {
    let home = IpAddr.loopback_v4();
    home.display(); -- 输出: 127.0.0.1
    let work = IpAddr.v6("::1".to_string());
    work.display(); -- 输出: ::1
}
```


**层级化与融合**

Flurry 支持**层级化枚举** (`enum.group.variant`) 和**枚举融合** (`meta.Enum.from(...)`)，提供了强大的组织和组合枚举的能力。（详见 [层级化与融合](enums/hierarchy_fusion.md)）

枚举是 Flurry 中表示“和或”类型（Sum Types）的关键工具，结合模式匹配，使得处理具有多种可能形态的数据变得安全和方便。