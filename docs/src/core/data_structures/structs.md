# 结构体 (Structs)

结构体（Struct）是 Flurry 中最基本的用户自定义数据类型之一。它允许你将多个不同类型的值组合在一起，形成一个有意义的、命名的复合类型。

**定义结构体**

使用 `struct` 关键字来定义结构体，并在花括号 `{}` 内声明其字段（成员变量）。每个字段都有一个名称和一个类型注解。

```flurry
struct Point {
    x: f64,
    y: f64,
}

struct Color {
    red: u8,
    green: u8,
    blue: u8,
}

struct UserProfile {
    username: String,
    email: String,
    is_active: bool,
    login_count: u64,
}
```

**实例化结构体**

定义了结构体后，你可以创建它的实例。Flurry 使用类似 `TypeName { .field1 value1, ... }` 的语法来初始化结构体实例，这正是我们之前讨论过的 "双变参调用" 语法的一种应用，这里没有传递 "children"，只传递了 "attributes"（字段）。

```flurry
let origin = Point { .x 0.0, .y 0.0 }

let white = Color { .red 255, .green 255, .blue 255 }

let user = UserProfile {
    .username "alice".to_string(),
    .email "alice@example.com".to_string(),
    .is_active true,
    .login_count 5,
}
```
字段初始化的顺序通常不重要，但必须提供所有非可选字段的值（除非它们有默认值，这需要语言支持）。

**访问字段**

使用点 (`.`) 操作符来访问结构体实例的字段。

```flurry
println("Origin point: ({}, {})", origin.x, origin.y); -- 输出: Origin point: (0.0, 0.0)

let user_email = user.email;
```
如果字段是可变的 (`mut`)，且实例绑定也是可变的 (`let ...`)，则可以修改字段的值：
```flurry
let user = user;
user.login_count += 1;
```

**结构体作为命名空间**

如前所述，`struct` 定义本身就是一个命名空间。你可以在 `struct` 定义的花括号内直接定义关联函数（通常称为**方法**）。方法通常接收 `*self` (指针), 或 `self` (获取所有权) 作为第一个参数，用于访问或修改实例数据。

```flurry
struct Rectangle {
    width: u32,
    height: u32,

    pub fn area(*self) -> u32 {
        self.width * self.height -- 通过 self 访问实例字段
    }

    pub fn square(size: u32) -> Self { -- Self 是类型的别名
        Rectangle { .width size, .height size }
    }
}

test {
    let rect = Rectangle { .width 10, .height 5 }
    println("Area: {}", rect.area()); -- 调用实例方法: 输出 Area: 50

    let sq = Rectangle.square(8);
    println("Square area: {}", sq.area()); -- 输出 Square area: 64
}
```
-   方法定义在 `struct { ... }` 内部。
-   使用 `.` 操作符调用实例方法 (`rect.area()`)。
-   使用 `.` 调用选取符号 (`Rectangle.square(8)`)。

**组合与委托 (`using`)**

结构体可以通过 `using` 关键字将内部字段的成员暴露出来，简化接口。

```flurry
struct Person {
    name: String,
    pub fn greet(*self) { println("Hello, I'm {}", self.name); }
}

struct Employee {
    person: Person,
    employee_id: u32,
    using person.*; -- 将 Person 的 greet 方法提升到 Employee
}

test {
    let emp = Employee {
        .person Person { .name "Bob".to_string() },
        .employee_id 123,
    }
    emp.greet(); -- 直接调用 greet，无需 emp.person.greet()
                 -- 输出: Hello, I'm Bob
}
```

结构体是 Flurry 中构建复杂数据模型的核心工具，结合其作为命名空间的能力和属性系统，提供了强大的定制化和组织能力。
