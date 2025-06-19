# Newtypes

Newtype 是一种创建新类型的方式，该新类型在底层表示上基于一个已存在的类型，但在类型系统层面被视为一个**完全不同**的类型。它主要用于增强类型安全，通过创建独特的类型来区分原本表示相同但逻辑意义不同的值。

**定义 Newtype**

使用 `newtype` 关键字来定义。

```flurry
-- 定义一个 Newtype UserId，底层是 Uuid
newtype UserId = Uuid;
-- 自动derive From<Uuid> for Uuid，derive Into<Uuid> for UserId

-- 定义一个 Newtype EmailAddress，底层是 String
newtype EmailAddress = String;

-- 也可以基于复杂类型
struct ProductId { value: u64 }
newtype ProductRef = *ProductId;
```

**类型安全优势**

Newtype 的核心价值在于类型安全。即使底层表示相同，不同 Newtype 之间也不能直接混用。

```flurry
fn process_user(id: UserId) { {- ... -} }
fn process_product(id: ProductId) { {- ... -} } -- 假设 ProductId 是一个 struct

test {
    let user_uuid = Uuid.new_v4();
    let product_id_val = 12345u64;

    let user_id = user_uuid.into();
    let product_id = ProductId { .value product_id_val }

    process_user(user_id); -- OK

    -- process_user(user_uuid); -- 错误! 类型不匹配 (Uuid vs UserId)
    -- process_user(product_id); -- 错误! 类型不匹配 (ProductId vs UserId)
    -- let temp_id = product_id_val.into();
}
```

**底层表示与开销**

Newtype 通常是**零成本抽象 (zero-cost abstraction)**。在编译后，Newtype 与其底层类型在内存表示和运行时行为上通常是**完全相同**的。类型检查只发生在编译时。这意味着使用 Newtype 不会带来额外的运行时性能开销。

**为 Newtype 实现 Trait 或方法**

可以像为普通 `struct` 或 `enum` 一样，为 Newtype 定义implementation和extension。
定义newtype时，不仅编译器会自动为其实现 `From` 和 `Into` trait，还可以通过type casting 来实现类型转换。

```flurry
newtype Age = u32;

impl Age {
    -- 为 Newtype 添加方法
    pub fn is_adult(*self) -> bool {
        self.as(u32) >= 18
    }
}

extend fmt.Display for Age {
    fn fmt(*self, f: *fmt.Formatter) -> fmt.Result {
        -- 需要访问底层值
        write!(f, "{} years old", self.as(u32))
    }
}

test {
    let age = 30.as(Age);
    if age.is_adult() {
        println("Is adult. {}", age); -- 输出: Is adult. 30 years old
    }
}
```

**Typealias vs Newtype**
Newtype 与类型别名（`typealias`）的区别在于，类型别名只是给现有类型一个新的名字，而 Newtype 创建了一个新的、独立的类型。类型别名不会提供额外的类型安全，而 Newtype 则会。


**总结**

Newtype 提供了一种轻量级（通常零成本）的方式来创建新的、类型安全的别名。它们是增强代码清晰度和防止逻辑错误的有力工具，通过在编译时区分不同逻辑含义但底层表示可能相同的值。当你想给一个现有类型赋予更强的语义约束时，Newtype 是一个绝佳的选择。
