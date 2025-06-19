# Trait 与多态 (Traits & Polymorphism)

Trait 是 Flurry 中定义共享行为的主要方式。它们类似于其他语言中的接口 (Interfaces) 或协议 (Protocols)，允许你定义一组方法签名，然后不同的类型可以实现这些方法。Trait 是 Flurry 实现抽象、代码复用以及多态性的核心机制。

多态性（Polymorphism）指的是代码可以处理多种不同类型的值的能力。Flurry trait系统主要通过以下方式支持多态：

-   **静态分派 (Static Dispatch)**: 基于泛型和 Trait 约束，在编译时确定调用哪个具体实现。
-   **动态分派 (Dynamic Dispatch)**: 在运行时根据对象的实际类型确定调用哪个实现，Flurry 提供了两种主要的动态多态机制。

本章将深入探讨：

-   Trait 的定义与实现 (`trait`, `impl`, `derive`)，以及孤儿原则。
-   独特的 `extend` 机制，用于作用域受限的行为扩展和字面量拓展。
-   Flurry 支持的动态多态方式：传统的 Trait Object (`dyn Trait`) 与基于枚举的 Tagged Polymorphism。
-   使用 `using` 实现组合与接口委托。

理解 Trait 和多态机制对于编写可扩展、可维护和抽象良好的 Flurry 代码至关重要。