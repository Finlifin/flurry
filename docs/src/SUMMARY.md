# Summary

<!-- 第一部分： 引论与愿景 -->
# 引论与愿景
- [Flurry: 一个编程语言](introduction/vision.md)
- [快速入门](introduction/quickstart.md)

<!-- 第二部分： 核心语言特性 -->
# 核心语言特性
- [基础语法与数据类型](core/basics.md)
  - [词法结构](core/basics/lexical.md)
  - [字面量详解](core/basics/literals.md)
  - [基本数据类型](core/basics/primitives.md)
  - [符号](core/basics/symbols.md)
- [变量、所有权与资源管理](core/ownership.md)
  - [声明与绑定](core/ownership/bindings.md)
  - [仿射类型与移动语义](core/ownership/affine_move.md)
  - [引用与借用](core/ownership/references.md)
  - [资源管理与 Drop](core/ownership/drop.md)
  - [引用有效性：可达性与副作用](core/ownership/reachability_effects.md)
- [表达式与运算符](core/expressions.md)
  - [常用运算符](core/expressions/operators.md)
  - [后缀风格与链式调用](core/expressions/suffix_chaining.md)
  - [取像操作 (expr ' image)](core/expressions/image_op.md)
- [基本控制流](core/control_flow.md)
  - [条件语句 (if, when)](core/control_flow/conditional.md)
  - [循环语句 (for, while)](core/control_flow/loops.md)
  - [控制转移 (break, continue, return)](core/control_flow/transfer.md)
- [函数](core/functions.md)
  - [定义与调用](core/functions/definition.md)
  - [参数详解](core/functions/parameters.md)
  - [返回值与错误处理 (!)](core/functions/return_errors.md)
- [数据结构](core/data_structures.md)
  - [结构体 (Structs)](core/data_structures/structs.md)
  - [枚举 (Enums)](core/data_structures/enums.md)
    - [层级化与融合](core/data_structures/enums/hierarchy_fusion.md)
    - [枚举属性](core/data_structures/enums/attributes.md)
    - [Tagged Polymorphism](core/data_structures/enums/tagged_polymorphism.md) # <-- Moved here
  - [联合体 (Unions)](core/data_structures/unions.md) # <-- Added
  - [数组与切片 (Arrays & Slices)](core/data_structures/arrays_slices.md)
  - [元组 (Tuples)](core/data_structures/tuples.md)
  - [模块 (Modules as Types)](core/data_structures/modules_as_types.md) # <-- Added
  - [Newtypes](core/data_structures/newtypes.md)
- [模式匹配](core/pattern_matching.md)
  - [模式语法](core/pattern_matching/syntax.md)
  - [匹配控制流 (match, if is, while is)](core/pattern_matching/control_flow.md)
- [Trait 与多态](core/traits_polymorphism.md)
  - [Trait 定义与实现 (impl, derive)](core/traits_polymorphism/traits.md)
  - [扩展 (extend) 与字面量拓展](core/traits_polymorphism/extend.md)
  - [动态多态 (dyn Trait vs tagged_polymorphic)](core/traits_polymorphism/dynamic.md)
  - [组合与委托 (using)](core/traits_polymorphism/using.md)

<!-- 第三部分： 高级特性与元编程 -->
# 高级特性与元编程
- [编译时计算 (comptime)](advanced/comptime.md)
  <!-- - [原理与上下文](advanced/comptime/principles.md)
  - [编译时控制流 (inline if/for)](advanced/comptime/inline_control_flow.md)
  - [编译时参数与依赖类型](advanced/comptime/dependent_types.md)
  - [编译时 `object`](advanced/comptime/object.md)
  - [类型作为值与反射](advanced/comptime/reflection.md)
  - [编译时错误处理](advanced/comptime/errors.md) -->
- [类型谓词 (type predicate)](advanced/type_predicates.md)
- [代数效应 (Algebraic Effects)](advanced/effects.md)
  <!-- - [概念与定义 (effect, yield)](advanced/effects/definition.md)
  - [处理与恢复 (handles, resume)](advanced/effects/handling.md)
  - [预制 Handler](advanced/effects/prefabricated.md)
  - [应用场景](advanced/effects/use_cases.md) -->
- [宏系统 (macros)](advanced/macros.md) <!-- Placeholder -->
  <!-- - [ORM DSL 示例](advanced/macros/orm_dsl_example.md) -->
- [错误处理机制](advanced/error_handling.md)
  <!-- - [语法与传播 (!)](advanced/error_handling/syntax.md)
  - [处理 (! { ... })](advanced/error_handling/handling.md)
  - [自动枚举融合](advanced/error_handling/fusion.md) -->
- [模块系统与包管理](advanced/modules_packages.md)
  - [模块定义与组织 (mod, mod file)](advanced/modules_packages/modules.md)
  - [导入 (use)](advanced/modules_packages/use.md)
  - [包与 `package.fl`](advanced/modules_packages/packages.md)
- [属性系统 (Attributes)](advanced/attributes.md)
  - [语法 (^expr, .symbol)](advanced/attributes/syntax.md)
  - [编译时配置与反射](advanced/attributes/comptime.md)
  - [库扩展应用](advanced/attributes/library_use.md)


<!-- 第四部分： 形式化验证与安全 -->
# 形式化验证与安全
- [Flurry 的安全哲学](safety/philosophy.md)
- [`safe` Flurry 的保证](safety/safe_guarantees.md)
  - [仿射类型与可达性系统回顾](safety/safe_guarantees/affine_reachability.md)
- [`unsafe` Flurry](safety/unsafe.md)
  - [必要性与风险](safety/unsafe/risks.md)
  - [验证 unsafe 代码](safety/unsafe/verification.md)
- [`verified` Flurry: 形式化验证](safety/verified.md)
  - [目标与方法 (ML, RL, OSL)](safety/verified/methods.md)
  - [规范与工作流程](safety/verified/workflow.md)

<!-- 第五部分： 深入主题与未来 -->
# 深入主题与未来
- [并发与并行](advanced_topics/concurrency.md)
- [与外部代码交互 (FFI)](advanced_topics/ffi.md)
- [语言设计原理与权衡](advanced_topics/design.md)
- [未来展望与社区参与](advanced_topics/roadmap_community.md)

<!-- 附录 -->
# 附录
- [语法速查表](appendix/syntax_cheat_sheet.md)
- [关键字参考](appendix/keywords.md)
- [内置属性参考](appendix/attributes_reference.md)
- [内置 "像" 参考](appendix/images_reference.md)
- [词汇表](appendix/glossary.md)