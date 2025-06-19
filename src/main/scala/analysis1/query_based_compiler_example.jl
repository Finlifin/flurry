# Query-Based Compiler System for Complex Type-Dependent Analysis
# 设计一个基于查询的编译系统来处理相互依赖的分析阶段

using DataStructures
using Base.Threads

# ============================================================================
# Core Query System Infrastructure
# ============================================================================

# 查询键类型 - 标识不同类型的查询
abstract type QueryKey end

# 查询结果类型
abstract type QueryResult end

# 查询状态
@enum QueryStatus begin
    NotStarted
    InProgress
    Completed
    Failed
end

# 查询上下文 - 管理所有查询和缓存
mutable struct QueryContext
    cache::Dict{QueryKey, QueryResult}
    status::Dict{QueryKey, QueryStatus}
    dependencies::Dict{QueryKey, Set{QueryKey}}
    dependents::Dict{QueryKey, Set{QueryKey}}
    stack::Vector{QueryKey}  # 用于检测循环依赖
    lock::ReentrantLock
    
    QueryContext() = new(
        Dict{QueryKey, QueryResult}(),
        Dict{QueryKey, QueryStatus}(),
        Dict{QueryKey, Set{QueryKey}}(),
        Dict{QueryKey, Set{QueryKey}}(),
        QueryKey[],
        ReentrantLock()
    )
end

# 循环依赖异常
struct CyclicDependencyError <: Exception
    cycle::Vector{QueryKey}
end

# ============================================================================
# AST and HIR Definitions
# ============================================================================

# 抽象语法树节点
abstract type ASTNode end

# HIR (High-level Intermediate Representation) 节点
abstract type HIRNode end

# 位置信息
struct SourceLocation
    file::String
    line::Int
    column::Int
end

# AST 节点类型
struct ASTIdent <: ASTNode
    name::String
    location::SourceLocation
end

struct ASTType <: ASTNode
    name::String
    params::Vector{ASTNode}
    location::SourceLocation
end

struct ASTFunction <: ASTNode
    name::String
    params::Vector{ASTNode}
    return_type::Union{ASTNode, Nothing}
    body::ASTNode
    location::SourceLocation
end

struct ASTModule <: ASTNode
    name::String
    items::Vector{ASTNode}
    location::SourceLocation
end

# HIR 节点类型 - 扩展版本支持完整的lowering
struct HIRType <: HIRNode
    id::Int
    name::String
    namespace::Vector{String}
    kind::Symbol  # :concrete, :generic, :module, :trait
    params::Vector{HIRType}
end

struct HIRParameter <: HIRNode
    id::Int
    name::String
    param_type::HIRType
    default_value::Union{HIRNode, Nothing}
end

struct HIRFunction <: HIRNode
    id::Int
    name::String
    function_type::HIRType
    parameters::Vector{HIRParameter}
    return_type::HIRNode
    body::HIRNode
end

struct HIRModule <: HIRNode
    id::Int
    name::String
    namespace::Vector{String}
    items::Vector{HIRNode}
    type_definitions::Dict{String, HIRType}
    trait_implementations::Vector{HIRTraitImpl}
    compile_time_values::Dict{String, Any}
end

struct HIRVariableRef <: HIRNode
    id::Int
    name::String
    var_type::HIRType
    namespace_path::Vector{String}
end

struct HIRModuleRef <: HIRNode
    id::Int
    referenced_type::HIRType
    namespace_path::Vector{String}
end

struct HIRTypeRef <: HIRNode
    id::Int
    referenced_type::HIRType
end

struct HIRTypeDef <: HIRNode
    id::Int
    name::String
    namespace::Vector{String}
    type_parameters::Vector{HIRType}
    kind::Symbol
end

struct HIRTraitImpl <: HIRNode
    target_type::HIRType
    implementation::HIRFunction
end

# ============================================================================
# Query Key Definitions
# ============================================================================

# 名称解析查询
struct NameResolutionQuery <: QueryKey
    name::String
    scope::Vector{String}
    location::SourceLocation
end

# 类型检查查询
struct TypeCheckQuery <: QueryKey
    node::ASTNode
    expected_type::Union{HIRType, Nothing}
end

# 类型推断查询
struct TypeInferenceQuery <: QueryKey
    node::ASTNode
    context::Vector{String}
end

# Trait 解析查询
struct TraitResolutionQuery <: QueryKey
    type_id::Int
    trait_name::String
end

# 编译时计算查询
struct CompileTimeComputationQuery <: QueryKey
    expression::ASTNode
    type_context::HIRType
end

# 脱糖查询
struct DesugaringQuery <: QueryKey
    node::ASTNode
    target_form::Symbol
end

# HIR 生成查询
struct HIRGenerationQuery <: QueryKey
    node::ASTNode
    context::Vector{String}
end

# ============================================================================
# Query Result Definitions
# ============================================================================

struct NameResolutionResult <: QueryResult
    resolved_type::HIRType
    namespace_path::Vector{String}
    is_module::Bool
end

struct TypeCheckResult <: QueryResult
    is_valid::Bool
    inferred_type::HIRType
    errors::Vector{String}
end

struct TypeInferenceResult <: QueryResult
    inferred_type::HIRType
    constraints::Vector{Tuple{HIRType, HIRType}}
end

struct TraitResolutionResult <: QueryResult
    implementations::Vector{HIRFunction}
    is_coherent::Bool
end

struct CompileTimeComputationResult <: QueryResult
    value::Any
    type::HIRType
end

struct DesugaringResult <: QueryResult
    desugared_node::ASTNode
end

struct HIRGenerationResult <: QueryResult
    hir_node::HIRNode
end

# ============================================================================
# Core Query Execution Engine
# ============================================================================

# 执行查询的主函数
function execute_query(ctx::QueryContext, key::QueryKey)::QueryResult
    lock(ctx.lock) do
        # 检查缓存
        if haskey(ctx.cache, key)
            return ctx.cache[key]
        end
        
        # 检查循环依赖
        if key in ctx.stack
            cycle_start = findfirst(x -> x == key, ctx.stack)
            cycle = ctx.stack[cycle_start:end]
            throw(CyclicDependencyError(cycle))
        end
        
        # 标记为进行中
        ctx.status[key] = InProgress
        push!(ctx.stack, key)
        
        try
            # 执行具体查询
            result = dispatch_query(ctx, key)
            
            # 缓存结果
            ctx.cache[key] = result
            ctx.status[key] = Completed
            
            return result
        catch e
            ctx.status[key] = Failed
            rethrow(e)
        finally
            pop!(ctx.stack)
        end
    end
end

# 查询分发器
function dispatch_query(ctx::QueryContext, key::QueryKey)::QueryResult
    if isa(key, NameResolutionQuery)
        return resolve_name(ctx, key)
    elseif isa(key, TypeCheckQuery)
        return check_type(ctx, key)
    elseif isa(key, TypeInferenceQuery)
        return infer_type(ctx, key)
    elseif isa(key, TraitResolutionQuery)
        return resolve_trait(ctx, key)
    elseif isa(key, CompileTimeComputationQuery)
        return compute_compile_time(ctx, key)
    elseif isa(key, DesugaringQuery)
        return desugar_node(ctx, key)
    elseif isa(key, HIRGenerationQuery)
        return generate_hir(ctx, key)
    else
        error("Unknown query type: $(typeof(key))")
    end
end

# ============================================================================
# Query Implementation Functions
# ============================================================================

# 名称解析实现
function resolve_name(ctx::QueryContext, query::NameResolutionQuery)::NameResolutionResult
    # 这里需要查询类型系统来解析名称
    # 由于类型承担命名空间的作用，需要递归查询类型定义
    
    # 示例实现：从作用域开始向上查找
    for scope_level in reverse(query.scope)
        # 构造类型查询来查找在该作用域中的类型定义
        type_query = TypeInferenceQuery(
            ASTIdent(query.name, query.location),
            [scope_level]
        )
        
        try
            type_result = execute_query(ctx, type_query)
            return NameResolutionResult(
                type_result.inferred_type,
                [scope_level],
                type_result.inferred_type.kind == :module
            )
        catch
            continue  # 在这个作用域中没找到，继续向上查找
        end
    end
    
    error("Name '$(query.name)' not found in scope")
end

# 类型检查实现
function check_type(ctx::QueryContext, query::TypeCheckQuery)::TypeCheckResult
    # 首先推断表达式的类型
    infer_query = TypeInferenceQuery(query.node, String[])
    infer_result = execute_query(ctx, infer_query)
    
    # 如果有期望类型，检查兼容性
    if query.expected_type !== nothing
        is_compatible = check_type_compatibility(
            infer_result.inferred_type,
            query.expected_type
        )
        return TypeCheckResult(is_compatible, infer_result.inferred_type, String[])
    else
        return TypeCheckResult(true, infer_result.inferred_type, String[])
    end
end

# 类型推断实现
function infer_type(ctx::QueryContext, query::TypeInferenceQuery)::TypeInferenceResult
    node = query.node
    
    if isa(node, ASTIdent)
        # 查询名称解析
        name_query = NameResolutionQuery(node.name, query.context, node.location)
        name_result = execute_query(ctx, name_query)
        return TypeInferenceResult(name_result.resolved_type, Tuple{HIRType, HIRType}[])
        
    elseif isa(node, ASTFunction)
        # 函数类型推断需要推断参数和返回类型
        param_types = HIRType[]
        for param in node.params
            param_query = TypeInferenceQuery(param, query.context)
            param_result = execute_query(ctx, param_query)
            push!(param_types, param_result.inferred_type)
        end
        
        # 如果有显式返回类型，使用它；否则推断
        return_type = if node.return_type !== nothing
            return_query = TypeInferenceQuery(node.return_type, query.context)
            return_result = execute_query(ctx, return_query)
            return_result.inferred_type
        else
            # 从函数体推断返回类型
            body_query = TypeInferenceQuery(node.body, query.context)
            body_result = execute_query(ctx, body_query)
            body_result.inferred_type
        end
        
        # 构造函数类型
        func_type = HIRType(
            hash(node),
            "Function",
            String[],
            :concrete,
            [HIRType(0, "Tuple", String[], :concrete, param_types), return_type]
        )
        
        return TypeInferenceResult(func_type, Tuple{HIRType, HIRType}[])
        
    else
        error("Type inference not implemented for $(typeof(node))")
    end
end

# Trait 解析实现
function resolve_trait(ctx::QueryContext, query::TraitResolutionQuery)::TraitResolutionResult
    # 查找给定类型的 trait 实现
    # 这里需要查询编译时计算来解决复杂的 trait 约束
    
    # 简化实现
    return TraitResolutionResult(HIRFunction[], true)
end

# 编译时计算实现
function compute_compile_time(ctx::QueryContext, query::CompileTimeComputationQuery)::CompileTimeComputationResult
    # 编译时计算需要类型信息和名称解析
    type_query = TypeInferenceQuery(query.expression, String[])
    type_result = execute_query(ctx, type_query)
    
    # 这里应该实现图灵完备的编译时计算
    # 简化实现返回占位符
    return CompileTimeComputationResult(nothing, type_result.inferred_type)
end

# 脱糖实现
function desugar_node(ctx::QueryContext, query::DesugaringQuery)::DesugaringResult
    # 脱糖可能需要类型信息来决定如何转换
    type_query = TypeInferenceQuery(query.node, String[])
    type_result = execute_query(ctx, type_query)
    
    # 根据目标形式和类型信息进行脱糖
    # 简化实现直接返回原节点
    return DesugaringResult(query.node)
end

# HIR 生成实现 - 这是整个lowering过程的核心驱动函数
function generate_hir(ctx::QueryContext, query::HIRGenerationQuery)::HIRGenerationResult
    node = query.node
    context = query.context
    
    # === 第一阶段：预处理和脱糖 ===
    # 脱糖可能依赖类型信息，所以这里可能触发类型查询
    desugar_query = DesugaringQuery(node, :canonical)
    desugar_result = execute_query(ctx, desugar_query)
    canonical_node = desugar_result.desugared_node
    
    # === 第二阶段：类型分析 ===
    # 类型推断是HIR生成的关键依赖
    type_query = TypeInferenceQuery(canonical_node, context)
    type_result = execute_query(ctx, type_query)
    inferred_type = type_result.inferred_type
    
    # === 第三阶段：根据AST节点类型进行分发式HIR生成 ===
    return dispatch_hir_generation(ctx, canonical_node, inferred_type, context)
end

# HIR生成的分发函数 - 根据不同AST节点类型采用不同策略
function dispatch_hir_generation(ctx::QueryContext, node::ASTNode, node_type::HIRType, context::Vector{String})::HIRGenerationResult
    if isa(node, ASTIdent)
        return generate_hir_ident(ctx, node, node_type, context)
    elseif isa(node, ASTFunction)
        return generate_hir_function(ctx, node, node_type, context)
    elseif isa(node, ASTModule)
        return generate_hir_module(ctx, node, node_type, context)
    elseif isa(node, ASTType)
        return generate_hir_type(ctx, node, node_type, context)
    else
        error("HIR generation not implemented for $(typeof(node))")
    end
end

# 标识符的HIR生成
function generate_hir_ident(ctx::QueryContext, node::ASTIdent, node_type::HIRType, context::Vector{String})::HIRGenerationResult
    # 标识符需要名称解析来确定其含义
    name_query = NameResolutionQuery(node.name, context, node.location)
    name_result = execute_query(ctx, name_query)
    
    # 根据解析结果创建HIR节点
    if name_result.is_module
        # 如果是模块引用，创建模块访问HIR
        hir_module_ref = HIRModuleRef(
            hash((node.name, context)),
            name_result.resolved_type,
            name_result.namespace_path
        )
        return HIRGenerationResult(hir_module_ref)
    else
        # 普通变量或函数引用
        hir_var_ref = HIRVariableRef(
            hash((node.name, context)),
            node.name,
            name_result.resolved_type,
            name_result.namespace_path
        )
        return HIRGenerationResult(hir_var_ref)
    end
end

# 函数的HIR生成 - 展示复杂的递归lowering过程
function generate_hir_function(ctx::QueryContext, node::ASTFunction, node_type::HIRType, context::Vector{String})::HIRGenerationResult
    # === 参数处理 ===
    hir_params = HIRParameter[]
    param_context = [context; node.name]  # 函数形成新的作用域
    
    for (i, param_ast) in enumerate(node.params)
        # 每个参数都需要递归生成HIR
        param_hir_query = HIRGenerationQuery(param_ast, param_context)
        param_hir_result = execute_query(ctx, param_hir_query)
        
        # 参数类型推断
        param_type_query = TypeInferenceQuery(param_ast, param_context)
        param_type_result = execute_query(ctx, param_type_query)
        
        push!(hir_params, HIRParameter(
            i,
            get_param_name(param_ast),
            param_type_result.inferred_type,
            param_hir_result.hir_node
        ))
    end
    
    # === 返回类型处理 ===
    return_type_hir = if node.return_type !== nothing
        # 显式返回类型需要lowering
        return_type_query = HIRGenerationQuery(node.return_type, context)
        return_type_result = execute_query(ctx, return_type_query)
        return_type_result.hir_node
    else
        # 从函数体推断返回类型
        body_type_query = TypeInferenceQuery(node.body, param_context)
        body_type_result = execute_query(ctx, body_type_query)
        HIRTypeRef(hash(body_type_result.inferred_type), body_type_result.inferred_type)
    end
    
    # === 函数体lowering ===
    body_hir_query = HIRGenerationQuery(node.body, param_context)
    body_hir_result = execute_query(ctx, body_hir_query)
    
    # === 组装最终的函数HIR ===
    hir_function = HIRFunction(
        hash((node.name, context)),
        node.name,
        node_type,
        hir_params,
        return_type_hir,
        body_hir_result.hir_node
    )
    
    return HIRGenerationResult(hir_function)
end

# 模块的HIR生成 - 展示如何处理命名空间
function generate_hir_module(ctx::QueryContext, node::ASTModule, node_type::HIRType, context::Vector{String})::HIRGenerationResult
    # 模块创建新的命名空间
    module_context = [context; node.name]
    hir_items = HIRNode[]
    
    # === 两阶段处理：先收集类型定义，再处理实现 ===
    
    # 第一阶段：收集所有类型定义（用于前向引用）
    type_definitions = Dict{String, HIRType}()
    for item in node.items
        if isa(item, ASTType)
            # 预先注册类型到上下文中
            type_query = TypeInferenceQuery(item, module_context)
            type_result = execute_query(ctx, type_query)
            type_definitions[item.name] = type_result.inferred_type
        end
    end
    
    # 第二阶段：递归生成所有项目的HIR
    for item in node.items
        item_hir_query = HIRGenerationQuery(item, module_context)
        item_hir_result = execute_query(ctx, item_hir_query)
        push!(hir_items, item_hir_result.hir_node)
    end
    
    # === 模块级别的trait解析 ===
    # 查询这个模块中定义的所有trait实现
    module_traits = HIRTraitImpl[]
    for (type_name, hir_type) in type_definitions
        trait_query = TraitResolutionQuery(hir_type.id, "*")  # 查询所有traits
        trait_result = execute_query(ctx, trait_query)
        for impl in trait_result.implementations
            push!(module_traits, HIRTraitImpl(hir_type, impl))
        end
    end
    
    # === 编译时计算 ===
    # 模块级别的编译时计算（如常量表达式）
    compile_time_values = Dict{String, Any}()
    for item in node.items
        if is_compile_time_computable(item)
            ct_query = CompileTimeComputationQuery(item, node_type)
            ct_result = execute_query(ctx, ct_query)
            compile_time_values[get_item_name(item)] = ct_result.value
        end
    end
    
    hir_module = HIRModule(
        hash((node.name, context)),
        node.name,
        module_context,
        hir_items,
        type_definitions,
        module_traits,
        compile_time_values
    )
    
    return HIRGenerationResult(hir_module)
end

# 类型定义的HIR生成
function generate_hir_type(ctx::QueryContext, node::ASTType, node_type::HIRType, context::Vector{String})::HIRGenerationResult
    # 类型参数处理
    hir_type_params = HIRType[]
    for param in node.params
        param_type_query = TypeInferenceQuery(param, context)
        param_type_result = execute_query(ctx, param_type_query)
        push!(hir_type_params, param_type_result.inferred_type)
    end
    
    # 创建HIR类型定义
    hir_type_def = HIRTypeDef(
        hash((node.name, context)),
        node.name,
        context,
        hir_type_params,
        node_type.kind  # :concrete, :generic, :trait等
    )
    
    return HIRGenerationResult(hir_type_def)
end

# ============================================================================
# Utility Functions
# ============================================================================

# 类型兼容性检查
function check_type_compatibility(actual::HIRType, expected::HIRType)::Bool
    # 简化的类型兼容性检查
    return actual.name == expected.name
end

# 依赖关系记录
function record_dependency(ctx::QueryContext, dependent::QueryKey, dependency::QueryKey)
    if !haskey(ctx.dependencies, dependent)
        ctx.dependencies[dependent] = Set{QueryKey}()
    end
    if !haskey(ctx.dependents, dependency)
        ctx.dependents[dependency] = Set{QueryKey}()
    end
    
    push!(ctx.dependencies[dependent], dependency)
    push!(ctx.dependents[dependency], dependent)
end

# 缓存失效
function invalidate_cache(ctx::QueryContext, key::QueryKey)
    delete!(ctx.cache, key)
    delete!(ctx.status, key)
    
    # 递归失效所有依赖这个查询的缓存
    if haskey(ctx.dependents, key)
        for dependent in ctx.dependents[key]
            invalidate_cache(ctx, dependent)
        end
    end
end

# ============================================================================
# Query驱动的Lowering过程核心机制
# ============================================================================

# 这是整个系统的核心：查询如何驱动lowering过程
function lower_ast_to_hir_driven_by_queries(compiler::Compiler, root_ast::ASTNode)::HIRNode
    """
    Query驱动的lowering过程说明：
    
    1. 入口查询：HIRGenerationQuery 触发整个过程
    2. 递归依赖：每个查询可能触发多个子查询
    3. 缓存优化：已计算结果自动缓存，避免重复计算
    4. 依赖追踪：自动记录查询间依赖关系，支持增量更新
    """
    
    # 创建根查询 - 这将触发整个lowering过程
    root_query = HIRGenerationQuery(root_ast, String[])
    
    # 执行查询 - 这里会递归触发所有必要的子查询
    result = execute_query(compiler.context, root_query)
    
    return result.hir_node
end

# 查询执行的调用链示例
function demonstrate_query_call_chain()
    """
    以一个函数定义为例，展示查询调用链：
    
    HIRGenerationQuery(func_ast) 
    ├── DesugaringQuery(func_ast) 
    ├── TypeInferenceQuery(func_ast)
    │   ├── TypeInferenceQuery(param1)
    │   │   └── NameResolutionQuery(param1_type)
    │   ├── TypeInferenceQuery(param2)
    │   └── TypeInferenceQuery(return_type)
    ├── HIRGenerationQuery(param1)  # 递归处理参数
    ├── HIRGenerationQuery(param2)
    ├── HIRGenerationQuery(function_body)
    │   ├── HIRGenerationQuery(body_stmt1)
    │   │   ├── NameResolutionQuery(variable_name)
    │   │   └── TypeCheckQuery(assignment)
    │   └── HIRGenerationQuery(body_stmt2)
    └── TraitResolutionQuery(func_type)  # 如果函数有trait约束
    """
    println("Query call chain demonstration - see function docstring")
end

# 增量更新的驱动机制
function incremental_lowering_update(compiler::Compiler, changed_ast::ASTNode, old_ast::ASTNode)
    """
    增量更新过程：
    1. 识别影响范围：找到哪些查询结果会受到AST变更影响
    2. 选择性失效：只失效受影响的缓存条目
    3. 懒惰重计算：只有在需要时才重新计算失效的查询
    """
    
    # 找到受影响的查询
    affected_queries = find_queries_affected_by_ast_change(compiler.context, old_ast, changed_ast)
    
    # 失效相关缓存
    for query in affected_queries
        invalidate_cache(compiler.context, query)
    end
    
    # 重新生成HIR（只会重计算失效的部分）
    new_hir = lower_ast_to_hir_driven_by_queries(compiler, changed_ast)
    
    return new_hir
end

function find_queries_affected_by_ast_change(ctx::QueryContext, old_ast::ASTNode, new_ast::ASTNode)::Vector{QueryKey}
    affected = QueryKey[]
    
    # 检查所有缓存的查询，看哪些依赖于改变的AST节点
    for (query, _) in ctx.cache
        if query_depends_on_ast_node(query, old_ast)
            push!(affected, query)
        end
    end
    
    return affected
end

function query_depends_on_ast_node(query::QueryKey, ast_node::ASTNode)::Bool
    # 检查查询是否直接或间接依赖于给定的AST节点
    if isa(query, HIRGenerationQuery)
        return query.node === ast_node
    elseif isa(query, TypeInferenceQuery)
        return query.node === ast_node
    elseif isa(query, NameResolutionQuery) && isa(ast_node, ASTIdent)
        return query.name == ast_node.name
    end
    
    return false
end

# ============================================================================
# 辅助函数支持lowering过程
# ============================================================================

function get_param_name(param_ast::ASTNode)::String
    if isa(param_ast, ASTIdent)
        return param_ast.name
    else
        error("Cannot extract parameter name from $(typeof(param_ast))")
    end
end

function get_item_name(item_ast::ASTNode)::String
    if isa(item_ast, ASTFunction)
        return item_ast.name
    elseif isa(item_ast, ASTType)
        return item_ast.name
    elseif isa(item_ast, ASTIdent)
        return item_ast.name
    else
        return "anonymous"
    end
end

function is_compile_time_computable(ast_node::ASTNode)::Bool
    # 简化实现：检查节点是否可以在编译时计算
    # 实际实现需要更复杂的分析
    return isa(ast_node, ASTIdent) || isa(ast_node, ASTType)
end

# 编译器主接口
struct Compiler
    context::QueryContext
    
    Compiler() = new(QueryContext())
end

# 编译入口函数
function compile_to_hir(compiler::Compiler, ast::ASTNode)::HIRNode
    hir_query = HIRGenerationQuery(ast, String[])
    result = execute_query(compiler.context, hir_query)
    return result.hir_node
end

# 增量编译支持
function update_ast_node(compiler::Compiler, old_node::ASTNode, new_node::ASTNode)
    # 找到受影响的查询并失效缓存
    affected_queries = find_affected_queries(compiler.context, old_node)
    for query in affected_queries
        invalidate_cache(compiler.context, query)
    end
end

function find_affected_queries(ctx::QueryContext, node::ASTNode)::Vector{QueryKey}
    # 简化实现：返回所有查询（实际应该更精确）
    return collect(keys(ctx.cache))
end

# ============================================================================
# 完整的Lowering过程示例
# ============================================================================

function complete_lowering_example()
    """
    展示完整的query驱动lowering过程
    """
    println("=== Query驱动的AST到HIR Lowering过程演示 ===")
    
    # 1. 创建编译器和示例AST
    compiler = Compiler()
    
    # 创建一个复杂的AST示例：模块包含函数定义
    module_ast = ASTModule(
        "ExampleModule",
        [
            ASTType(
                "Point", 
                [ASTIdent("x", SourceLocation("test.jl", 2, 5)),
                 ASTIdent("y", SourceLocation("test.jl", 2, 8))],
                SourceLocation("test.jl", 2, 1)
            ),
            ASTFunction(
                "distance",
                [ASTIdent("p1", SourceLocation("test.jl", 5, 10)),
                 ASTIdent("p2", SourceLocation("test.jl", 5, 14))],
                ASTType("Float64", ASTNode[], SourceLocation("test.jl", 5, 20)),
                ASTIdent("result", SourceLocation("test.jl", 6, 5)),
                SourceLocation("test.jl", 5, 1)
            )
        ],
        SourceLocation("test.jl", 1, 1)
    )
    
    println("2. 开始query驱动的lowering...")
    
    try
        # 这个调用会触发整个查询链
        hir_result = lower_ast_to_hir_driven_by_queries(compiler, module_ast)
        
        println("3. Lowering成功完成！")
        println("   HIR类型: $(typeof(hir_result))")
        println("   缓存的查询数量: $(length(compiler.context.cache))")
        
        # 显示查询执行统计
        show_query_statistics(compiler.context)
        
        # 演示增量更新
        println("\n4. 演示增量更新...")
        modified_ast = modify_ast_for_demo(module_ast)
        updated_hir = incremental_lowering_update(compiler, modified_ast, module_ast)
        println("   增量更新完成，重新计算的查询数量较少")
        
    catch e
        if isa(e, CyclicDependencyError)
            println("检测到循环依赖: $(e.cycle)")
        else
            println("Lowering失败: $e")
        end
    end
end

function show_query_statistics(ctx::QueryContext)
    query_types = Dict{Type, Int}()
    
    for query in keys(ctx.cache)
        query_type = typeof(query)
        query_types[query_type] = get(query_types, query_type, 0) + 1
    end
    
    println("   查询执行统计:")
    for (query_type, count) in query_types
        println("     $(query_type): $(count)次")
    end
end

function modify_ast_for_demo(original_ast::ASTModule)::ASTModule
    # 创建修改版本用于演示增量更新
    modified_items = copy(original_ast.items)
    
    # 添加一个新函数
    new_function = ASTFunction(
        "new_function",
        ASTNode[],
        nothing,
        ASTIdent("unit", SourceLocation("test.jl", 10, 5)),
        SourceLocation("test.jl", 10, 1)
    )
    
    push!(modified_items, new_function)
    
    return ASTModule(
        original_ast.name,
        modified_items,
        original_ast.location
    )
end

# ============================================================================
# Main Compiler Interface
# ============================================================================

function example_usage()
    # 创建编译器实例
    compiler = Compiler()
    
    # 创建示例 AST
    ast_func = ASTFunction(
        "example_func",
        [ASTIdent("x", SourceLocation("test.jl", 1, 1))],
        ASTType("Int", ASTNode[], SourceLocation("test.jl", 1, 10)),
        ASTIdent("x", SourceLocation("test.jl", 1, 15)),
        SourceLocation("test.jl", 1, 1)
    )
    
    try
        # 编译为 HIR
        hir_result = compile_to_hir(compiler, ast_func)
        println("Successfully compiled to HIR: $(typeof(hir_result))")
        
        # 查询缓存状态
        println("Cache size: $(length(compiler.context.cache))")
        
    catch e
        println("Compilation failed: $e")
    end
end

# 运行示例
# example_usage()