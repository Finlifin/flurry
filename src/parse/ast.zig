const std = @import("std");
const vfs = @import("../vfs/vfs.zig");
const w = @import("../common.zig");
const errors = @import("../errors/errors.zig");

pub const Tag = enum(u64) {
    id,
    int,
    real,
    str,
    bool,
    char,
    symbol,
    // int_extension -> int id
    int_extension,
    // real_extension -> real id
    real_extension,
    // str_extension -> str id
    str_extension,
    // char_extension -> char id
    char_extension,

    // expr_from_pattern -> < pattern >
    // list -> [ expr* ]
    // tuple -> ( expr, expr* )
    // parenthesis -> ( epxr )
    // unit -> ()
    // record -> { ( ...expr | property | expr )* }
    expr_from_pattern,
    list,
    tuple,
    parenthesis,
    unit,
    record,

    bool_not,

    // types
    effect_qualified,
    error_qualified,
    optional,
    trait_object,
    pointer_type,
    // forall_type -> forall<param*> expr
    forall_type,
    // for_type -> for<param*> expr
    for_type,

    range_to,
    range_to_inclusive,
    range_from,
    range_from_to,
    range_from_to_inclusive,

    add,
    sub,
    mul,
    div,
    mod,
    add_add,
    bool_eq,
    bool_not_eq,
    bool_and,
    bool_or,
    bool_gt,
    bool_gt_eq,
    bool_lt,
    bool_lt_eq,
    bool_implies,
    // bool_matches -> expr matches pattern
    bool_matches,

    // type_with -> expr : expr
    // subtype_with -> expr <: expr
    // trait_bound -> expr :- expr
    // field_method_bound -> expr :~ id : expr
    // declaration_bound -> expr :: id : expr
    type_with,
    subtype_with,
    trait_bound,
    field_method_bound,
    declaration_bound,

    // select -> expr . id
    // image -> expr ' id
    // pipe -> expr | expr
    // pipe_prepend -> expr |> call
    select,
    image,
    pipe,
    pipe_prepend,

    // deref -> expr . *
    // refer -> expr . ref
    // handler_apply -> expr . use(expr)
    // type_cast -> expr . as(expr)
    // as_dyn -> expr . dyn(expr)
    deref,
    refer,
    handler_apply,
    type_cast,
    as_dyn,

    // effect_elimination -> expr # { pattern_branch* }
    // error_elimination -> expr ! { (catch_branch | pattern_branch)* }
    // option_elimination -> expr ? block
    effect_elimination,
    error_elimination,
    option_elimination,
    catch_branch,

    // effect_emit -> expr #
    // error_throw -> expr !
    // option_unwrap -> expr ?
    effect_emit,
    error_throw,
    option_unwrap,

    // call -> expr ( arg* )
    // curry_call -> expr ' ( arg* )
    // diamond_call -> expr <arg*>
    // record_call -> expr { (... expr | property | expr)* }
    // index_call -> expr [ expr ]
    call,
    curry_call,
    diamond_call,
    record_call,
    index_call,

    macro_call,
    prefix_macro_call,

    closure,

    self,
    Self,
    null,
    itself,

    //-----------------//
    //    patterns    //
    //-----------------//
    // pattern_from_expr -> < expr >
    pattern_from_expr,

    // pattern_list -> [ pattern* ]
    pattern_list,

    // pattern_tuple -> ( pattern* )
    pattern_tuple,

    // property_pattern -> id : pattern
    property_pattern,
    // pattern_record -> { (id | id : pattern)*  }
    pattern_record,

    // patern_str_prefix -> str ++ id
    pattern_str_prefix,

    // pattern_symbold -> . id
    pattern_symbol,

    // pattern_not -> not pattern
    pattern_not,

    // pattern_type_bind -> ' id
    pattern_type_bind,

    // pattern_range_to -> .. pattern
    pattern_range_to,

    // pattern_range_to_inclusive -> ..= pattern
    pattern_range_to_inclusive,

    // pattern_list_rest_bind -> ... id
    pattern_list_rest_bind,

    // pattern_bitvec_0x -> 0x (int | id | ( id : pattern | pattern ))*
    pattern_bitvec_0x,

    // pattern_bitvec_0o -> 0o (int | ( id : pattern | pattern ))*
    pattern_bitvec_0o,

    // pattern_bitvec_0b -> 0b (int | ( id : pattern | pattern))*
    pattern_bitvec_0b,

    // pattern_option_some -> pattern ?
    pattern_option_some,

    // pattern_range_from -> pattern ..
    pattern_range_from,

    // pattern_range_from_to -> pattern .. pattern
    pattern_range_from_to,

    // pattern_range_from_to_inclusive -> pattern ..= pattern
    pattern_range_from_to_inclusive,

    // pattern_or -> pattern or pattern
    pattern_or,

    // pattern_and_is -> pattern and expr is pattern
    pattern_and_is,

    // pattern_select -> pattern . id
    pattern_select,

    // pattern_select_all -> pattern . *
    pattern_select_all,

    // pattern_call -> pattern ( (id : pattern | pattern)* )
    pattern_call,

    // pattern_diamond_call -> pattern < (id : pattern | pattern)* >
    pattern_diamond_call,

    // pattern_record_call -> pattern { (id | id: pattern)* }
    pattern_record_call,

    // pattern_as_bind -> pattern as id
    pattern_as_bind,

    // pattern_if_guard -> pattern if expr
    pattern_if_guard,

    // branch -> pattern => statement | block,
    branch,

    // if_is_pattern -> if expr is pattern block (else (if | block))?
    if_is_pattern,

    // if_is_match -> if expr is { branch* }
    if_is_match,

    // post_match -> expr match { branch }
    post_match,

    // while_is_pattern -> while label? expr is pattern invariants? block
    while_is_pattern,

    // while_is_match -> while label? expr is invariants? { branch* }
    while_is_match,

    //----------------//
    //   statements   //
    //----------------//
    // use_statement -> use mod_path
    // mod_path ->
    //     id
    //     | path_select
    //     | path_select_all
    //     | path_select_multi
    //     | super_path
    //     | package_path
    //     | path_as_bind
    // path_select -> mod_path . id
    // path_select_multi -> mod_path . { mod_path*}
    // path_select_all -> mod_path . *
    // super_path -> (.)+ mod_path
    // package_path -> @ mod_path
    // path_as_bind -> mod_path as id
    use_statement,
    path_select,
    path_select_multi,
    path_select_all,
    super_path,
    package_path,
    path_as_bind,

    mod_file,
    // if_guard -> if expr
    // label -> id
    // -- 控制流操作
    // break -> break label? if_guard?
    // continue -> continue label? if_guard?
    // return -> return label? if_guard?
    break_statement,
    continue_statement,
    return_statement,

    // let_decl -> let pattern (: expr)? (= expr)?
    // const_decl -> const pattern (: expr)? (= expr)?
    let_decl,
    const_decl,

    // expr_statement -> expr
    // assign -> expr = expr
    // assign_add -> expr += expr
    // assign_mul -> expr *= expr
    // assign_sub -> expr -= expr
    // assign_div -> expr /= expr
    // assign_mod -> expr %= expr
    expr_statement,
    assign,
    assign_add,
    assign_mul,
    assign_sub,
    assign_div,
    assign_mod,

    // -- 这几个用于形式化验证
    // asserts -> asserts expr
    // assumes -> assumes expr
    // invariant -> invariant expr
    // decreases -> decreases expr
    // -- 数据结构specification
    // axiom -> axiom expr
    asserts,
    assumes,
    invariant,
    decreases,
    axiom,
    invariants,

    // -- 基本分支语句
    // condition_branch -> expr => stmt | block
    // when -> when { condition_branch* }
    // else -> ( else block ) | ( else if)
    // if -> if expr block else?label -> : id
    condition_branch,
    when_statement,
    if_statement,

    // -- 基本循环语句
    // for_loop -> for label? pattern in expr invariants? block
    // while_loop -> while label? expr invariants? block
    // -- 如 whie:loop_example true { ... }, 其中loop_example是label名，并且这是一个死循环
    for_loop,
    while_loop,

    // 块
    block,
    do_block,
    unsafe_block,
    comptime_block,
    async_block,
    atomic_block,

    // typealias -> typealias<param*> id = expr
    typealias,
    // newtype -> newtype<param*> id = expr
    newtype,

    //---------------//
    //  definitions  //
    //---------------//
    // clauses -> where clause*
    clauses,
    // id : expr
    clause_decl,
    // .id : expr
    clause_decl_optional,
    // id
    clause_type_decl,
    // .id
    clause_type_decl_optional,
    // id :- expr
    clause_trait_bound_decl,
    // requires expr
    requires_predicate,
    // ensures expr
    ensures_predicate,
    // outcome (P expr)*
    outcome_predicate,

    // param_optional -> .id: expr = expr
    // param_typed -> id: expr
    // param_trait_bound -> id: expr :- expr
    // param_id -> id
    // param_optional_id -> .id
    // param_self -> self
    // param_self_ref -> *self
    // param_itself -> itself
    // param_itself_ref -> *itself
    // param_rest_bind -> ... id: expr
    param_optional,
    param_typed,
    param_trait_bound,
    param_id,
    param_optional_id,
    param_self,
    param_self_ref,
    param_itself,
    param_itself_ref,
    param_rest_bind,
    params,

    // return_type -> -> expr
    return_type,
    // gradual_return_type -> ~> expr
    gradual_return_type,

    // fn_def -> fn id? ( param* ) ( return_type | gradual_return_type )? clauses? block
    fn_def,
    // effect_def -> effect id? ( param* ) ( return_type | gradual_return_type )? clauses? block
    effect_def,
    // diamond_fn_def -> diamond id? ( param* ) ( return_type | gradual_return_type )? clauses? block
    diamond_fn_def,
    // handles_def -> handles id ( param* ) ( return_type | gradual_return_type )? clauses? block
    handles_def,

    // impl_def -> impl expr (for expr)? clauses? block
    // derive -> derive expr* for expr clauses?
    // extend -> extend expr (for expr)? clauses? block
    impl_def,
    derive,
    extend,

    // struct_field -> id : expr (= expr)?
    // struct_def -> struct id? clauses? block(struct_field*)
    struct_field,
    struct_def_body,
    struct_def,

    // enum_variant_with_struct -> id block(struct_field)
    // enum_variant_with_tuple -> id tuple
    // enum_variant_with_sub_enum -> id . block(enum_variant)
    // enum_variant_def_with_pattern -> id = pattern
    // enum_variant -> id | enum_variant_with_struct | enum_variant_with_tuple | enum_variant_with_sub_enum | enum_variant_def_with_pattern
    // enum_def -> enum id? clauses? block(enum_variant*)
    enum_variant_with_struct,
    enum_variant_with_tuple,
    enum_variant_with_sub_enum,
    enum_variant_def_with_pattern,
    enum_def_body,
    enum_def,

    // union_variant -> id : expr
    // union_def -> union id? clauses? block(union_variant*)
    union_variant,
    union_def_body,
    union_def,

    // pub term
    pub_def,
    // ^expr term
    attr_def,
    // property -> .id expr
    property,
    // inline term
    inline_def,
    // pure term
    pure_def,
    // comptime term
    comptime_def,

    // expand_items -> ... expr
    expand_items,

    file_scope,

    invalid,

    pub inline fn into(t: Tag) u64 {
        return @intFromEnum(t);
    }

    pub inline fn toString(t: Tag) []const u8 {
        return @tagName(t);
    }
};

pub const Node2Span = struct {
    from: u32,
    to: u32,
    node: u64,
};

pub const Ast = struct {
    allocator: std.mem.Allocator,

    file: *vfs.Node,
    nodes: std.ArrayList(u64),
    root: u64,
    spans: std.ArrayList(Node2Span),

    pub fn init(
        allocator: std.mem.Allocator,
        file: *vfs.Node,
    ) Ast {
        return Ast{
            .allocator = allocator,
            .file = file,
            .nodes = std.ArrayList(u64).init(allocator),
            .spans = std.ArrayList(Node2Span).init(allocator),
            .root = 0,
        };
    }

    // dump node to s-expression format
    pub fn dump(self: Ast, node_index: u64, writer: anytype) !void {
        const node = self.nodes.items[node_index];
        const tag: Tag = @enumFromInt(node);
        try writer.print("({s} ", .{@tagName(tag)});
        switch (tag) {
            // single tag
            .self,
            .param_self_ref,
            .itself,
            .param_itself_ref,
            .invalid,
            .null,
            .unit,
            => {},
            // with single token
            .int,
            .str,
            .id,
            .bool,
            .char,
            .real,
            => {
                const token = self.getToken(self.getNode(node_index + 1));
                try writer.writeAll(self.srcContent(token));
            },

            // with single child
            .pub_def,
            .inline_def,
            .pure_def,
            .comptime_def,

            .symbol,
            .use_statement,
            .expr_statement,
            .asserts,
            .assumes,
            .invariant,
            .decreases,
            .axiom,
            .mod_file,
            .do_block,
            .atomic_block,
            .comptime_block,
            .async_block,
            .unsafe_block,
            .pattern_from_expr,
            .expr_from_pattern,
            .path_select_all,
            .pattern_option_some,
            .effect_emit,
            .error_throw,
            .option_unwrap,
            .pattern_type_bind,
            .return_type,
            .gradual_return_type,
            .range_from,
            .range_to,
            .range_to_inclusive,
            .pattern_range_from,
            .pattern_range_to,
            .pattern_range_to_inclusive,
            .expand_items,
            .clause_type_decl,
            .clause_type_decl_optional,
            => {
                try self.dump(self.getNode(node_index + 1), writer);
            },
            // with two children
            .property,
            .add,
            .sub,
            .div,
            .mul,
            .add_add,
            .select,
            .image,
            .bool_eq,
            .bool_not_eq,
            .bool_and,
            .bool_or,
            .bool_gt,
            .bool_gt_eq,
            .bool_lt,
            .bool_lt_eq,
            .bool_matches,
            .type_with,
            .trait_bound,
            .index_call,
            .as_dyn,
            .type_cast,
            .int_extension,
            .real_extension,
            .str_extension,
            .char_extension,
            .refer,
            .handler_apply,
            .range_from_to,
            .range_from_to_inclusive,
            .pattern_range_from_to,
            .pattern_range_from_to_inclusive,
            .break_statement,
            .return_statement,
            .continue_statement,
            .assign,
            .assign_add,
            .assign_sub,
            .assign_div,
            .assign_mul,
            .assign_mod,
            .path_select,
            .pattern_select,
            .condition_branch,
            .branch,
            .post_match,
            .effect_elimination,
            .error_elimination,
            .option_elimination,
            .property_pattern,
            .pattern_as_bind,
            .pattern_if_guard,
            .enum_variant_def_with_pattern,
            .union_variant,
            .param_typed,
            .param_rest_bind,
            .param_trait_bound,
            .clause_decl,
            .clause_decl_optional,
            .clause_trait_bound_decl,
            => {
                try self.dump(self.getNode(node_index + 1), writer);
                try writer.writeAll(" ");
                try self.dump(self.getNode(node_index + 2), writer);
            },
            // // with multiple children
            .record,
            .list,
            .tuple,
            .pattern_record,
            .pattern_tuple,
            .pattern_list,
            .block,
            .file_scope,
            .clauses,
            .params,
            .struct_def_body,
            .enum_def_body,
            .union_def_body,
            .invariants,
            .when_statement,
            => {
                const len = self.getNode(node_index + 1);
                try writer.print("(len {d}) ", .{len});
                for (0..len) |i| {
                    try self.dump(self.getNode(node_index + 2 + i), writer);
                }
            },
            // // one left item and multiple children
            .call,
            .record_call,
            .curry_call,
            .if_is_match,
            .diamond_call,
            .pattern_call,
            .pattern_record_call,
            .path_select_multi,
            .enum_variant_with_struct,
            .enum_variant_with_tuple,
            .enum_variant_with_sub_enum,
            => {
                try self.dump(self.getNode(node_index + 1), writer);
                const len = self.getNode(node_index + 2);
                try writer.print("(len {d}) ", .{len});
                for (0..len) |i| {
                    try self.dump(self.getNode(node_index + 3 + i), writer);
                }
            },
            // // two left items and multiple children
            .derive,
            => {
                try self.dump(self.getNode(node_index + 1), writer);
                try self.dump(self.getNode(node_index + 2), writer);
                const len = self.getNode(node_index + 3);
                try writer.print("(len {d}) ", .{len});
                for (0..len) |i| {
                    try self.dump(self.getNode(node_index + 4 + i), writer);
                }
            },
            // three children!
            .let_decl,
            .const_decl,
            .if_statement,
            .pattern_and_is,
            .struct_field,
            .struct_def,
            .enum_def,
            .union_def,
            => {
                try self.dump(self.getNode(node_index + 1), writer);
                try writer.writeAll(" ");
                try self.dump(self.getNode(node_index + 2), writer);
                try writer.writeAll(" ");
                try self.dump(self.getNode(node_index + 3), writer);
            },
            // four children!
            .if_is_pattern,
            .while_loop,
            .impl_def,
            .extend,
            .closure,
            => {
                try self.dump(self.getNode(node_index + 1), writer);
                for (2..5) |i| {
                    try writer.writeAll(" ");
                    try self.dump(self.getNode(node_index + i), writer);
                }
            },
            // five children!
            .while_is_pattern,
            .for_loop,
            .fn_def,
            => {
                try self.dump(self.getNode(node_index + 1), writer);
                for (2..6) |i| {
                    try writer.writeAll(" ");
                    try self.dump(self.getNode(node_index + i), writer);
                }
            },
            // // |args| (-> expr)? clause* (block | expr)
            // .lambda => {
            //     try self.dump(self.getNode(node_index + 1), writer);
            //     try writer.writeAll(" ");
            //     try self.dump(self.getNode(node_index + 2), writer);
            //     try writer.writeAll(" ");
            //     try self.dump(self.getNode(node_index + 3), writer);
            //     try writer.writeAll(" ");

            //     const len = self.getNode(node_index + 4);
            //     try writer.print("(arg len {d}) ", .{len});
            //     for (0..len) |i| {
            //         try self.dump(self.getNode(node_index + 5 + i), writer);
            //     }
            // },
            // // fn name? (args) (->expr)? clauses? (block | ;)
            // .fn_def,
            // .effect_def,
            // .diamond_fn_def,
            // => {
            //     try self.dump(self.getNode(node_index + 1), writer);
            //     try writer.writeAll(" ");
            //     try self.dump(self.getNode(node_index + 2), writer);
            //     try writer.writeAll(" ");
            //     try self.dump(self.getNode(node_index + 3), writer);
            //     try writer.writeAll(" ");
            //     try self.dump(self.getNode(node_index + 4), writer);
            //     try writer.writeAll(" ");
            //     const len = self.getNode(node_index + 5);
            //     try writer.print("(arg len {d}) ", .{len});
            //     for (0..len) |i| {
            //         try self.dump(self.getNode(node_index + 6 + i), writer);
            //     }
            // },
            else => {
                try writer.writeAll("todo");
            },
        }
        try writer.writeAll(")");
    }

    pub inline fn getNode(self: Ast, node_index: u64) u64 {
        return self.nodes.items[node_index];
    }

    pub fn getNodeTag(self: Ast, node_index: u64) Tag {
        return @enumFromInt(self.getNode(node_index));
    }

    pub fn getToken(self: Ast, cursor: u64) w.Token {
        return self.file.tokens.?.items[cursor];
    }

    pub fn srcContent(self: Ast, token: w.Token) []const u8 {
        return self.file.content.?[token.from..token.to];
    }

    pub fn srcContentD(self: Ast, node: u64) []const u8 {
        const token = self.getToken(self.getNode(self.getNode(node) + 1));
        return self.src[token.from..token.to];
    }

    pub fn srcContentT(self: Ast, node: u64) []const u8 {
        return self.srcContent(self.getToken(self.getNode(node + 1)));
    }

    pub fn toSlice(self: Ast, node_index: u64) []u64 {
        const len = self.getNode(node_index);
        return self.nodes.items[node_index + 1 .. node_index + 1 + len];
    }

    pub fn report(
        self: *Ast,
        node: u64,
        kind: errors.Kind,
        code: errors.Err,
        label_info: []const u8,
        extra_lines: u64,
    ) !void {
        // Find the token span for the given node
        const span = self.getNode2Span(node) orelse return error.NodeNotFound;

        if (self.file.tokens) |tokens| {
            const from_token = tokens.items[span.from];
            const to_token = tokens.items[span.to];

            // If from and to are the same, use simpler report
            if (span.from == span.to) {
                try self.file.report(self.allocator, from_token, kind, code, label_info, extra_lines);
            } else {
                try self.file.reportSpan(self.allocator, from_token, to_token, kind, code, label_info, extra_lines);
            }
        }
    }

    // binary search
    pub fn getNode2Span(self: *Ast, node: u64) ?Node2Span {
        if (node == 0 or node >= self.nodes.items.len) return null;

        var left: u64 = 0;
        var right: u64 = self.spans.items.len;
        while (left < right) {
            const mid = left + (right - left) / 2;
            const span = self.spans.items[mid];
            if (span.node == node) return span;
            if (span.node < node) {
                left = mid + 1;
            } else {
                right = mid;
            }
        }
        return null;
    }

    pub fn deinit(self: *Ast) void {
        self.spans.deinit();
        self.nodes.deinit();
    }
};

const Line = struct {
    from: u64,
    to: u64,
};
