gpa: Alc,
arena: Alc,

src: []const u8,
map: std.StringArrayHashMapUnmanaged(Index),
str_bytes: std.ArrayListUnmanaged(u8),
nodes: Soa(Node),
tokens: Soa(Token),
prisma: std.ArrayListUnmanaged(Index),

pub const Node = struct {
    tag: Tag,
    lhs: Index,
    rhs: Index,
    // token: Index,

    pub const Tag = enum(Index) {
        // =========================
        // expressions
        // =========================

        // basic expressions
        // .say
        // lhs: token index
        symbol,
        // `say you say me`
        // lhs: token index
        id,
        // 23
        // lhs: token index
        int,
        // 23.23
        // lhs: token index, rhs: token index to variant id
        real,
        // "hello" " world"
        // lhs: token index, rhs: 1 inserted, 0 not
        str,
        // "hello" " world" cstr
        // lhs: token index, rhs: 1 inserted, 0 not
        cstr,
        // 'a'
        // lhs: token index
        char,
        // "hello" " world" byte
        // lhs: token index, rhs: 1 inserted, 0 not
        byte_array,
        // true
        // lhs: token index, rhs: 0 false, 1 true
        bool_expr,
        // unreachable
        // lhs: token index
        unreachable_expr,
        // undefined
        // lhs: token index
        undefined_expr,
        // unit
        // lhs: token index
        unit_expr,
        // null
        // lhs: token index
        null_expr,
        // any
        // lhs: token index
        any_expr,
        // ..
        // lhs: token index
        range_full,

        // Any
        // lhs: token index
        Any_type,
        // noreturn
        // lhs: token index
        noreturn_type,
        // void
        // lhs: token index
        void_type,

        // prefix expressions
        // not expr
        // lhs: token index, rhs: expr
        bool_not,
        // -expr
        // lhs: token index, rhs: expr
        negative,
        // ..expr
        // lhs: token index, rhs: expr
        range_to,
        // ..=expr
        // lhs: token index, rhs: expr
        range_to_inclusive,
        // ^expr_l expr_r
        // lhs: expr_l, rhs: expr_r
        attribute,
        // #expr_l expr_r
        // lhs: expr_l, rhs: expr_r
        effect_type,
        // !expr_l expr_r
        // lhs: expr_l, rhs: expr_r
        error_type,
        // ?expr
        // lhs: token index, rhs: expr
        pointer_type,
        // *expr
        // lhs: token index, rhs: expr
        optional_type,
        // dyn expr
        // lhs: token index, rhs: expr
        dyn_type,
        // refines expr where clause_list
        // lhs: expr, rhs: []clause_list
        refined_type,
        // invariant expr
        // lhs: token index, rhs: expr
        invariant,
        // @id(expr)
        // lhs: id token index, rhs: expr
        builtin_call,
        // \param_list -> block | expr
        // lhs: param_list, rhs: [] if len = 1 then expr else []statements
        lambda,

        /////--------notice that function type and closure type are not yet finished
        // (pure comptime)' fn(param_type_list) -> return_type
        // lhs: fn_kind, rhs: [2]{ return_type, param_type_list }
        fn_type,
        // alright(param_type_list) -> return_type
        Fn_trait,
        FnOnce_trait,
        FnMut_trait,

        // [expr_list]
        // lhs: []expr_list
        list,
        // [(id: expr)+]
        // lhs: []pairs
        map,
        // (expr_list)
        // lhs: []expr_list
        tuple,
        // (expr)
        // lhs: expr
        paren,
        // <pattern>
        // lhs: pattern
        pattern_expr,
        // struct where clause_list? { statements }
        // lhs: []clause_list, rhs: []statements
        struct_def,
        // enum where clause_list? { statements }
        // lhs: []clause_list, rhs: []statements
        enum_def,
        // union where clause_list? { statements }
        // lhs: []clause_list, rhs: []statements
        union_def,
        // opaque where clause_list? { statements }
        // lhs: []clause_list, rhs: []statements
        opaque_def,
        // impl expr_trait for expr_type where clause_list? { statements }
        // lhs: expr_type, rhs: [2]{ expr_trait, []statements, []clause_list, pub }
        impl_def,
        // derive expr_trait for expr_type where clause_list?
        // lhs: expr_type, rhs: [2]{ expr_trait, []clause_list, pub }
        derive_def,
        // handle expr expr? { handler body }
        handler_def,
        fn_def,
        effect_def,
        // do block
        // lhs: []statements
        do_block,
        // comptime block
        // lhs: []statements
        comptime_block,
        // when { branches }
        when_expr,
        if_expr,
        if_is_expr,
        if_is_pattern_expr,
        // return expr
        // lhs: expr
        return_expr,
        // .symbol { init }
        symbol_init,
        // .symbol(arg_list)
        symbol_call,
        // . { init }
        arbitary_init,

        // postfix expressions
        // expr + expr
        // lhs + rhs
        add,
        // expr - expr
        // lhs - rhs
        minus,
        // expr * expr
        // lhs * rhs
        mul,
        // expr / expr
        // lhs / rhs
        div,
        // expr ++ expr
        // lhs ++ rhs
        add_add,
        // expr % expr
        // lhs % rhs
        mod,

        // expr == expr
        // lhs == rhs
        bool_eq,
        // expr != expr
        // lhs != rhs
        bool_not_eq,
        // expr < expr
        // lhs < rhs
        bool_lt,
        // expr <= expr
        // lhs <= rhs
        bool_lt_eq,
        // expr > expr
        // lhs > rhs
        bool_gt,
        // expr >= expr
        // lhs >= rhs
        bool_gt_eq,
        // expr and expr
        // lhs and rhs
        bool_and,
        // expr or expr
        // lhs or rhs
        bool_or,
        // expr ==> expr
        // lhs ==> rhs
        bool_implies,
        // expr: expr
        // lhs: rhs
        bool_typed_with,
        // expr:- expr
        // lhs:- rhs
        bool_implements,
        // expr_m:: id: expr_s
        // lhs: expr_m , rhs: [2]{ id, expr_s }
        bool_restricted_with,

        // expr | expr
        // lhs | rhs
        pipe,
        // expr ' id
        // lhs ' rhs
        lift,
        // expr'as(expr)
        // lhs'as(rhs)
        cast,
        // expr . id
        // lhs . rhs
        select,
        // expr.*
        // lhs: expr
        dereference,

        // expr .. expr
        // lhs .. rhs
        range_from_to,
        // expr ..
        // lhs: expr
        range_from,
        // expr ..= expr
        // lhs ..= rhs
        range_from_to_inclusive,

        // expr ( arg_list )
        // lhs: expr, rhs: []arg_list
        call,
        // expr < arg_list >
        // lhs: expr, rhs: []arg_list
        tcall,
        // expr [expr]
        // lhs [rhs]
        index,

        // expr # { effect_elimination_branches }?
        // lhs: expr, rhs: []effect_elimination_branches
        effect_elimination,
        // expr ## expr
        // lhs ## rhs
        effect_handler_elimination,
        // expr ! { error_elimination_branches }?
        // lhs: expr, rhs: []error_elimination_branches
        error_elimination,
        // expr !! expr
        // lhs !! rhs
        error_handler_elimination,
        // expr ? { expr }?
        // lhs ? rhs
        option_elimination,

        // expr match
        match_expr,
        // expr handle error pattern { handler body }
        error_handle_expr,

        // =========================
        // statements
        // =========================

        // =========================
        // patterns
        // =========================

        // =========================
        // definitions
        // =========================
        // lhs: [is_runtime, name, dependent args, args, return_type, clauses], rhs: body

        // =========================
        // others
        // =========================
        invalid,
        placeholder,
    };
};

const Index = @import("../base/types.zig").Index;
const std = @import("std");
const Soa = std.MultiArrayList;
const Alc = std.mem.Allocator;
const Token = @import("../lex/lex.zig").Token;
