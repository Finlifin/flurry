gpa: Alc,
arena: Alc,

src: []const u8,
map: std.StringArrayHashMapUnmanaged(Index),
str_bytes: std.ArrayListUnmanaged(u8),
nodes: Soa(Node),
tokens: Soa(Token),
prisma: std.ArrayListUnmanaged(Index),

pub const Err = error{
    expect_expr,
    expect_construction,
};

pub const Node = struct {
    tag: Tag,
    lhs: Index,
    rhs: Index,
    // data: Index,

    pub const Tag = enum(Index) {
        // =========================
        // expressions
        // =========================

        tuple_init,

        match_expr,
        if_expr,
        when_expr,
        if_match_expr,
        arbitrary_construction,

        lambda,

        list,
        map,

        // literals
        // .say
        symbol,
        // 23
        int,
        // 23.23
        real,
        str,
        cstr,
        char_str,
        byte_array,
        bool_expr,
        unreachable_expr,
        unit_expr,
        null_expr,
        any_expr,

        bool_not,

        negative,

        // binary
        add,
        minus,
        mul,
        div,
        mod,
        // std.mem.Allocator
        select,
        // well'type
        lift,
        pipe,
        eff_elm,
        err_elm,
        bool_and,
        bool_or,
        bool_eq,
        bool_not_eq,
        bool_gt,
        bool_lt,
        bool_gt_eq,
        bool_lt_eq,

        add_add,

        is_subtype,
        is_supertype,

        range,
        range_inclusive,

        // unary (prefix)
        // &usize
        type_ref,
        type_mut_ref,
        type_noreturn,
        type_void,
        type_Any,
        // ^ann.always_inline fn ...
        annotation,

        // unary (postfix)

        // Node { .tag =  }
        construction,

        // types ?
        // error union,
        struct_def,
        trait_def,
        enum_def,
        union_def,
        opaque_def,
        impl_def,
        derive_def,
        handler_def,

        err_prefix,
        eff_prefix,
        ref_prefix,

        // others
        id,
        // HashMap<&str, Box<usize>>.new()
        call,
        // HashMap<&str, Box<Alright>>
        tcall,
        // { ...; false }
        block,
        cast,
        undefined,
        @"unreachable",

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
        fn_def,

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
