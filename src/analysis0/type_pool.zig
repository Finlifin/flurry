const std = @import("std");
const gc = @import("gc.zig");
const Ref = gc.Ref;
const StringPool = @import("../string_pool/string_pool.zig").StringPool;

pub const TypePool = struct {
    types: std.ArrayList(Type),
    sp: *StringPool,

    pub const Index = u32;
};

pub const Type = enum {
    Any,

    app,
    named,
    unamed,
    tuple,
    builtin,
    typealias,
    newtype,

    effect_qualified,
    error_qualified,
    optional,
    pointer,

    function,

    void,
    NoReturn,
};

const StructField = struct {
    name: StringPool.Index,
    type: Type,
    default_value: ?[*]u8,
};
