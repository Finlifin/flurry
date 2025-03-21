const type_pool = @import("type_pool.zig");
const StringPool = @import("../string_pool/string_pool.zig").StringPool;
const TypePool = type_pool.TypePool;
const std = @import("std");
const ast = @import("../parse/ast.zig");

const gc = @import("gc.zig");
const Ref = gc.Ref;

pub const Tag = enum(u32) {
    root,
    package_def,
    mod_def,
    struct_def,
    enum_def,
    union_def,
    fn_def,
    trait_def,
    effect_def,
    const_def,

    children,
    impls,

    // exprs
    binary,
    int,
    float,
    str,
    char,

    unit,

    invalid,
};

pub fn resolve(obj_: Ref, name: StringPool.Index) ?*Children.Symbol {
    if (obj_ == null) return null;
    const obj = obj_.?;

    const meta = gc.MetaData.of(obj);
    var result = switch (meta.tag) {
        .package_def => PackageDef.resolve(obj, name),
        .mod_def => ModDef.resolve(obj, name),
        .root => return Root.resolve(obj, name),
        else => null,
    };

    if (result == null) {
        switch (meta.tag) {
            .package_def => result = resolve(PackageDef.at(obj).parent, name),
            .mod_def => result = resolve(ModDef.at(obj).parent, name),
            else => {},
        }
    }

    return result;
}

pub const PackageDef = struct {
    name: StringPool.Index,
    type_ref: Ref = null,
    impls: Ref = null,
    parent: Ref = null,
    children: Ref = null,

    pub const tag = Tag.package_def;

    pub fn at(obj: [*]u8) *PackageDef {
        return @as(*PackageDef, @alignCast(@ptrCast(obj)));
    }

    pub inline fn resolve(obj: [*]u8, name: StringPool.Index) Ref {
        const package = PackageDef.at(obj);
        return Children.resolve(package.children.?, name);
    }
};

pub const ModDef = struct {
    name: StringPool.Index,
    ast_node: u64,
    type_ref: Ref = null,
    impls: Ref = null,
    parent: Ref = null,
    children: Ref = null,

    pub const tag = Tag.mod_def;

    pub fn at(obj: [*]u8) *ModDef {
        return @as(*ModDef, @alignCast(@ptrCast(obj)));
    }

    pub inline fn resolve(obj: [*]u8, name: StringPool.Index) Ref {
        const mod = ModDef.at(obj);
        return Children.resolve(mod.children.?, name);
    }
};

pub const ConstDef = struct {
    name: StringPool.Index,
    ast_node: u64,
    type_ref: Ref = null,
    parent: Ref = null,
    type: Ref = null,
    value: Ref = null,

    pub fn at(obj: [*]u8) *ConstDef {
        return @as(*ConstDef, @alignCast(@ptrCast(obj)));
    }
};

pub const Root = struct {
    children: Ref = null,
    type_ref: Ref = null,

    pub const tag = Tag.root;

    pub fn at(obj: [*]u8) *Root {
        return @as(*Root, @alignCast(@ptrCast(obj)));
    }

    pub inline fn resolve(obj: [*]u8, name: StringPool.Index) Ref {
        const mod = Root.at(obj);
        return Children.resolve(mod.children.?, name);
    }
};

pub const Children = struct {
    pub fn resolve(children: [*]u8, name: StringPool.Index) ?*Symbol {
        for (Symbol.entries(children)) |*entry| {
            if (entry.name == name) return entry;
        }

        return null;
    }

    pub const tag = Tag.children;

    pub const Symbol = struct {
        name: StringPool.Index,
        ast_node: u64,
        obj: Ref,

        pub fn at(obj: [*]u8) *Symbol {
            return @as(*Symbol, @alignCast(@ptrCast(obj)));
        }

        pub fn entries(obj: [*]u8) []Symbol {
            const meta = gc.MetaData.of(obj);
            const base = @as([*]Symbol, @alignCast(@ptrCast(obj)));
            return base[0..meta.field_len];
        }

        comptime {
            std.debug.assert(@sizeOf(Symbol) == 24);
        }
    };

    pub fn fromSlice(
        gc_: *gc.Gc,
        slice: []Symbol,
    ) !Ref {
        const result = try gc_.newHirChildren(@intCast(slice.len));
        @memcpy(Symbol.entries(result), slice);
        return result;
    }
};

pub fn childrenPush(
    gc_: *gc.Gc,
    children: Ref,
    name: StringPool.Index,
    obj: Ref,
) !Ref {
    const meta = gc.MetaData.of(children.?);
    const base = @as([*]Children.Symbol, @alignCast(@ptrCast(children)));
    const new_children = try gc_.newHirChildren(meta.field_len + 1);
    @memcpy(Children.Symbol.entries(new_children), base);
    Children.Symbol.entries(new_children)[meta.field_len] = Children.Symbol{
        .name = name,
        .ast_node = 0,
        .obj = obj,
    };
    return new_children;
}

pub const Binary = struct {
    op: Op,
    ast_node: u64,
    type_ref: Ref,
    left: Ref,
    right: Ref,

    pub const Op = enum(u64) {
        add,
        sub,
        mul,
        div,
        mod,
        bool_eq,
        bool_ne,
        bool_lt,
        bool_le,
        bool_gt,
        bool_ge,
        bool_and,
        bool_or,
    };

    pub fn at(obj: [*]u8) *Binary {
        return @as(*Binary, @alignCast(@ptrCast(obj)));
    }
};

pub const Atom = struct {
    pub fn new(gc_: *gc.Gc, ast_node: u64, tag: Tag) ![*]u8 {
        const meta = gc.MetaData.init(0, 2, tag);
        const result = try gc_.new(meta);
        Int.at(result).type_index = 0;
        Int.at(result).ast_node = ast_node;
        return result;
    }

    pub const Int = struct {
        type_index: type_pool.Index,
        ast_node: u64,
        value: i64,

        pub fn at(obj: [*]u8) *Int {
            return @as(*Int, @alignCast(@ptrCast(obj)));
        }
    };

    pub const Float = struct {
        type_index: type_pool.Index,
        ast_node: u64,
        value: f64,

        pub fn at(obj: [*]u8) *Float {
            return @as(*Float, @alignCast(@ptrCast(obj)));
        }
    };

    pub const Str = struct {
        type_index: type_pool.Index,
        ast_node: u64,
        value: StringPool.Index,

        pub fn at(obj: [*]u8) *Str {
            return @as(*Str, @alignCast(@ptrCast(obj)));
        }
    };

    pub const Char = struct {
        type_index: type_pool.Index,
        ast_node: u64,
        value: u8,

        pub fn at(obj: [*]u8) *Char {
            return @as(*Char, @alignCast(@ptrCast(obj)));
        }
    };
};
