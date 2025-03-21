pub const Ref = ?[*]u8;

const std = @import("std");
const mem = std.mem;
const hir = @import("hir.zig");

pub const GcOption = struct {
    mode: enum { debug, release } = .debug,
    object_limit: usize = 1024 * 1024 * 32,
    verbose: bool = false,
};

pub const Err = error{
    OutOfMemory,
    OutOfObjectLimit,
    TypeError,
    UnknownErr,
};

pub const Gc = struct {
    arena: mem.Allocator,
    gpa: mem.Allocator,

    objects: std.ArrayList([*]u8),
    freed_objects: std.ArrayList(usize),
    // 所有object的数量

    option: GcOption,

    pub fn init(gpa: mem.Allocator, arena: mem.Allocator, opt: GcOption) Gc {
        return Gc{
            .arena = arena,
            .gpa = gpa,
            .objects = std.ArrayList([*]u8).init(gpa),
            .freed_objects = std.ArrayList(usize).init(gpa),
            .option = opt,
        };
    }

    pub fn newHir(self: *Gc, comptime T: type) Err![*]u8 {
        hirNodeTypeConstraints(T);
        const tag = T.tag;
        var field_len: u16 = 0;
        var ref_start: u8 = 0;
        inline for (@typeInfo(T).Struct.fields) |field| {
            if ((ref_start == 0 and field.type == Ref))
                ref_start = @as(u8, @intCast(field_len));
            field_len += 1;
        }
        // no ref field
        if (ref_start == 0)
            ref_start = @as(u8, @intCast(field_len));

        const meta = MetaData.init(
            .used,
            .hir,
            ref_start,
            field_len,
            tag,
            null,
        );
        const result = try self.allocRaw(field_len * 8);
        MetaData.of(result).* = meta;
        return result;
    }

    pub fn newHirChildren(self: *Gc, field_len: u16) Err![*]u8 {
        const meta = MetaData.init(
            .used,
            .hir,
            0,
            field_len,
            .children,
            null,
        );
        const result = try self.allocRaw(field_len * @sizeOf(hir.Children.Symbol));
        MetaData.of(result).* = meta;

        return result;
    }

    fn hirNodeTypeConstraints(comptime T: type) void {
        inline for (@typeInfo(T).Struct.fields) |field| {
            if (@sizeOf(field.type) > 8)
                @compileError("hir node (excluding `Children`) type field size must be less than 8 bytes");
        }
    }

    //      -----------
    //     | metadata |
    //  -> -----------
    //     |   data   |
    //     |   ...    |
    pub inline fn allocRaw(self: *Gc, byte_size: usize) Err![*]u8 {
        if (self.option.verbose)
            std.debug.print("DEBUG: allocRaw with byte size {any}\n", .{byte_size});
        if (self.objects.items.len >= self.option.object_limit)
            return Err.OutOfObjectLimit;

        const obj = self.gpa.alloc(u8, byte_size + MetaData.size) catch return Err.OutOfMemory;
        @memset(obj, 0);
        MetaData.of(obj.ptr + MetaData.size).* = .{};
        self.objects.append(obj.ptr + MetaData.size) catch return Err.OutOfMemory;
        return obj.ptr + MetaData.size;
    }

    pub fn free(self: *Gc, obj: [*]u8) void {
        self.gpa.free(MetaData.entireObject(obj));
    }

    pub fn mark(self: *Gc, obj: [*]u8) void {
        const meta = MetaData.of(obj);
        if (meta.color == .used) return;
        meta.color = .used;

        switch (meta.domain) {
            .hir => {
                if (meta.tag == .children) {
                    for (hir.Children.Symbol.entries(obj)) |child| {
                        if (child.obj) |child_obj| self.mark(child_obj);
                    }
                } else {
                    const ref_start = meta.size_description;
                    for (ref_start..meta.field_len) |f| {
                        if (Object.nthField(obj, @intCast(f))) |field| {
                            self.mark(field);
                        }
                    }
                }
            },
            .type_ctx => {
                unreachable;
            },
            .gc => {
                if (meta.size_description == 0) {
                    for (0..meta.field_len) |f| {
                        self.mark(obj + f * 8);
                    }
                }
            },
        }
    }

    pub fn gc(self: *Gc, root: []const [*]u8) !void {
        for (self.objects.items) |obj|
            MetaData.of(obj).color = .unused;

        for (root) |obj| {
            self.mark(obj);
        }

        for (self.objects.items, 0..) |obj, i| {
            const meta = MetaData.of(obj);
            if (meta.color == .unused) {
                self.free(obj);
                try self.freed_objects.append(i);
            }
        }

        var new_objects = std.ArrayList([*]u8).init(self.gpa);
        for (self.objects.items, 0..) |obj, i| {
            if (!self.bsContains(i))
                try new_objects.append(obj);
        }
        self.objects.deinit();
        self.objects = new_objects;
        self.freed_objects.clearRetainingCapacity();
        std.debug.print("DEBUG: self.objects.items.len after gc: {any}\n", .{self.objects.items.len});
    }

    // check freed_objects contains obj, binary search
    fn bsContains(self: Gc, obj: usize) bool {
        var l: usize = 0;
        var r: usize = self.freed_objects.items.len;
        while (l < r) {
            const m = l + (r - l) / 2;
            if (self.freed_objects.items[m] == obj)
                return true;
            if (self.freed_objects.items[m] < obj)
                l = m + 1
            else
                r = m;
        }
        return false;
    }

    pub fn deinit(self: *Gc) void {
        for (self.objects.items) |obj| {
            self.arena.free(MetaData.entireObject(obj));
        }
        self.objects.deinit();
        self.freed_objects.deinit();
    }
};

// 16 bytes
pub const MetaData = packed struct {
    color: Color = .used,
    domain: Domain = .hir,
    size_description: u8 = 0,
    field_len: u16 = 0,
    // 什么？你问我为什么不用union？因为是石山
    tag: hir.Tag = .invalid,
    type_ref: Ref = null,

    pub const size = @sizeOf(MetaData);

    comptime {
        std.debug.assert(size == 16);
    }

    pub inline fn of(obj: [*]u8) *MetaData {
        return @as(*MetaData, @alignCast(@ptrCast(obj - size)));
    }

    pub fn init(
        color: Color,
        domain: Domain,
        size_description: u8,
        field_len: u16,
        tag: hir.Tag,
        type_ref: Ref,
    ) MetaData {
        return MetaData{
            .color = color,
            .domain = domain,
            .size_description = size_description,
            .field_len = field_len,
            .tag = tag,
            .type_ref = type_ref,
        };
    }

    pub fn entireObject(obj: [*]u8) []u8 {
        const meta = of(obj);
        const base = obj - size;

        var obj_size: usize = size;

        switch (meta.domain) {
            .hir => {
                if (meta.tag == .children)
                    obj_size += meta.field_len * @sizeOf(hir.Children.Symbol)
                else
                    obj_size += meta.field_len * 8;
            },
            .type_ctx => {
                unreachable;
            },
            .gc => {
                if (meta.size_description == 0)
                    obj_size += meta.field_len * 8
                else
                    obj_size += meta.size_description;
            },
        }
        return base[0..obj_size];
    }
};

pub const Object = struct {
    pub fn toBytes(comptime ValueType: type, value: *const ValueType) [*]u8 {
        return @as([*]u8, @alignCast(@ptrCast(@constCast(value))));
    }

    // not { domain: .hir, tag: .children }
    pub fn nthField(obj: [*]u8, n: u16) Ref {
        const meta = MetaData.of(obj);
        std.debug.assert(n < meta.field_len);
        const base = @as([*]Ref, @alignCast(@ptrCast(obj)));
        return base[n];
    }

    // not { domain: .hir, tag: .children }
    pub fn setNthField(obj: [*]u8, n: u16, value: [*]u8) void {
        const meta = MetaData.of(obj);
        std.debug.assert(n < meta.field_len);
        const base = @as([*]Ref, @alignCast(@ptrCast(obj)));
        base[n] = value;
    }
};

pub const Color = enum(u4) {
    used,
    unused,
    undefined,
};

pub const Domain = enum(u4) {
    hir,
    type_ctx,
    gc,
};

pub inline fn asRef(comptime T: type, obj: [*]u8) *T {
    return @as(*T, @alignCast(@ptrCast(obj)));
}

pub fn main() !void {
    var gpa = std.heap.GeneralPurposeAllocator(.{}){};
    defer if (gpa.deinit() == .leak) {
        std.debug.print("Leaked....", .{});
    };

    var gc = Gc.init(gpa.allocator(), gpa.allocator(), .{});
    defer gc.deinit();

    // A { B, C }
    const mod_A = try gc.newHir(hir.ModDef);
    const mod_B = try gc.newHir(hir.ModDef);
    hir.ModDef.at(mod_B).parent = mod_A;
    const mod_C = try gc.newHir(hir.ModDef);
    hir.ModDef.at(mod_C).parent = mod_A;
    const children_of_A = try gc.newHirChildren(2);
    hir.Children.Symbol.entries(children_of_A)[0].obj = mod_B;
    hir.Children.Symbol.entries(children_of_A)[1].obj = mod_C;

    try gc.gc(&.{mod_A});
}
