const std = @import("std");
const gc_module = @import("gc.zig");
const Ref = gc_module.Ref;
const MetaData = gc_module.MetaData;
const Gc = gc_module.Gc;
const w = @import("../common.zig");
const FileNode = @import("../vfs/node.zig").Node;
const hir = @import("hir.zig");
const ast = @import("../parse/ast.zig");
const StringPool = @import("../string_pool/string_pool.zig").StringPool;

const mod0 = @import("mod0.zig");
const file0 = @import("file0.zig");
const struct0 = @import("struct0.zig");
const enum0 = @import("enum0.zig");
const function0 = @import("function0.zig");

pub const Analyzer = struct {
    gpa: std.mem.Allocator,
    gc: *Gc,
    sp: *StringPool,

    root: Ref,
    stack: std.ArrayList(Context),

    pub fn init(
        gpa: std.mem.Allocator,
        gc: *Gc,
        sp: *StringPool,
        src_dir: *FileNode,
    ) !Analyzer {
        var result = Analyzer{
            .gpa = gpa,
            .gc = gc,
            .sp = sp,
            .root = null,
            .stack = std.ArrayList(Context).init(gpa),
        };

        const root = try result.gc.newHir(hir.Root);
        result.root = root;

        try result.enter(undefined, root);
        hir.Root.at(root).children = try result.gc.newHirChildren(0);

        const src_path = try src_dir.fullPath(gpa);
        defer gpa.free(src_path);
        std.debug.print("DEBUG: starting to allocate src dir symbols: {s}\n", .{src_path});
        try file0.packageVfsSymbolAllocation(&result, src_dir);

        return result;
    }

    pub fn deinit(self: *Analyzer) void {
        self.stack.deinit();
    }

    pub fn analyze(self: *Analyzer) !void {
        const ctx = self.current();
        const name = try self.sp.put(ctx.file.?.name);
        _ = try file0.analyzeFileScope(self, ctx.file.?, name);
    }

    pub inline fn current(self: *Analyzer) Context {
        return self.stack.items[self.stack.items.len - 1];
    }

    pub inline fn nodeItems(self: *Analyzer, idx: u64) u64 {
        return self.current().file.?.ast.?.nodes.items[idx];
    }

    pub inline fn srcContent(self: *Analyzer, idx: u64) []const u8 {
        // const token = self.getToken(self.getNode(idx + 1));
        const ctx = self.current();
        const token = ctx.file.?.ast.?.getToken(ctx.file.?.ast.?.getNode(idx + 1));
        return self.current().file.?.srcContentT(token, undefined) catch unreachable;
    }

    pub inline fn enter(self: *Analyzer, file: *FileNode, node: Ref) !void {
        try self.stack.append(.{
            .file = file,
            .node = node,
        });
    }

    pub inline fn exit(self: *Analyzer) void {
        if (self.stack.items.len == 1) {
            return;
        }
        _ = self.stack.pop();
    }

    pub fn symbolAllocation(self: *Analyzer, items: u64) anyerror!std.ArrayList(hir.Children.Symbol) {
        var symbols = std.ArrayList(hir.Children.Symbol).init(self.gpa);
        const len = self.nodeItems(items);

        for (0..len) |i| {
            const child = self.nodeItems(items + 1 + i);
            const tag: ast.Tag = @enumFromInt(self.nodeItems(child));

            switch (tag) {
                .mod => {
                    const mod_name = self.srcContent(self.nodeItems(child + 1));
                    try symbols.append(.{
                        .name = try self.sp.put(mod_name),
                        .ast_node = child,
                        .obj = null,
                    });
                },

                else => {},
            }
        }

        try symbols.ensureTotalCapacity(len);
        const children = try self.gc.newHirChildren(@intCast(symbols.items.len));
        @memcpy(hir.Children.Symbol.entries(children), symbols.items);

        return symbols;
    }
};

pub const Context = struct {
    file: ?*FileNode,
    node: Ref,
};
