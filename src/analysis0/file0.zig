const Analyzer = @import("analyzer.zig").Analyzer;
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
const struct0 = @import("struct0.zig");
const enum0 = @import("enum0.zig");
const function0 = @import("function0.zig");

pub fn analyzeFileScope(self: *Analyzer, file: *FileNode, name_sp: usize) !Ref {
    const ast_node = file.ast.?.root;
    const ctx = self.current();
    const node = try self.gc.newHir(hir.ModDef);
    const mod = hir.ModDef.at(node);
    mod.* = .{
        .name = name_sp,
        .ast_node = ast_node,
        .parent = ctx.node,
    };
    try self.enter(file, node);
    defer self.exit();

    var symbols = try self.symbolAllocation(ast_node + 1);
    defer symbols.deinit();
    const children = try hir.Children.fromSlice(self.gc, symbols.items);
    mod.children = children;

    return node;
}

// 通过文件结构分配mod
pub fn packageVfsSymbolAllocation(self: *Analyzer, src_dir: *FileNode) !void {
    std.debug.assert(src_dir.kind == .directory);

    const package_ = try self.gc.newHir(hir.PackageDef);
    const package = hir.PackageDef.at(package_);
    package.parent = self.root;
    const root = hir.Root.at(self.root.?);
    root.children = try hir.childrenPush(
        self.gc,
        root.children,
        try self.sp.put("main_package"),
        package_,
    );

    var symbols = std.ArrayList(hir.Children.Symbol).init(self.gpa);
    defer symbols.deinit();
    try symbols.ensureTotalCapacity(src_dir.children.items.len);

    const entry = src_dir.locateEntry(.main) orelse @panic("expected a `main.fl`(for root directory) or `mod.fl`(for sub directory)  file");
    var main_mod: Ref = null;

    const children = try self.gc.newHirChildren(@intCast(src_dir.children.items.len));
    for (src_dir.children.items) |child| {
        const child_name = try self.sp.put(child.name);
        const child_mod = switch (child.kind) {
            .directory => try directorySymbolAllocation(self, child, package_),
            .file => try fileSymbolAllocation(self, child, package_),
        };
        if (child == entry) main_mod = child_mod;

        try symbols.append(.{
            .name = child_name,
            .ast_node = if (child.ast) |ast_| ast_.root else 0,
            .obj = child_mod,
        });
    }

    @memcpy(hir.Children.Symbol.entries(children), symbols.items);
    package.children = children;

    try self.enter(undefined, package_);
    try self.enter(entry, main_mod);
}

fn directorySymbolAllocation(
    self: *Analyzer,
    dir: *FileNode,
    parent: Ref,
) !Ref {
    std.debug.assert(dir.kind == .directory);

    const entry = dir.locateEntry(.mod) orelse @panic("expected a `main.fl`(for root directory) or `mod.fl`(for sub directory)  file");

    const node = try self.gc.newHir(hir.ModDef);
    const mod = hir.ModDef.at(node);
    const mod_name = try self.sp.put(dir.name[0..]);
    mod.* = .{
        .name = mod_name,
        .ast_node = entry.ast.?.root,
    };

    var symbols = std.ArrayList(hir.Children.Symbol).init(self.gpa);
    defer symbols.deinit();
    try symbols.ensureTotalCapacity(dir.children.items.len - 1);
    const children = try self.gc.newHirChildren(@intCast(dir.children.items.len - 1));
    for (dir.children.items) |child| {
        if (child == entry) continue;
        const child_name = try self.sp.put(child.name);
        const child_mod = switch (child.kind) {
            .directory => try directorySymbolAllocation(self, child, parent),
            .file => try fileSymbolAllocation(self, child, parent),
        };

        try symbols.append(.{
            .name = child_name,
            .ast_node = child.ast.?.root,
            .obj = child_mod,
        });
    }

    @memcpy(hir.Children.Symbol.entries(children), symbols.items);
    mod.children = children;
    mod.parent = parent;

    return node;
}

fn fileSymbolAllocation(self: *Analyzer, file: *FileNode, parent: Ref) !Ref {
    std.debug.assert(file.kind == .file);
    const node = try self.gc.newHir(hir.ModDef);
    const mod = hir.ModDef.at(node);
    const mod_name = try self.sp.put(file.name[0 .. file.name.len - 3]);
    mod.* = .{
        .name = mod_name,
        .ast_node = if (file.ast) |ast_| ast_.root else 0,
        .parent = parent,
    };
    return node;
}
