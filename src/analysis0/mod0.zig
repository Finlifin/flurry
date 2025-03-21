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

const file0 = @import("file0.zig");
const struct0 = @import("struct0.zig");
const enum0 = @import("enum0.zig");
const function0 = @import("function0.zig");

pub fn anaylzeModScope(self: *Analyzer, file: *FileNode, ast_node: u64, parent: Ref) !Ref {
    const node = try self.gc.newHir(hir.ModDef);
    const mod = hir.ModDef.at(node);
    const name = self.srcContent(ast_node + 1);
    mod.* = .{
        .name = try self.sp.put(name),
        .ast_node = ast_node,
        .parent = parent,
    };

    try file.ast.?.report(
        ast_node,
        .log,
        error.NoError,
        "analyzing this mod",
        3,
    );

    try self.enter(file, node);
    defer self.exit();
    var symbols = try self.symbolAllocation(ast_node + 2);
    defer symbols.deinit();

    const children = try hir.Children.fromSlice(self.gc, symbols.items);
    mod.children = children;

    return node;
}
