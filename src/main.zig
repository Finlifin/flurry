const std = @import("std");
const w = @import("common.zig");
const parse = @import("parse/parser.zig");
const Gc = @import("analysis0/gc.zig").Gc;
const asRef = @import("analysis0/gc.zig").asRef;
const hir = @import("analysis0/hir.zig");
const analysis0 = @import("analysis0/analyzer.zig");

pub fn main() !void {
    var gpa = std.heap.GeneralPurposeAllocator(.{}){};
    defer if (gpa.deinit() == .leak) std.debug.print("leaked..", .{});

    var sp = w.Sp.init(gpa.allocator());
    defer sp.deinit();

    var vfs = try w.Vfs.init(gpa.allocator(), .{ .mode = .release });
    defer vfs.deinit();

    try vfs.buildVfsFromDisk();

    var gc = Gc.init(gpa.allocator(), gpa.allocator(), .{});
    defer gc.deinit();

    const main_file = vfs.findNodeByPath("test_project/src/main.fl") orelse return error.FileNotFound;
    const main_file_path = try main_file.fullPath(gpa.allocator());
    defer gpa.allocator().free(main_file_path);
    std.debug.print("DEBUG: main.fl path: {s}\n", .{main_file_path});

    try main_file.lexModule(gpa.allocator());
    // try test_file.dumpTokens(gpa.allocator());
    try main_file.parseModule(gpa.allocator(), .{});
    try main_file.dumpAst();

    var anaylzer = try analysis0.Analyzer.init(
        gpa.allocator(),
        &gc,
        &sp,
        vfs.findNodeByPath("test_project/src") orelse return error.FileNotFound,
    );
    defer anaylzer.deinit();

    try anaylzer.analyze();
}

// pub fn main() !void {
//     var gpa = std.heap.GeneralPurposeAllocator(.{}){};
//     defer if (gpa.deinit() == .leak) {
//         std.debug.print("Leaked....", .{});
//     };

//     var gc = Gc.init(gpa.allocator(), gpa.allocator(), .{});
//     defer gc.deinit();

//     // A { B, C }
//     const mod_A = try gc.newHir(hir.ModDef);
//     const mod_B = try gc.newHir(hir.ModDef);
//     hir.ModDef.at(mod_B).parent = mod_A;
//     const mod_C = try gc.newHir(hir.ModDef);
//     hir.ModDef.at(mod_C).parent = mod_A;
//     const children_of_A = try gc.newHirChildren(2);
//     hir.Children.Symbol.entries(children_of_A)[0].obj = mod_B;
//     hir.Children.Symbol.entries(children_of_A)[1].obj = mod_C;
//     asRef(hir.ModDef, mod_A).children = children_of_A;

//     try gc.gc(&.{children_of_A});
//     std.debug.print("DEBUG: {any}\n", .{gc.objects.items});
// }
