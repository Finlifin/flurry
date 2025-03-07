const std = @import("std");
const w = @import("common.zig");
const parse = @import("parse/parser.zig");

pub fn main() !void {
    var gpa = std.heap.GeneralPurposeAllocator(.{}){};
    defer if (gpa.deinit() == .leak) std.debug.print("leaked..", .{});

    var sp = w.Sp.init(gpa.allocator());
    defer sp.deinit();

    var vfs = try w.Vfs.init(gpa.allocator(), .{ .mode = .release });
    defer vfs.deinit();

    try vfs.buildVfsFromDisk();

    const test_file = vfs.findNodeByPath("test.fl") orelse return error.FileNotFound;
    const test_file_path = try test_file.fullPath(gpa.allocator());
    defer gpa.allocator().free(test_file_path);
    std.debug.print("DEBUG: test.fl path: {s}\n", .{test_file_path});

    try test_file.lexModule(gpa.allocator());
    try test_file.dumpTokens(gpa.allocator());
    try test_file.parseModule(gpa.allocator());
    try test_file.dumpAst();
}
