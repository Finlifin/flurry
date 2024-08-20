const std = @import("std");
const Parse = @import("parse/Parse.zig");
const Lex = @import("lex/lex.zig");
const Gpa = std.heap.GeneralPurposeAllocator(.{});

pub fn main() !void {
    var gpa = Gpa{};
    // defer if (gpa.deinit() == .leak) {};

    const src_file = std.fs.cwd().openFile("test.whale", .{ .mode = .read_only }) catch unreachable;
    defer src_file.close();
    const src = src_file.readToEndAlloc(gpa.allocator(), 1024 * 1024) catch unreachable;
    defer gpa.allocator().free(src);

    const tokens = Lex.lex(gpa.allocator(), src);

    var p = Parse.init(gpa.allocator(), gpa.allocator(), src, &tokens);
    const expr = p.pExpr();

    p.dump(expr, 0);
}
