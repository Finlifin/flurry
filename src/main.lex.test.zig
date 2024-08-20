const std = @import("std");
const Lexer = @import("lex/lex.zig").Lexer;

pub fn main() !void {
    const src_file = std.fs.cwd().openFile("test.fl", .{ .mode = .read_only }) catch unreachable;
    const src = src_file.readToEndAlloc(std.heap.page_allocator, 1024 * 1024) catch unreachable;
    var lexer = Lexer.init(src);
    defer lexer.deinit();

    std.debug.print("================src=================\n{s}\n", .{src});

    while (true) {
        const token = lexer.next();
        std.debug.print("'{s}' => {s}\n", .{
            @tagName(token.tag),
            src[token.from..token.to],
        });

        if (token.tag == .eof) break;
    }
}
