const std = @import("std");
const lex = @import("../lex/lexer.zig");
const ast = @import("../parse/ast.zig");
const parse = @import("../parse/parser.zig");
const Err = @import("../common.zig").Err;
const errors = @import("../errors/errors.zig");

// a global span
pub const Span = struct {
    src: u32,
    lspan: LSpan,
};

// a file local span
pub const LSpan = struct {
    from: u32,
    to: u32,
};

// 定义 VFS 节点类型（文件或目录）
pub const NodeKind = enum {
    file,
    directory,
};

// VFS 节点结构
pub const Node = struct {
    src_id: u32,
    kind: NodeKind,
    name: []const u8, // 文件或目录名
    parent: ?*Node, // 指向父节点的指针（可选，方便向上遍历）
    children: std.ArrayList(*Node), // 子节点列表（仅目录有）

    // 渐进式加载内容
    content: ?[]const u8 = null,
    tokens: ?std.ArrayList(lex.Token) = null,
    ast: ?ast.Ast = null,

    // For error reporting
    lines: ?std.ArrayList(Line) = null,

    // 创建新节点
    pub fn init(
        allocator: std.mem.Allocator,
        src_id: u32,
        kind: NodeKind,
        name: []const u8,
        parent: ?*Node,
    ) !*Node {
        const self = try allocator.create(Node);
        self.* = .{
            .src_id = src_id,
            .kind = kind,
            .name = try allocator.dupe(u8, name), //复制字符串
            .parent = parent,
            .children = std.ArrayList(*Node).init(allocator),
        };
        return self;
    }

    // 释放节点及其子节点
    pub fn deinit(self: *Node, allocator: std.mem.Allocator) void {
        allocator.free(self.name);
        if (self.kind == .directory) {
            for (self.children.items) |child| {
                child.deinit(allocator);
            }
            self.children.deinit();
        }
        if (self.content) |content|
            allocator.free(content);
        if (self.tokens) |tokens|
            tokens.deinit();
        if (self.ast) |*ast_|
            ast_.deinit();
        if (self.lines) |lines|
            lines.deinit();
        allocator.destroy(self);
    }

    // 添加子节点（仅用于目录）
    pub fn addChild(self: *Node, child: *Node) !void {
        if (self.kind != .directory) {
            return error.NotADirectory;
        }
        try self.children.append(child);
    }

    pub fn fullPath(self: *Node, allocator: std.mem.Allocator) ![]const u8 {
        var names = std.ArrayList([]const u8).init(allocator);
        defer names.deinit();
        var path = std.ArrayList(u8).init(allocator);

        var current_node: ?*Node = self;
        while (current_node) |node| {
            try names.append(node.name);
            current_node = node.parent;
        }

        var len = names.items.len - 1;
        while (len >= 0) {
            const name = names.items[len];
            try path.appendSlice(name);
            if (len != 0)
                try path.append('/');

            if (len == 0)
                break;

            len -= 1;
        }
        return path.toOwnedSlice();
    }

    // 读取文件内容
    pub fn loadModule(self: *Node, allocator: std.mem.Allocator) ![]const u8 {
        // dir nodes are not implemented
        std.debug.assert(self.kind == .file);

        if (self.content) |content| {
            return content;
        }

        const path = try self.fullPath(allocator);
        defer allocator.free(path);
        const file = try std.fs.openFileAbsolute(path, .{ .mode = .read_only });
        defer file.close();
        self.content = try file.readToEndAlloc(allocator, 1024 * 1024 * 1024 * 4);
        return self.content.?;
    }

    pub fn lexModule(self: *Node, allocator: std.mem.Allocator) !void {
        // dir nodes are not implemented
        std.debug.assert(self.kind == .file);

        if (self.tokens != null)
            return error.FileAlreadyLexed;

        const content = try self.loadModule(allocator);

        self.tokens = try lex.lex(allocator, content);
    }

    pub fn parseModule(self: *Node, allocator: std.mem.Allocator) !void {
        // dir nodes are not implemented
        std.debug.assert(self.kind == .file);

        if (self.ast != null)
            return error.FileAlreadyParsed;

        var parser = try parse.Parser.init(allocator, self, .{});
        try parser.parse();
    }

    pub fn srcContentT(self: *Node, token: lex.Token, allocator: std.mem.Allocator) ![]const u8 {
        const content = try self.loadModule(allocator);
        if (token.from > token.to or token.to > content.len) {
            return error.InvalidToken;
        }

        return content[token.from..token.to];
    }

    pub fn dumpTokens(self: *Node, allocator: std.mem.Allocator) !void {
        if (self.tokens) |tokens| {
            for (tokens.items) |token| {
                std.debug.print("| {s}:\t\t{s}\n", .{
                    @tagName(token.tag),
                    try self.srcContentT(token, allocator),
                });
            }
        }
    }

    pub fn dumpAst(self: *Node) !void {
        const dump_file = try std.fs.cwd().createFile("dump.lisp", .{ .read = true, .truncate = true });
        defer dump_file.close();

        if (self.ast) |ast_|
            try ast_.dump(ast_.root, dump_file.writer());
    }

    // Process lines for error reporting
    pub fn processLines(self: *Node, allocator: std.mem.Allocator) !void {
        if (self.lines != null) return;

        const content = try self.loadModule(allocator);
        var lines = std.ArrayList(Line).init(allocator);

        var line_start: u64 = 0;
        var i: u64 = 0;

        while (i < content.len) {
            const c = content[i];
            if (c == '\n') {
                try lines.append(Line{ .from = line_start, .to = i });
                line_start = i + 1;
            }
            i += 1;
        }
        try lines.append(Line{ .from = line_start, .to = content.len });

        self.lines = lines;
    }

    // binary search for the line number
    pub fn locate(self: Node, pos: u64) u64 {
        if (self.lines) |lines| {
            var lo: u64 = 0;
            var hi: u64 = lines.items.len;
            while (lo < hi) {
                const mid = lo + (hi - lo) / 2;
                const line = lines.items[mid];
                if (pos < line.from) {
                    hi = mid;
                } else if (pos > line.to) {
                    lo = mid + 1;
                } else {
                    return mid;
                }
            }
            return lo;
        }
        return 0;
    }

    // one token may cross multiple lines
    pub fn locateToken(self: Node, token: lex.Token, extra_lines: u64) LinesSlice {
        if (self.lines) |lines| {
            var from = self.locate(token.from);
            from = if (from < extra_lines) 0 else from - extra_lines;
            var to = self.locate(token.to);
            to = if (to + extra_lines >= lines.items.len) lines.items.len - 1 else to + extra_lines;
            return LinesSlice{ .lines = lines.items[from .. to + 1], .start_line = from };
        }
        return LinesSlice{ .lines = &[_]Line{}, .start_line = 0 };
    }

    pub fn locateTokens(self: Node, token_begin: lex.Token, token_end: lex.Token, extra_lines: u64) LinesSlice {
        if (self.lines) |lines| {
            var from = self.locate(token_begin.from);
            from = if (from < extra_lines) 0 else from - extra_lines;
            var to = self.locate(token_end.to);
            to = if (to + extra_lines >= lines.items.len) lines.items.len - 1 else to + extra_lines;
            return LinesSlice{ .lines = lines.items[from .. to + 1], .start_line = from };
        }
        return LinesSlice{ .lines = &[_]Line{}, .start_line = 0 };
    }

    pub fn report(
        self: *Node,
        allocator: std.mem.Allocator,
        token: lex.Token,
        kind: errors.Kind,
        code: errors.Err,
        label_info: []const u8,
        extra_lines: u64,
    ) !void {
        try self.processLines(allocator);

        const path = try self.fullPath(allocator);
        defer allocator.free(path);

        const lines = self.locateToken(token, extra_lines);
        const err_msg = @errorName(code);
        var buffer = std.ArrayList(u8).init(allocator);
        defer buffer.deinit();
        const writer = buffer.writer();

        const kind_str = switch (kind) {
            errors.Kind.err => "error",
            errors.Kind.warn => "warning",
            errors.Kind.info => "info",
            errors.Kind.help => "help",
            errors.Kind.log => "log",
        };
        const kind_color = switch (kind) {
            errors.Kind.err => "\x1b[31m",
            errors.Kind.warn => "\x1b[33m",
            errors.Kind.info => "\x1b[36m",
            errors.Kind.help => "\x1b[32m",
            errors.Kind.log => "\x1b[34m",
        };
        const reset_color = "\x1b[0m\x1b[39m";
        const bold = "\x1b[1m";

        try writer.print(
            "{s}{s}{s}[{d}]: {s}{s}\n",
            .{
                bold,
                kind_color,
                kind_str,
                @intFromError(code),
                err_msg,
                reset_color,
            },
        );
        try writer.print("{s}:\n", .{path});

        const content = self.content orelse return error.ContentNotLoaded;

        for (lines.lines, lines.start_line..) |line, line_idx| {
            const line_content = content[line.from..line.to];
            try writer.print("{d:>6} | {s}\n", .{ line_idx + 1, line_content });
            if (token.from >= line.from and token.to <= line.to) {
                const column = token.from - line.from;
                for (0..column + 9) |_| {
                    try writer.writeByte(' ');
                }
                try writer.writeAll(kind_color);
                for (token.from..token.to) |_| {
                    try writer.writeByte('^');
                }
                try writer.print(" {s}{s}\n", .{ label_info, reset_color });
            }
        }

        std.debug.print("{s}", .{buffer.items});
    }

    pub fn reportSpan(
        self: *Node,
        allocator: std.mem.Allocator,
        from_token: lex.Token,
        to_token: lex.Token,
        kind: errors.Kind,
        code: errors.Err,
        label_info: []const u8,
        extra_lines: u64,
    ) !void {
        try self.processLines(allocator);

        const path = try self.fullPath(allocator);
        defer allocator.free(path);

        const lines = self.locateTokens(from_token, to_token, extra_lines);
        const err_msg = @errorName(code);
        var buffer = std.ArrayList(u8).init(allocator);
        defer buffer.deinit();
        const writer = buffer.writer();

        const kind_str = switch (kind) {
            errors.Kind.err => "error",
            errors.Kind.warn => "warning",
            errors.Kind.info => "info",
            errors.Kind.help => "help",
            errors.Kind.log => "log",
        };
        const kind_color = switch (kind) {
            errors.Kind.err => "\x1b[31m",
            errors.Kind.warn => "\x1b[33m",
            errors.Kind.info => "\x1b[36m",
            errors.Kind.help => "\x1b[32m",
            errors.Kind.log => "\x1b[34m",
        };
        const reset_color = "\x1b[0m\x1b[39m";
        const bold = "\x1b[1m";

        try writer.print(
            "{s}{s}{s}[{d}]: {s}{s}\n",
            .{
                bold,
                kind_color,
                kind_str,
                @intFromError(code),
                err_msg,
                reset_color,
            },
        );
        try writer.print("{s}:\n", .{path});

        const content = try self.loadModule(allocator);

        for (lines.lines, lines.start_line..) |line, line_idx| {
            const line_content = content[line.from..line.to];
            try writer.print("{d:>6} | {s}\n", .{ line_idx + 1, line_content });

            // the first line
            if (from_token.from >= line.from and from_token.to <= line.to) {
                const column = from_token.from - line.from;
                for (0..column + 9) |_| {
                    try writer.writeByte(' ');
                }
                try writer.writeAll(kind_color);
                const min = if (to_token.to < line.to) to_token.to else line.to;
                for (from_token.from..min) |_| {
                    try writer.writeByte('^');
                }
                if (to_token.to <= line.to)
                    try writer.print(" {s}{s}\n", .{ label_info, reset_color })
                else
                    try writer.print("{s}\n", .{reset_color});
            } else if (from_token.from < line.from and to_token.to > line.to) {
                // the middle lines
                try writer.writeAll("         ");
                try writer.writeAll(kind_color);
                for (line.from..line.to) |_| {
                    try writer.writeByte('^');
                }
                try writer.print("{s}\n", .{reset_color});
            } else if (to_token.from >= line.from and to_token.to <= line.to) {
                // the last line
                const column = to_token.to - line.from;
                try writer.writeAll("         ");
                try writer.writeAll(kind_color);
                for (0..column) |_| {
                    try writer.writeByte('^');
                }
                try writer.print(" {s}{s}\n", .{ label_info, reset_color });
            }
        }

        std.debug.print("{s}", .{buffer.items});
    }
};

// Helper structures for error reporting
const Line = struct {
    from: u64,
    to: u64,
};

const LinesSlice = struct {
    lines: []const Line,
    start_line: u64,
};
