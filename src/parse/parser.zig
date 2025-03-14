const std = @import("std");
const w = @import("../common.zig");
const ast = @import("ast.zig");
const vfs = @import("../vfs/vfs.zig");
const exprs = @import("expr.zig");
const patterns = @import("pattern.zig");
const statements = @import("statement.zig");

pub const Parser = struct {
    alc: std.mem.Allocator,
    tmp_buf: []u8,
    tmp_alc: std.heap.FixedBufferAllocator,

    // for debug
    tags: std.ArrayList(ast.Tag),
    tags_location: std.ArrayList(u64),
    file: *vfs.Node,
    ast: ast.Ast,

    cursors: std.ArrayList(u32),
    cursor: u32,

    option: ParseOption,
    err: ErrPayload,
    err_msg: []const u8,

    pub fn init(
        allocator: std.mem.Allocator,
        file: *vfs.Node,
        option: ParseOption,
    ) Err!Parser {
        var result: Parser = .{
            .alc = allocator,
            .tmp_buf = try std.heap.page_allocator.alloc(u8, 1024 * 1024),
            .tmp_alc = undefined,
            .file = file,
            .option = option,
            .tags = undefined,
            .tags_location = undefined,
            .cursors = std.ArrayList(u32).init(allocator),
            .cursor = 0,
            .ast = ast.Ast.init(allocator, file),

            .err = undefined,
            .err_msg = "everything's fine",
        };
        try result.ast.nodes.append(Tag.invalid.into());
        result.tmp_alc = std.heap.FixedBufferAllocator.init(result.tmp_buf);
        if (option.mode == .debug) {
            result.tags = std.ArrayList(ast.Tag).init(allocator);
            result.tags_location = std.ArrayList(u64).init(allocator);
        }
        return result;
    }

    pub fn peek(self: Parser, tokens: []const w.Token.Tag) bool {
        if (self.cursor + tokens.len >= self.file.tokens.?.items.len)
            return false;

        for (tokens, 0..) |token, i| {
            if (self.file.tokens.?.items[self.cursor + 1 + i].tag != token)
                return false;
        }

        return true;
    }

    // if next token is not the same as token, return false
    // if next token is the same as token, return true and move cursor
    pub fn eat(self: *Parser, token: w.Token.Tag) bool {
        if (self.cursor + 1 >= self.file.tokens.?.items.len)
            return false;

        if (self.file.tokens.?.items[self.cursor + 1].tag != token)
            return false;

        self.cursor += 1;
        return true;
    }

    pub fn peekToken(self: Parser) w.Token {
        if (self.cursor + 1 >= self.file.tokens.?.items.len)
            return w.Token{ .tag = .eof, .from = 0, .to = 0 };

        return self.file.tokens.?.items[self.cursor + 1];
    }

    pub fn nextToken(self: Parser) w.Token {
        if (self.cursor + 1 >= self.file.tokens.?.items.len)
            return w.Token{ .tag = .eof, .from = 0, .to = 0 };

        self.cursor += 1;
        return self.file.tokens.?.items[self.cursor];
    }

    pub fn currentToken(self: Parser) w.Token {
        if (self.cursor >= self.file.tokens.?.items.len)
            return w.Token{ .tag = .eof, .from = 0, .to = 0 };

        return self.file.tokens.?.items[self.cursor];
    }

    pub inline fn eatTokens(self: *Parser, count: u32) void {
        self.cursor += count;
    }

    const NodeKind = enum {
        tag,
        len,
        token_index,
        child,
        property,
    };
    pub fn push(self: *Parser, item: u64, kind: NodeKind) Err!u64 {
        try self.ast.nodes.append(item);
        if (kind == .tag) {
            try self.tags.append(@enumFromInt(item));
            try self.tags_location.append(self.ast.nodes.items.len);
            try self.pushSpan(self.ast.nodes.items.len - 1);
        }
        return self.ast.nodes.items.len - 1;
    }

    // the first element shall be the tag
    pub fn pushNode(self: *Parser, node: anytype) Err!u64 {
        var result: u64 = 0;
        switch (node.len) {
            // single tag
            1 => {
                result = try self.push(@intFromEnum(node[0]), .tag);
            },
            // with single child or multiple children
            2 => {
                result = try self.push(@intFromEnum(node[0]), .tag);
                if (@TypeOf(node[1]) == []u64) {
                    const len = node[1].len;
                    _ = try self.push(len, .len);
                    try self.ast.nodes.appendSlice(node[1]);
                } else {
                    _ = try self.push(node[1], .child);
                }
            },
            // with two children or one left and multiple children
            3 => {
                result = try self.push(@intFromEnum(node[0]), .tag);
                if (@TypeOf(node[2]) == []u64) {
                    _ = try self.push(node[1], .child);
                    const len = node[2].len;
                    _ = try self.push(len, .len);
                    try self.ast.nodes.appendSlice(node[2]);
                } else {
                    _ = try self.push(node[1], .child);
                    _ = try self.push(node[2], .child);
                }
            },
            // three children or two left and multiple children
            4 => {
                result = try self.push(@intFromEnum(node[0]), .tag);
                if (@TypeOf(node[3]) == []u64) {
                    _ = try self.push(node[1], .child);
                    _ = try self.push(node[2], .child);
                    const len = node[3].len;
                    _ = try self.push(len, .len);
                    try self.ast.nodes.appendSlice(node[3]);
                } else {
                    _ = try self.push(node[1], .child);
                    _ = try self.push(node[2], .child);
                    _ = try self.push(node[3], .child);
                }
            },
            // for lambda
            5 => {
                result = try self.push(@intFromEnum(node[0]), .tag);
                _ = try self.push(node[1], .child);
                _ = try self.push(node[2], .child);
                _ = try self.push(node[3], .child);
                if (@TypeOf(node[4]) == []u64) {
                    const len = node[4].len;
                    _ = try self.push(len, .len);
                    try self.ast.nodes.appendSlice(node[4]);
                } else {
                    _ = try self.push(node[4], .child);
                }
            },
            // for function definition
            6 => {
                result = try self.push(@intFromEnum(node[0]), .tag);
                _ = try self.push(node[1], .child);
                _ = try self.push(node[2], .child);
                _ = try self.push(node[3], .child);
                _ = try self.push(node[4], .child);
                if (@TypeOf(node[5]) == []u64) {
                    const len = node[5].len;
                    _ = try self.push(len, .len);
                    try self.ast.nodes.appendSlice(node[5]);
                } else {
                    _ = try self.push(node[5], .child);
                }
            },
            else => {
                @compileLog(node);
                @compileError("unsupported node length");
            },
        }

        return result;
    }

    pub fn pushSpan(self: *Parser, index: u64) Err!void {
        const span = ast.Node2Span{
            .node = index,
            .from = self.cursors.items[self.cursors.items.len - 1] + 1,
            .to = self.cursor,
        };
        try self.ast.spans.append(span);
    }

    pub fn enter(self: *Parser) Err!void {
        try self.cursors.append(self.cursor);
    }

    pub fn exit(self: *Parser) void {
        _ = self.cursors.pop();
    }

    pub fn tryExpr(self: *Parser) Err!u64 {
        return try exprs.tryExpr(self, .{});
    }

    pub fn tryPattern(self: *Parser) Err!u64 {
        return try patterns.tryPattern(self, .{});
    }

    pub fn tryStatement(self: *Parser) Err!u64 {
        return try statements.tryStatement(self);
    }

    pub fn parse(self: *Parser) Err!void {
        defer {
            self.file.ast = self.ast;
            self.cursors.deinit();
            std.heap.page_allocator.free(self.tmp_buf);

            if (self.option.mode == .debug) {
                self.tags_location.deinit();
                self.tags.deinit();
            }
        }

        self.ast.root = statements.tryFileScope(self) catch |e| {
            switch (e) {
                error.UnexpectedToken => {
                    self.file.report(
                        self.alc,
                        self.err.unexpected_token.got,
                        .err,
                        e,
                        self.err_msg,
                        3,
                    ) catch @panic("failed to report error");
                },
                error.InvalidExpr => {
                    self.file.report(
                        self.alc,
                        self.err.invalid_expr.got,
                        .err,
                        e,
                        self.err_msg,
                        3,
                    ) catch @panic("failed to report error");
                },
                error.InvalidPattern => {
                    self.file.report(
                        self.alc,
                        self.err.invalid_pattern.got,
                        .err,
                        e,
                        self.err_msg,
                        3,
                    ) catch @panic("failed to report error");
                },
                error.InvalidModPath => {
                    self.file.report(
                        self.alc,
                        self.err.invalid_path.got,
                        .err,
                        e,
                        self.err_msg,
                        3,
                    ) catch @panic("failed to report error");
                },
                error.InvalidStatement => {
                    self.file.report(
                        self.alc,
                        self.err.invalid_statement.got,
                        .err,
                        e,
                        self.err_msg,
                        3,
                    ) catch @panic("failed to report error");
                },

                else => @panic("unknown error"),
            }
            self.ast.root = 0;

            for (self.ast.nodes.items, 0..) |node, i| {
                std.debug.print("<{}, {}>,", .{ i, node });
            }
            std.debug.print("\n", .{});
            for (self.tags.items, 0..) |node, i| {
                std.debug.print("<{}, {s}>,", .{ i, node.toString() });
            }
            std.debug.print("\n", .{});
            for (self.tags_location.items, 0..) |node, i| {
                std.debug.print("<{}, {}>,", .{ i, node });
            }
            std.debug.print("\n", .{});
            return;
        };
    }

    pub fn expectNextToken(self: *Parser, tag: w.Token.Tag, msg: []const u8) Err!void {
        if (!self.eat(tag)) {
            self.err = ErrPayload{ .unexpected_token = .{
                .got = self.peekToken(),
                .span = w.LSpan{
                    .from = self.cursors.items[self.cursors.items.len - 1] + 1,
                    .to = self.cursor,
                },
            } };
            self.err_msg = msg;
            return Err.UnexpectedToken;
        }
    }

    pub fn unexpectedToken(self: *Parser, msg: []const u8) Err {
        self.err = ErrPayload{ .unexpected_token = .{
            .got = self.peekToken(),
            .span = w.LSpan{
                .from = self.cursors.items[self.cursors.items.len - 1] + 1,
                .to = self.cursor,
            },
        } };
        self.err_msg = msg;
        return Err.UnexpectedToken;
    }

    pub fn invalidExpr(
        self: *Parser,
        msg: []const u8,
    ) Err {
        self.err = ErrPayload{
            .invalid_expr = .{
                .got = self.peekToken(),
                .span = w.LSpan{
                    .from = self.cursors.items[self.cursors.items.len - 1] + 1,
                    .to = self.cursor,
                },
            },
        };
        self.err_msg = msg;
        return Err.InvalidExpr;
    }

    pub fn invalidPattern(
        self: *Parser,
        msg: []const u8,
    ) Err {
        self.err = ErrPayload{
            .invalid_pattern = .{
                .got = self.peekToken(),
                .span = w.LSpan{
                    .from = self.cursors.items[self.cursors.items.len - 1] + 1,
                    .to = self.cursor,
                },
            },
        };
        self.err_msg = msg;
        return Err.InvalidPattern;
    }

    pub fn invalidPath(
        self: *Parser,
        msg: []const u8,
    ) Err {
        self.err = ErrPayload{
            .invalid_expr = .{
                .got = self.peekToken(),
                .span = w.LSpan{
                    .from = self.cursors.items[self.cursors.items.len - 1] + 1,
                    .to = self.cursor,
                },
            },
        };
        self.err_msg = msg;
        return Err.InvalidModPath;
    }

    pub fn invalidStatement(
        self: *Parser,
        msg: []const u8,
    ) Err {
        self.err = ErrPayload{
            .invalid_statement = .{
                .got = self.peekToken(),
                .span = w.LSpan{
                    .from = self.cursors.items[self.cursors.items.len - 1] + 1,
                    .to = self.cursor,
                },
            },
        };
        self.err_msg = msg;
        return Err.InvalidStatement;
    }

    pub fn invalidDefinition(
        self: *Parser,
        msg: []const u8,
    ) Err {
        self.err = ErrPayload{
            .invalid_definition = .{
                .got = self.peekToken(),
                .span = w.LSpan{
                    .from = self.cursors.items[self.cursors.items.len - 1] + 1,
                    .to = self.cursor,
                },
            },
        };
        self.err_msg = msg;
        return Err.InvalidDefinition;
    }
};

const Err = w.Err;

const Tag = ast.Tag;

pub const ErrPayload = union {
    unexpected_token: struct {
        got: w.Token,
        span: w.LSpan,
    },
    invalid_expr: struct {
        got: w.Token,
        span: w.LSpan,
    },
    invalid_pattern: struct {
        got: w.Token,
        span: w.LSpan,
    },
    invalid_path: struct {
        got: w.Token,
        span: w.LSpan,
    },
    invalid_statement: struct {
        got: w.Token,
        span: w.LSpan,
    },
    invalid_definition: struct {
        got: w.Token,
        span: w.LSpan,
    },
};

pub const ParseOption = struct {
    mode: Mode = .debug,

    const Mode = enum {
        debug,
        release,
    };
};
