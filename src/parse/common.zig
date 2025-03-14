const std = @import("std");
const w = @import("../common.zig");
const Err = w.Err;
const Parser = @import("parser.zig").Parser;
const ast = @import("ast.zig");
const Tag = ast.Tag;
const exprs = @import("expr.zig");

pub fn tryLiteral(self: *Parser) Err!u64 {
    try self.enter();
    defer self.exit();

    const token = self.peekToken();
    var result: u64 = 0;
    switch (token.tag) {
        .int => result = try self.pushNode(.{ Tag.int, self.cursor + 1 }),
        .real => result = try self.pushNode(.{ Tag.real, self.cursor + 1 }),
        .char => result = try self.pushNode(.{ Tag.char, self.cursor + 1 }),
        .str => result = try self.pushNode(.{ Tag.str, self.cursor + 1 }),
        .id => result = try self.pushNode(.{ Tag.id, self.cursor + 1 }),
        .k_false => result = try self.pushNode(.{ Tag.bool, self.cursor + 1 }),
        .k_true => result = try self.pushNode(.{ Tag.bool, self.cursor + 1 }),
        .k_null => result = try self.pushNode(.{ Tag.null, self.cursor + 1 }),
        .k_self => result = try self.pushNode(.{ Tag.self, self.cursor + 1 }),
        .k_Self => result = try self.pushNode(.{ Tag.Self, self.cursor + 1 }),
        .k_itself => result = try self.pushNode(.{ Tag.itself, self.cursor + 1 }),
        else => {},
    }
    if (result != 0)
        self.eatTokens(1);
    return result;
}

pub fn tryId(self: *Parser) Err!u64 {
    try self.enter();
    defer self.exit();

    if (!self.peek(&.{.id})) return 0;

    const result = try self.pushNode(.{ Tag.id, self.cursor + 1 });
    self.eatTokens(1);
    return result;
}

pub fn trySymbol(self: *Parser) Err!u64 {
    try self.enter();
    defer self.exit();

    if (!self.peek(&.{ .@".", .id })) return 0;
    self.eatTokens(1);

    const result = try self.pushNode(.{ Tag.symbol, try tryId(self) });
    return result;
}

pub fn tryProperty(self: *Parser) Err!u64 {
    try self.enter();
    defer self.exit();
    if (!self.peek(&.{ .@".", .id, .@"=" })) return 0;

    self.eatTokens(1);
    const id = try tryId(self);
    self.eatTokens(1);
    const result = try self.pushNode(.{ Tag.property, id, try self.tryExpr() });

    return result;
}

pub fn tryExpendItems(self: *Parser) Err!u64 {
    try self.enter();
    defer self.exit();

    if (!self.peek(&.{ .@".", .@".", .@"." })) return 0;
    self.eatTokens(3);

    const expr = try exprs.tryExpr(self, .{ .no_record_call = true });
    const result = try self.pushNode(.{ Tag.expand_items, expr });
    return result;
}

const Rule = struct {
    name: []const u8,
    p: fn (self: *Parser) Err!u64,

    // 你可能会问，为什么不是给整个函数指定delimiter, 而是给每个rule指定delimiter
    // 因为以前还没有句末省略分号的设计
    delimiter: w.Token.Tag = .@",",
};

pub fn rule(name: []const u8, p: fn (self: *Parser) Err!u64) Rule {
    return Rule{ .name = name, .p = p };
}

pub fn ruleWithDelimiter(name: []const u8, p: fn (self: *Parser) Err!u64, delimiter: w.Token.Tag) Rule {
    return Rule{ .name = name, .p = p, .delimiter = delimiter };
}

// BUG: 返回的nodes貌似无法正常deinit? 目前使用tmp_alc来统一释放。可以使用gpa来复现bug
pub fn pMulti(
    self: *Parser,
    comptime rules: anytype,
    comptime br: ?w.Token.Tag,
) Err!std.ArrayList(u64) {
    if (rules.len == 0) @compileError("provide at least one rule");
    const close_br: ?w.Token.Tag = if (br) |bracket| switch (bracket) {
        .@"{" => .@"}",
        .@"[" => .@"]",
        .@"(" => .@")",
        .@"<" => .@">",
        .@"|" => .@"|",
        .eof => .eof,
        else => @compileError("unsupported bracket"),
    } else null;
    if (br) |b| if (b != .eof)
        try self.expectNextToken(b, "expected an opening token");

    var nodes = std.ArrayList(u64).init(self.tmp_alc.allocator());
    if (close_br) |close|
        if (self.eat(close)) return nodes;

    while (true) {
        var node: u64 = 0;
        var delimiter: w.Token.Tag = .@",";

        inline for (rules) |r| {
            if (@TypeOf(r) != Rule) @compileError("rules should be a tuple whose elements are of type `Rule`");

            node = try r.p(self);
            if (node != 0) {
                delimiter = r.delimiter;
                break;
            }
        }

        if (node == 0) {
            // std.debug.print("DEBUG: WTF {any}\n", .{self.peekToken()});
            if (close_br) |close|
                if (self.eat(close))
                    break
                else
                    return self.unexpectedToken("expected a term or a closing token");
        }
        try nodes.append(node);

        // 如果是语句，如果最后一个token的最后一个字符是`}`，那么就不需要分号了
        if (delimiter == .@";") {
            const current = self.currentToken();
            const last_char = self.file.content.?[current.to - 1];
            if (last_char == '}') continue;
        }

        if (close_br) |close| {
            if (self.eat(close)) break;
            try self.expectNextToken(delimiter, "expected a delimiter or a closing token");
            if (self.eat(close)) break;
        } else {
            // if eat one delimiter, then go for next
            if (self.eat(delimiter))
                continue
            else
                break;
        }
    }

    return nodes;
}

// inline term
pub fn tryInline(self: *Parser, r: Rule) Err!u64 {
    try self.enter();
    defer self.exit();

    if (!self.eat(.k_inline)) return 0;

    const result = try self.pushNode(.{ Tag.inline_def, try r.p(self) });
    return result;
}

// pub term
pub fn tryPub(self: *Parser, r: Rule) Err!u64 {
    try self.enter();
    defer self.exit();

    if (!self.eat(.k_pub)) return 0;

    const result = try self.pushNode(.{ Tag.pub_def, try r.p(self) });
    return result;
}

// comptime term
pub fn tryComptime(self: *Parser, r: Rule) Err!u64 {
    try self.enter();
    defer self.exit();

    if (!self.eat(.k_comptime)) return 0;

    const result = try self.pushNode(.{ Tag.comptime_def, try r.p(self) });
    return result;
}

// pure term
pub fn tryPure(self: *Parser, r: Rule) Err!u64 {
    try self.enter();
    defer self.exit();

    if (!self.eat(.k_pure)) return 0;

    const result = try self.pushNode(.{ Tag.pure_def, try r.p(self) });
    return result;
}

// ^expr term
pub fn tryExpr(self: *Parser, r: Rule) Err!u64 {
    try self.enter();
    defer self.exit();

    if (!self.eat(.@"^")) return 0;
    const expr = exprs.tryExpr(self, .{ .no_record_call = true });

    const result = try self.pushNode(.{ Tag.attr_def, expr, try r.p(self) });
    return result;
}
