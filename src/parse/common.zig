const std = @import("std");
const w = @import("../common.zig");
const Err = w.Err;
const Parser = @import("parser.zig").Parser;
const ast = @import("ast.zig");
const Tag = ast.Tag;

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

const Rule = struct {
    name: []const u8,
    p: fn (self: *Parser) Err!u64,
    delimiter: w.Token.Tag = .@",",
};

pub fn rule(name: []const u8, p: fn (self: *Parser) Err!u64) Rule {
    return Rule{ .name = name, .p = p };
}

pub fn ruleWithDelimiter(name: []const u8, p: fn (self: *Parser) Err!u64, delimiter: w.Token.Tag) Rule {
    return Rule{ .name = name, .p = p, .delimiter = delimiter };
}

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
    if (br) |b|
        try self.expectNextToken(b, "expected an opening bracket");

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
        try nodes.append(node);

        if (close_br) |close| {
            if (self.eat(close)) break;
            try self.expectNextToken(delimiter, "expected a delimiter or a closing bracket");
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
