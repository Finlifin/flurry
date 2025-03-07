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
    self.eatTokens(1);
    return result;
}

pub fn tryProperty(self: *Parser) Err!u64 {
    try self.enter();
    defer self.exit();

    if (!self.peek(&.{ .@".", .id })) return 0;
    self.eatTokens(2);

    const result = try self.pushNode(.{ Tag.property, try self.tryExpr() });
    self.eatTokens(1);
    return result;
}
