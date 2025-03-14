const std = @import("std");
const w = @import("../common.zig");
const Err = w.Err;
const Parser = @import("parser.zig").Parser;
const ast = @import("ast.zig");
const Tag = ast.Tag;
const common = @import("common.zig");
const statements = @import("statement.zig");
const exprs = @import("expr.zig");

const PatternOption = struct {
    no_record_call: bool = false,
};

pub fn tryPattern(
    self: *Parser,
    opt: PatternOption,
) Err!u64 {
    try self.enter();
    defer self.exit();

    return try tryPratt(self, 0, opt);
}

const OpInfo = struct {
    prec: i8,
    tag: ast.Tag,
    assoc_left: bool = true,
};

const op_table = std.enums.directEnumArrayDefault(
    w.Token.Tag,
    OpInfo,
    .{ .prec = -1, .tag = .invalid },
    0,
    .{
        .k_if = .{ .prec = 10, .tag = .pattern_if_guard },
        .k_and = .{ .prec = 10, .tag = .pattern_and_is },

        .k_as = .{ .prec = 20, .tag = .pattern_as_bind },

        .k_or = .{ .prec = 30, .tag = .pattern_or },

        .@"?" = .{ .prec = 40, .tag = .pattern_option_some },

        .@"(" = .{ .prec = 80, .tag = .pattern_call },
        .@"{" = .{ .prec = 80, .tag = .pattern_record_call },
        .@"<" = .{ .prec = 80, .tag = .pattern_diamond_call },

        .@"." = .{ .prec = 90, .tag = .select },
    },
);

fn tryPratt(self: *Parser, min_prec: i8, opt: PatternOption) Err!u64 {
    var left = try tryPrefixPattern(self);
    if (left == 0) {
        return 0;
    }

    while (true) {
        const token = self.peekToken();
        const op_info = op_table[@intFromEnum(token.tag)];

        if (op_info.tag == .invalid or op_info.prec < min_prec)
            break;

        const post = tryPostfixPattern(self, token.tag, left, opt) catch |e| {
            if (e == error.NoRecordCall)
                break
            else
                return e;
        };
        if (post != 0) {
            left = post;
            continue;
        }

        self.eatTokens(1);
        const right = try tryPratt(self, op_info.prec + 1, opt);
        if (right == 0)
            return self.invalidPattern("missing right operand");

        left = try self.pushNode(.{ op_info.tag, left, right });
    }

    return left;
}

pub fn tryPrefixPattern(self: *Parser) Err!u64 {
    try self.enter();
    defer self.exit();

    const literal = try common.tryLiteral(self);
    if (literal != 0)
        return literal;

    const token = self.peekToken();
    return switch (token.tag) {
        .@"." => {
            var result = try tryRangeTo(self);
            if (result == 0)
                result = try common.trySymbol(self);
            return result;
        },
        .@"[" => try tryListPattern(self),
        .@"(" => try tryTuplePattern(self),
        .@"{" => try tryRecordPattern(self),
        .@"'" => try tryTypeVarPattern(self),
        .@"<" => try tryPatternFromExpr(self),
        else => 0,
    };
}

pub fn tryPostfixPattern(self: *Parser, tag: w.Token.Tag, left: u64, opt: PatternOption) Err!u64 {
    var result: u64 = 0;
    switch (tag) {
        .@"(" => {
            const rules = .{
                common.rule("pattern", Parser.tryPattern),
            };

            const nodes = try common.pMulti(self, rules, .@"(");
            defer nodes.deinit();
            result = try self.pushNode(.{ Tag.pattern_call, left, nodes.items });
        },
        .@"<" => {
            const rules = .{
                common.rule("pattern", Parser.tryPattern),
            };
            const nodes = try common.pMulti(self, rules, .@"<");
            defer nodes.deinit();
            result = try self.pushNode(.{ Tag.pattern_diamond_call, left, nodes.items });
        },
        .@"." => {
            self.eatTokens(1);
            if (self.peek(&.{ .@".", .@"=" })) {
                self.eatTokens(2);
                const end = try tryPattern(self, .{ .no_record_call = true });

                if (end == 0)
                    return self.invalidPattern("expected an expression after `..=`");
                result = try self.pushNode(.{ Tag.pattern_range_from_to_inclusive, left, end });
            } else {
                self.eatTokens(1);
                const end = try tryPattern(self, .{ .no_record_call = true });
                if (end == 0)
                    result = try self.pushNode(.{ Tag.pattern_range_from, left })
                else
                    result = try self.pushNode(.{ Tag.pattern_range_from_to, left, end });
            }
        },
        .@"{" => {
            if (opt.no_record_call)
                return error.NoRecordCall;

            const rules = .{
                common.rule("property", tryPropertyPattern),
                common.rule("id", common.tryId),
            };

            const nodes = try common.pMulti(self, rules, .@"{");
            defer nodes.deinit();
            result = try self.pushNode(.{ Tag.pattern_record_call, left, nodes.items });
        },
        .k_as => {
            self.eatTokens(1);
            const id = try common.tryId(self);
            result = try self.pushNode(.{ Tag.pattern_as_bind, left, id });
        },
        .k_if => {
            self.eatTokens(1);
            const guard = try exprs.tryExpr(self, .{ .no_record_call = true });
            result = try self.pushNode(.{ Tag.pattern_if_guard, left, guard });
        },
        .k_and => {
            self.eatTokens(1);
            const expr = try self.tryExpr();
            if (!self.eat(.k_is))
                return self.invalidPattern("missing 'is' after 'and'");

            const pattern = try tryPattern(self, .{ .no_record_call = true });
            result = try self.pushNode(.{ Tag.pattern_and_is, left, expr, pattern });
        },
        .@"?" => {
            self.eatTokens(1);
            result = try self.pushNode(.{ Tag.pattern_option_some, left });
        },
        else => {},
    }
    return result;
}

fn tryPatternFromExpr(self: *Parser) Err!u64 {
    try self.enter();
    defer self.exit();

    if (!self.eat(.@"<")) return 0;

    const expr = try exprs.tryExpr(self, .{ .no_record_call = true });
    if (expr == 0)
        return self.invalidPattern("expected an expression after `<`");

    try self.expectNextToken(.@">", "expected `>` after expression");

    return try self.pushNode(.{ Tag.pattern_from_expr, expr });
}

fn tryTypeVarPattern(self: *Parser) Err!u64 {
    try self.enter();
    defer self.exit();

    if (!self.peek(&.{ .@"'", .id })) return 0;
    self.eatTokens(1);

    const id = try common.tryId(self);
    return try self.pushNode(.{ Tag.pattern_type_bind, id });
}

fn tryRangeTo(self: *Parser) Err!u64 {
    try self.enter();
    defer self.exit();

    if (self.peek(&.{ .@".", .@".", .@"=" })) {
        self.eatTokens(3);
        const end = try tryPattern(self, .{ .no_record_call = true });
        if (end == 0)
            return self.invalidPattern("expected an pattern after `..=`");

        return try self.pushNode(.{ Tag.pattern_range_to_inclusive, end });
    } else if (self.peek(&.{ .@".", .@"." })) {
        self.eatTokens(2);
        const end = try tryPattern(self, .{ .no_record_call = true });
        if (end == 0)
            return self.invalidPattern("expected an pattern after `..`");

        return try self.pushNode(.{ Tag.pattern_range_to, end });
    }
    return 0;
}

fn tryPropertyPattern(self: *Parser) Err!u64 {
    try self.enter();
    defer self.exit();

    if (!self.peek(&.{ .id, .@":" })) return 0;

    const id = try common.tryId(self);
    self.eatTokens(1);
    return try self.pushNode(.{ Tag.property_pattern, id, try self.tryPattern() });
}

fn tryRecordPattern(self: *Parser) Err!u64 {
    try self.enter();
    defer self.exit();

    if (!self.peek(&.{.@"{"})) return 0;
    const rules = .{
        common.rule("property", tryPropertyPattern),
        common.rule("id", common.tryId),
    };
    const nodes = try common.pMulti(self, rules, .@"{");
    defer nodes.deinit();
    return try self.pushNode(.{ Tag.pattern_record, nodes.items });
}

fn tryListPattern(self: *Parser) Err!u64 {
    try self.enter();
    defer self.exit();

    if (!self.peek(&.{.@"["})) return 0;
    const rules = .{
        common.rule("pattern", Parser.tryPattern),
    };
    const nodes = try common.pMulti(self, rules, .@"[");
    defer nodes.deinit();
    return try self.pushNode(.{ Tag.pattern_list, nodes.items });
}

fn tryTuplePattern(self: *Parser) Err!u64 {
    try self.enter();
    defer self.exit();

    if (!self.peek(&.{.@"("})) return 0;
    const rules = .{
        common.rule("pattern", Parser.tryPattern),
    };
    const nodes = try common.pMulti(self, rules, .@"(");
    defer nodes.deinit();
    return try self.pushNode(.{ Tag.pattern_tuple, nodes.items });
}
