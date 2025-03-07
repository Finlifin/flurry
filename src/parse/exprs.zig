const std = @import("std");
const w = @import("../common.zig");
const Err = w.Err;
const Parser = @import("parser.zig").Parser;
const ast = @import("ast.zig");
const Tag = ast.Tag;
const common = @import("common.zig");

pub const ExprOption = struct {
    no_record_call: bool = false,
};

pub fn tryExpr(
    self: *Parser,
    opt: ExprOption,
) Err!u64 {
    try self.enter();
    defer self.exit();

    return try tryPratt(self, 0, opt);
}

const OpInfo = struct {
    prec: i8,
    tag: ast.Tag,
};

const op_table = std.enums.directEnumArrayDefault(
    w.Token.Tag,
    OpInfo,
    .{ .prec = -1, .tag = .invalid },
    0,
    .{
        .@"==>" = .{ .prec = 10, .tag = .bool_implies },

        .k_or = .{ .prec = 20, .tag = .bool_or },

        .k_and = .{ .prec = 30, .tag = .bool_and },

        .@"!=" = .{ .prec = 40, .tag = .bool_not_eq },
        .@"==" = .{ .prec = 40, .tag = .bool_eq },
        .@">=" = .{ .prec = 40, .tag = .bool_gt_eq },
        .@" > " = .{ .prec = 40, .tag = .bool_gt },
        .@"<=" = .{ .prec = 40, .tag = .bool_lt_eq },
        .@" < " = .{ .prec = 40, .tag = .bool_lt },
        .@":" = .{ .prec = 40, .tag = .type_with },
        .@":-" = .{ .prec = 40, .tag = .trait_bound },

        .@" + " = .{ .prec = 60, .tag = .add },
        .@" - " = .{ .prec = 60, .tag = .sub },

        .@" / " = .{ .prec = 70, .tag = .div },
        .@" * " = .{ .prec = 70, .tag = .mul },
        .@" % " = .{ .prec = 70, .tag = .mod },
        .@"++" = .{ .prec = 70, .tag = .add_add },

        .@"|" = .{ .prec = 80, .tag = .pipe },
        .@"|>" = .{ .prec = 80, .tag = .pipe_prepend },

        // .@"(" = .{ .prec = 90, .tag = .call },
        // .@"[" = .{ .prec = 90, .tag = .index_call },
        // .@"{" = .{ .prec = 90, .tag = .object_call },
        // .@"<" = .{ .prec = 90, .tag = .diamond_call },
        // .@"#" = .{ .prec = 90, .tag = .effect_elimination },
        // .@"!" = .{ .prec = 90, .tag = .error_elimination },
        // .@"?" = .{ .prec = 90, .tag = .option_elimination },
        // .k_match = .{ .prec = 90, .tag = .match },

        .@"." = .{ .prec = 100, .tag = .select },
        .@"'" = .{ .prec = 100, .tag = .image },
    },
);

fn tryPratt(self: *Parser, min_prec: i8, opt: ExprOption) Err!u64 {
    var left = try tryPrefixExpr(self);
    if (left == 0) {
        return 0;
    }

    while (true) {
        const token = self.peekToken();
        const op_info = op_table[@intFromEnum(token.tag)];

        if (op_info.tag == .invalid or op_info.prec < min_prec)
            break;

        // switch (token.tag) {
        //     .@"(" => {
        //         const rules = .{
        //             basic.rule("property", basic.tryProperty),
        //             basic.rule("expr", Parser.tryExpr),
        //         };

        //         const nodes = try basic.pMulti(self, rules, .@"(");
        //         defer nodes.deinit();
        //         left = try self.pushNode(.{ Tag.call, left, nodes.items });

        //         continue;
        //     },
        //     .@"<" => {
        //         const rules = .{
        //             basic.rule("property", basic.tryProperty),
        //             basic.rule("expr", Parser.tryExpr),
        //         };

        //         const nodes = try basic.pMulti(self, rules, .@"<");
        //         defer nodes.deinit();
        //         left = try self.pushNode(.{ Tag.diamond_call, left, nodes.items });

        //         continue;
        //     },
        //     .@"{" => {
        //         if (opt.no_object_call) {
        //             break;
        //         }
        //         const rules = .{
        //             basic.rule("property", basic.tryProperty),
        //             basic.rule("expr", Parser.tryExpr),
        //         };

        //         const nodes = try basic.pMulti(self, rules, .@"{");
        //         defer nodes.deinit();
        //         left = try self.pushNode(.{ Tag.object_call, left, nodes.items });

        //         continue;
        //     },
        //     .@"[" => {
        //         self.eatTokens(1);
        //         const index_expr = try self.tryExpr();
        //         try self.expectNextToken(.@"]", "expect a `]` to close index call");
        //         left = try self.pushNode(.{ Tag.index_call, left, index_expr });
        //         continue;
        //     },
        //     .@"#" => {
        //         self.eatTokens(1);
        //         switch (self.peekToken().tag) {
        //             .@"{" => {
        //                 const branches = try pattern.tryBranches(self);
        //                 left = try self.pushNode(.{ Tag.effect_elimination, left, branches });
        //                 continue;
        //             },
        //             .k_use => {
        //                 _ = self.nextToken();
        //                 const expr = try tryPrefixExpr(self);
        //                 left = try self.pushNode(.{ Tag.effect_elimination_use, left, expr });
        //                 continue;
        //             },
        //             else => {
        //                 left = try self.pushNode(.{ Tag.effect_elimination_unwrap, left });
        //                 continue;
        //             },
        //         }
        //     },
        //     .@"!" => {
        //         self.eatTokens(1);
        //         std.debug.print("DEBUG: {any}\n", .{self.peekToken()});
        //         switch (self.peekToken().tag) {
        //             .@"{" => {
        //                 const branches = try pattern.tryBranches(self);
        //                 left = try self.pushNode(.{ Tag.error_elimination, left, branches });
        //                 continue;
        //             },
        //             .k_use => {
        //                 self.eatTokens(1);
        //                 const expr = try tryPrefixExpr(self);
        //                 left = try self.pushNode(.{ Tag.error_elimination_use, left, expr });
        //                 continue;
        //             },
        //             else => {
        //                 left = try self.pushNode(.{ Tag.error_elimination_unwrap, left });
        //                 continue;
        //             },
        //         }
        //     },
        //     .@"?" => {
        //         self.eatTokens(1);
        //         var block: u64 = 0;
        //         block = try stmt.tryBlock(self);

        //         if (block != 0) {
        //             left = try self.pushNode(.{ Tag.option_elimination, left, block });
        //             continue;
        //         }
        //         left = try self.pushNode(.{ Tag.option_elimination_unwrap, left });
        //         continue;
        //     },
        //     .k_match => {
        //         self.eatTokens(1);
        //         const branches = try pattern.tryBranches(self);
        //         left = try self.pushNode(.{ Tag.match, left, branches });
        //         continue;
        //     },
        //     // it might be a select, @"pattern . *", range_from, range_from_to, range_from_to_inclusive
        //     .@"." => {
        //         try self.enter();
        //         defer self.exit();
        //         self.eatTokens(1);
        //         if (self.peek(&.{.id})) {
        //             const id = try self.pushAtom(.id);
        //             left = try self.pushNode(.{ Tag.select, left, id });
        //             continue;
        //         }

        //         if (self.eatToken(.@".")) {
        //             if (basic.isTerminator(self.peekToken())) {
        //                 left = try self.pushNode(.{ Tag.range_from, left });
        //                 continue;
        //             }

        //             const tag = if (self.eatToken(.@"=")) Tag.range_from_to_inclusive else Tag.range_from_to;
        //             const expr = try tryPrefixExpr(self);

        //             left = try self.pushNode(.{ tag, left, expr });
        //             continue;
        //         }

        //         return self.invalidExpr(self.rcursor, self.rcursor, "expect an identifier or a range after a `.`");
        //     },

        //     .@"'" => {
        //         _ = self.nextToken();
        //         const id = try self.pushAtom(.id);
        //         left = try self.pushNode(.{ Tag.image, left, id });
        //         continue;
        //     },
        //     else => {},
        // }
        self.eatTokens(1);
        const right = try tryPratt(self, op_info.prec + 1, opt);
        if (right == 0)
            return self.invalidExpr("expect an expression after a binary operator");

        left = try self.pushNode(.{ op_info.tag, left, right });
    }

    return left;
}

pub fn tryPrefixExpr(
    self: *Parser,
) Err!u64 {
    try self.enter();
    defer self.exit();

    const literal = try common.tryLiteral(self);
    if (literal != 0)
        return literal;

    const token = self.peekToken();
    return switch (token.tag) {
        .@"." => try common.trySymbol(self),
        else => 0,
    };
}
