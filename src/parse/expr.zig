const std = @import("std");
const w = @import("../common.zig");
const Err = w.Err;
const Parser = @import("parser.zig").Parser;
const ast = @import("ast.zig");
const Tag = ast.Tag;
const common = @import("common.zig");
const statements = @import("statement.zig");
const definitions = @import("definition.zig");

pub const ExprOption = struct {
    no_record_call: bool = false,
};

pub fn tryExpr(self: *Parser, opt: ExprOption) Err!u64 {
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
        .k_matches = .{ .prec = 40, .tag = .bool_matches },

        .@" + " = .{ .prec = 60, .tag = .add },
        .@" - " = .{ .prec = 60, .tag = .sub },

        .@" / " = .{ .prec = 70, .tag = .div },
        .@" * " = .{ .prec = 70, .tag = .mul },
        .@" % " = .{ .prec = 70, .tag = .mod },
        .@"++" = .{ .prec = 70, .tag = .add_add },

        .@"|" = .{ .prec = 80, .tag = .pipe },
        .@"|>" = .{ .prec = 80, .tag = .pipe_prepend },

        .@"(" = .{ .prec = 90, .tag = .call },
        .@"[" = .{ .prec = 90, .tag = .index_call },
        .@"{" = .{ .prec = 90, .tag = .record_call },
        .@"<" = .{ .prec = 90, .tag = .diamond_call },
        .@"#" = .{ .prec = 90, .tag = .effect_elimination },
        .@"!" = .{ .prec = 90, .tag = .error_elimination },
        .@"?" = .{ .prec = 90, .tag = .option_elimination },
        .k_match = .{ .prec = 90, .tag = .post_match },

        .@"." = .{ .prec = 100, .tag = .select },
        .@"'" = .{ .prec = 100, .tag = .image },

        // literal extension
        .id = .{ .prec = 110, .tag = .id },
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

        const post = tryPostfixExpr(self, token.tag, left, opt) catch |e| {
            switch (e) {
                error.NoRecordCall, error.EndOfTerm => break,
                else => return e,
            }
        };
        if (post != 0) {
            left = post;
            continue;
        }

        self.eatTokens(1);
        const right = try tryPratt(self, op_info.prec + 1, opt);
        if (right == 0)
            return self.invalidExpr("expected an expression after a binary operator");

        left = try self.pushNode(.{ op_info.tag, left, right });
    }

    return left;
}

fn tryPostfixExpr(self: *Parser, tag: w.Token.Tag, left: u64, opt: ExprOption) Err!u64 {
    var result: u64 = 0;
    switch (tag) {
        .@"(" => {
            const rules = .{
                common.rule("expand items", common.tryExpendItems),
                common.rule("property", common.tryProperty),
                common.rule("expr", Parser.tryExpr),
            };

            const nodes = try common.pMulti(self, rules, .@"(");
            defer nodes.deinit();
            result = try self.pushNode(.{ Tag.call, left, nodes.items });
        },
        .@"<" => {
            const rules = .{
                common.rule("property", common.tryProperty),
                common.rule("expr", Parser.tryExpr),
            };

            const nodes = try common.pMulti(self, rules, .@"<");
            defer nodes.deinit();
            result = try self.pushNode(.{ Tag.diamond_call, left, nodes.items });
        },

        .@"{" => {
            if (opt.no_record_call)
                return error.NoRecordCall;

            const rules = .{
                common.rule("expand items", common.tryExpendItems),
                common.rule("property", common.tryProperty),
            };

            const nodes = try common.pMulti(self, rules, .@"{");
            defer nodes.deinit();
            result = try self.pushNode(.{ Tag.record_call, left, nodes.items });
        },
        .@"[" => {
            self.eatTokens(1);
            const index_expr = try self.tryExpr();
            try self.expectNextToken(.@"]", "expected a `]` to close index call");
            result = try self.pushNode(.{ Tag.index_call, left, index_expr });
        },
        .@"." => {
            self.eatTokens(1);
            const next = self.peekToken();
            switch (next.tag) {
                .@"*" => {
                    self.eatTokens(1);
                    result = try self.pushNode(.{ Tag.deref, left });
                },
                .k_use => {
                    self.eatTokens(1);
                    try self.expectNextToken(.@"(", "expected a `(` to start a handler expression");
                    const handler = try self.tryExpr();
                    try self.expectNextToken(.@")", "expected a `)` to close a handler expression");
                    result = try self.pushNode(.{ Tag.handler_apply, left, handler });
                },
                .k_ref => {
                    self.eatTokens(1);
                    result = try self.pushNode(.{ Tag.refer, left });
                },
                .k_dyn => {
                    self.eatTokens(1);
                    try self.expectNextToken(.@"(", "expected a `(` to start a trait expression");
                    const trait = try self.tryExpr();
                    try self.expectNextToken(.@")", "expected a `)` to close a trait expression");
                    result = try self.pushNode(.{ Tag.as_dyn, left, trait });
                },
                .k_as => {
                    self.eatTokens(1);
                    try self.expectNextToken(.@"(", "expected a `(` to start a type cast");
                    const t = try self.tryExpr();
                    try self.expectNextToken(.@")", "expected a `)` to close a type cast");
                    result = try self.pushNode(.{ Tag.type_cast, left, t });
                },
                .@"." => {
                    if (self.peek(&.{ .@".", .@"=" })) {
                        self.eatTokens(2);
                        const end = try tryExpr(self, .{ .no_record_call = true });

                        if (end == 0)
                            return self.invalidExpr("expected an expression after `..=`");
                        result = try self.pushNode(.{ Tag.range_from_to_inclusive, left, end });
                    } else {
                        self.eatTokens(1);
                        const end = try tryExpr(self, .{ .no_record_call = true });
                        if (end == 0)
                            result = try self.pushNode(.{ Tag.range_from, left })
                        else
                            result = try self.pushNode(.{ Tag.range_from_to, left, end });
                    }
                },
                else => {
                    const id = try common.tryId(self);
                    result = try self.pushNode(.{ Tag.select, left, id });
                },
            }
        },
        .@"'" => {
            self.eatTokens(1);
            if (self.peek(&.{.@"("})) {
                const rules = .{
                    common.rule("expand items", common.tryExpendItems),
                    common.rule("property", common.tryProperty),
                    common.rule("expr", Parser.tryExpr),
                };

                const nodes = try common.pMulti(self, rules, .@"(");
                defer nodes.deinit();
                result = try self.pushNode(.{ Tag.curry_call, left, nodes.items });
            } else {
                const id = try common.tryId(self);
                result = try self.pushNode(.{ Tag.image, left, id });
            }
        },
        .@"#" => {
            self.eatTokens(1);
            if (!self.peek(&.{.@"{"}))
                return try self.pushNode(.{ Tag.effect_emit, left });

            const rules = .{
                common.rule("pattern branch", statements.tryBranch),
            };
            const branches = try statements.tryBlock(self, rules);
            result = try self.pushNode(.{ Tag.effect_elimination, left, branches });
        },
        .@"!" => {
            self.eatTokens(1);
            if (!self.peek(&.{.@"{"}))
                return try self.pushNode(.{ Tag.error_throw, left });

            const rules = .{
                common.rule("catch branch", statements.tryCatchBranch),
                common.rule("pattern branch", statements.tryBranch),
            };
            const branches = try statements.tryBlock(self, rules);
            result = try self.pushNode(.{ Tag.error_elimination, left, branches });
        },
        .@"?" => {
            self.eatTokens(1);
            if (!self.peek(&.{.@"{"}))
                return try self.pushNode(.{ Tag.option_unwrap, left });

            const rules = .{
                common.ruleWithDelimiter("statement", statements.tryStatement, .@";"),
            };
            const block = try statements.tryBlock(self, rules);
            result = try self.pushNode(.{ Tag.option_elimination, left, block });
        },
        .k_match => {
            self.eatTokens(1);
            if (!self.peek(&.{.@"{"}))
                return self.invalidExpr("expected a `{` to start a match block");

            const rules = .{
                common.rule("pattern branch", statements.tryBranch),
            };
            const branches = try statements.tryBlock(self, rules);
            result = try self.pushNode(.{ Tag.post_match, left, branches });
        },
        .k_matches => {
            self.eatTokens(1);
            const pattern = try self.tryPattern();
            if (pattern == 0)
                return self.invalidPattern("expected a pattern after `matches`");

            result = try self.pushNode(.{ Tag.bool_matches, left, pattern });
        },
        .id => {
            const current_token = self.file.tokens.?.items[self.cursor];
            switch (current_token.tag) {
                .str => result = try self.pushNode(.{ Tag.str_extension, left, try common.tryId(self) }),
                .int => result = try self.pushNode(.{ Tag.int_extension, left, try common.tryId(self) }),
                .char => result = try self.pushNode(.{ Tag.char_extension, left, try common.tryId(self) }),
                .real => result = try self.pushNode(.{ Tag.real_extension, left, try common.tryId(self) }),
                else => return Err.EndOfTerm,
            }
        },
        // .str => unreachable,
        else => {},
    }

    return result;
}

pub fn tryPrefixExpr(self: *Parser) Err!u64 {
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
        .@"[" => tryList(self),
        .@"(" => tryParenthesisOrTupleOrUnit(self),
        .@"{" => tryRecord(self),
        .@"<" => tryExprFromPattern(self),

        .@"|" => tryClosure(self),

        .k_do => statements.tryABlock(self, .k_do),
        .k_async => statements.tryABlock(self, .k_async),
        .k_comptime => statements.tryABlock(self, .k_comptime),
        .k_unsafe => statements.tryABlock(self, .k_unsafe),
        .k_atomic => statements.tryABlock(self, .k_atomic),

        .k_if => statements.tryIfStatement(self),
        .k_when => statements.tryWhenStatement(self),

        .@"#" => tryEffectQualified(self),
        .@"!" => tryErrorQualified(self),
        .@"?" => tryOptional(self),
        .k_dyn => tryTraitObject(self),
        .@"*" => tryPointerType(self),

        .k_fn, .k_struct, .k_enum, .k_union, .k_extend => try definitions.tryDefinition(self),

        .k_inline => try common.tryInline(self, common.rule("expr", tryPrefixExpr)),
        else => 0,
    };
}

fn tryRangeTo(self: *Parser) Err!u64 {
    try self.enter();
    defer self.exit();

    if (self.peek(&.{ .@".", .@".", .@"=" })) {
        self.eatTokens(3);
        const end = try tryExpr(self, .{ .no_record_call = true });
        if (end == 0)
            return self.invalidExpr("expected an expression after `..=`");

        return try self.pushNode(.{ Tag.range_to_inclusive, end });
    } else if (self.peek(&.{ .@".", .@"." })) {
        self.eatTokens(2);
        const end = try tryExpr(self, .{ .no_record_call = true });
        if (end == 0)
            return self.invalidExpr("expected an expression after `..`");

        return try self.pushNode(.{ Tag.range_to, end });
    }
    return 0;
}

fn tryClosure(self: *Parser) Err!u64 {
    try self.enter();
    defer self.exit();

    if (!self.peek(&.{.@"|"})) return 0;

    var params: u64 = 0;
    var return_type: u64 = 0;
    var clauses: u64 = 0;
    var block_or_statement: u64 = 0;

    const param_rules = .{
        common.rule("param", definitions.tryParam),
    };
    const params_nodes = try common.pMulti(self, param_rules, .@"|");
    defer params_nodes.deinit();
    params = try self.pushNode(.{ Tag.params, params_nodes.items });

    return_type = try definitions.tryReturnType(self);
    if (return_type == 0)
        return_type = try definitions.tryGradualReturnType(self);

    clauses = try definitions.tryClauses(self);

    if (self.peek(&.{.@"{"})) {
        const rules = .{
            common.rule("statement", statements.tryStatement),
        };
        block_or_statement = try statements.tryBlock(self, rules);
    } else block_or_statement = try statements.tryStatement(self);

    return try self.pushNode(.{
        Tag.closure,
        params,
        return_type,
        clauses,
        block_or_statement,
    });
}

fn tryList(self: *Parser) Err!u64 {
    try self.enter();
    defer self.exit();

    const rules = .{
        common.rule("expr", Parser.tryExpr),
    };

    const nodes = try common.pMulti(self, rules, .@"[");
    defer nodes.deinit();
    return try self.pushNode(.{ Tag.list, nodes.items });
}

// tuple -> ( expr, expr* )
// parenthesis -> ( epxr )
// unit -> ()
fn tryParenthesisOrTupleOrUnit(self: *Parser) Err!u64 {
    try self.enter();
    defer self.exit();

    if (!self.eat(.@"(")) return 0;

    const head = try self.tryExpr();
    if (head == 0) {
        if (!self.eat(.@")"))
            return self.invalidExpr("expected a `)` to close an unit, as you did not provide any expression");

        return try self.pushNode(.{Tag.unit});
    }
    if (!self.eat(.@",")) {
        if (!self.eat(.@")"))
            return self.invalidExpr("expected a `)` to close a parenthesis");
        return try self.pushNode(.{ Tag.parenthesis, head });
    }

    const rules = .{
        common.rule("expr", Parser.tryExpr),
    };

    var nodes = try common.pMulti(self, rules, null);
    defer nodes.deinit();
    try nodes.insert(0, head);
    return try self.pushNode(.{ Tag.tuple, nodes.items });
}

fn tryExprFromPattern(self: *Parser) Err!u64 {
    try self.enter();
    defer self.exit();

    if (!self.eat(.@"<")) return 0;
    const pattern = try self.tryPattern();
    if (pattern == 0)
        return self.invalidPattern("expected a pattern between `<` and `>`");
    try self.expectNextToken(.@">", "expected a `>` to close a pattern expression");
    return try self.pushNode(.{ Tag.expr_from_pattern, pattern });
}

fn tryRecord(self: *Parser) Err!u64 {
    try self.enter();
    defer self.exit();

    if (!self.peek(&.{.@"{"})) return 0;

    const rules = .{
        common.rule("property", common.tryProperty),
    };

    const nodes = try common.pMulti(self, rules, .@"{");
    defer nodes.deinit();
    return try self.pushNode(.{ Tag.record, nodes.items });
}

// effect_qualified -> #expr expr
fn tryEffectQualified(self: *Parser) Err!u64 {
    try self.enter();
    defer self.exit();

    if (!self.eat(.@"#")) return 0;

    const expr = try tryExpr(self, .{ .no_record_call = true });
    if (expr == 0)
        return self.invalidExpr("expected an effect or effect list after `#`");

    const ty = try tryExpr(self, .{ .no_record_call = true });
    if (ty == 0)
        return self.invalidExpr("expected an type after `#`");

    return try self.pushNode(.{ Tag.effect_qualified, expr, ty });
}

// error_qualified -> !expr expr
fn tryErrorQualified(self: *Parser) Err!u64 {
    try self.enter();
    defer self.exit();

    if (!self.eat(.@"!")) return 0;

    const expr = try tryExpr(self, .{ .no_record_call = true });
    if (expr == 0)
        return self.invalidExpr("expected an error or error list after `!`");

    const ty = try tryExpr(self, .{ .no_record_call = true });
    if (ty == 0)
        return self.invalidExpr("expected an type after `!`");

    return try self.pushNode(.{ Tag.error_qualified, expr, ty });
}

// optional -> ? expr
fn tryOptional(self: *Parser) Err!u64 {
    try self.enter();
    defer self.exit();

    if (!self.eat(.@"?")) return 0;

    const ty = try tryExpr(self, .{ .no_record_call = true });
    if (ty == 0)
        return self.invalidExpr("expected an type after `?`");

    return try self.pushNode(.{ Tag.optional, ty });
}

// dyn -> dyn expr
fn tryTraitObject(self: *Parser) Err!u64 {
    try self.enter();
    defer self.exit();

    if (!self.eat(.k_dyn)) return 0;

    const ty = try tryExpr(self, .{ .no_record_call = true });
    if (ty == 0)
        return self.invalidExpr("expected an type after `dyn`");

    return try self.pushNode(.{ Tag.trait_bound, ty });
}

// pointer_type -> *expr
fn tryPointerType(self: *Parser) Err!u64 {
    try self.enter();
    defer self.exit();

    if (!self.eat(.@"*")) return 0;

    const ty = try tryExpr(self, .{ .no_record_call = true });
    if (ty == 0)
        return self.invalidExpr("expected an type after `*`");

    return try self.pushNode(.{ Tag.pointer_type, ty });
}
