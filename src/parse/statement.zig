const std = @import("std");
const w = @import("../common.zig");
const Err = w.Err;
const Parser = @import("parser.zig").Parser;
const ast = @import("ast.zig");
const Tag = ast.Tag;
const common = @import("common.zig");
const exprs = @import("expr.zig");
const patterns = @import("pattern.zig");
const definitions = @import("definition.zig");

pub fn tryStatement(self: *Parser) Err!u64 {
    try self.enter();
    defer self.exit();

    const token = self.peekToken();
    return switch (token.tag) {
        .k_use => try tryUseStatement(self),
        .k_break => try tryBreakStatement(self),
        .k_continue => try tryContinueStatement(self),
        .k_return => try tryReturnStatement(self),
        .k_let => try tryLetDecl(self),
        .k_const => try tryConstDecl(self),
        .k_for => try tryForLoop(self),
        .k_while => try tryWhileLoop(self),
        .k_if => try tryIfStatement(self),
        .k_when => try tryWhenStatement(self),
        .k_asserts => try tryAStatement(self, .k_asserts),
        .k_assumes => try tryAStatement(self, .k_assumes),
        .k_invariant => try tryAStatement(self, .k_invariant),
        .k_decreases => try tryAStatement(self, .k_decreases),
        .k_axiom => try tryAStatement(self, .k_axiom),
        .k_inline => try common.tryInline(self, common.rule("statement", tryStatement)),
        .k_pub => try common.tryPub(self, common.rule("statements", tryStatement)),
        else => try tryExprStatements(self),
    };
}

pub fn tryFileScope(self: *Parser) Err!u64 {
    try self.enter();
    defer self.exit();

    const rules = .{
        common.ruleWithDelimiter("definitions", definitions.tryDefinition, .@";"),
        common.ruleWithDelimiter("statements", tryStatement, .@";"),
    };
    const nodes = try common.pMulti(self, rules, .eof);
    defer nodes.deinit();

    return try self.pushNode(.{ Tag.file_scope, nodes.items });
}

// if_guard -> if expr
// label -> id
// -- 控制流操作
// break -> break label? if_guard?
// break id expr
pub fn tryBreakStatement(self: *Parser) Err!u64 {
    try self.enter();
    defer self.exit();

    if (!self.eat(.k_break))
        return 0;

    var id: u64 = 0;
    var expr: u64 = 0;
    if (self.eat(.@":")) id = try common.tryId(self);
    if (self.eat(.k_if)) expr = try self.tryExpr();

    return try self.pushNode(.{
        Tag.break_statement,
        id,
        expr,
    });
}

// continue -> continue label? if_guard?
pub fn tryContinueStatement(self: *Parser) Err!u64 {
    try self.enter();
    defer self.exit();

    if (!self.eat(.k_continue))
        return 0;

    var id: u64 = 0;
    var expr: u64 = 0;
    if (self.eat(.@":")) id = try common.tryId(self);
    if (self.eat(.k_if)) expr = try self.tryExpr();

    return try self.pushNode(.{
        Tag.continue_statement,
        id,
        expr,
    });
}
// return -> return if_guard?
pub fn tryReturnStatement(self: *Parser) Err!u64 {
    try self.enter();
    defer self.exit();

    if (!self.eat(.k_return))
        return 0;

    var expr: u64 = 0;
    var guard: u64 = 0;
    expr = try self.tryExpr();
    if (self.eat(.k_if)) guard = try self.tryExpr();

    return try self.pushNode(.{ Tag.return_statement, expr, guard });
}

// let_decl -> let pattern (: expr)? = expr
fn tryLetDecl(self: *Parser) Err!u64 {
    try self.enter();
    defer self.exit();

    if (!self.eat(.k_let))
        return 0;

    var pattern: u64 = 0;
    var expr: u64 = 0;
    var type_: u64 = 0;

    pattern = try self.tryPattern();
    if (pattern == 0)
        return self.invalidPattern("expected a pattern");

    if (self.eat(.@":")) {
        type_ = try self.tryExpr();
        if (type_ == 0)
            return self.invalidStatement("missing type");
    }

    if (!self.eat(.@"="))
        return self.invalidStatement("missing `=` after pattern");

    expr = try self.tryExpr();
    if (expr == 0)
        return self.invalidExpr("expected an initializer expression");

    return try self.pushNode(.{ Tag.let_decl, pattern, type_, expr });
}

// const_decl -> const pattern (: expr)? = expr
fn tryConstDecl(self: *Parser) Err!u64 {
    try self.enter();
    defer self.exit();

    if (!self.eat(.k_const))
        return 0;

    var pattern: u64 = 0;
    var expr: u64 = 0;
    var type_: u64 = 0;

    pattern = try self.tryPattern();
    if (pattern == 0)
        return self.invalidPattern("expected a pattern");

    if (self.eat(.@":")) {
        type_ = try self.tryExpr();
        if (type_ == 0)
            return self.invalidStatement("missing type");
    }

    if (!self.eat(.@"="))
        return self.invalidStatement("missing `=` after pattern");

    expr = try self.tryExpr();
    if (expr == 0)
        return self.invalidExpr("expected an initializer expression");

    return try self.pushNode(.{ Tag.const_decl, pattern, type_, expr });
}

// statements that starts with an expression
// expr_statement -> expr
// assign -> expr = expr
// assign_add -> expr += expr
// assign_mul -> expr *= expr
// assign_sub -> expr -= expr
// assign_div -> expr /= expr
// assign_mod -> expr %= expr
pub fn tryExprStatements(self: *Parser) Err!u64 {
    try self.enter();
    defer self.exit();
    const expr = try self.tryExpr();
    if (expr == 0)
        return 0;

    switch (self.peekToken().tag) {
        .@"=" => {
            self.eatTokens(1);
            const right = try self.tryExpr();
            if (right == 0)
                return self.invalidStatement("missing right operand");
            return try self.pushNode(.{ Tag.assign, expr, right });
        },
        .@"+=" => {
            self.eatTokens(1);
            const right = try self.tryExpr();
            if (right == 0)
                return self.invalidStatement("missing right operand");
            return try self.pushNode(.{ Tag.assign_add, expr, right });
        },
        .@"-=" => {
            self.eatTokens(1);
            const right = try self.tryExpr();
            if (right == 0)
                return self.invalidStatement("missing right operand");
            return try self.pushNode(.{ Tag.assign_sub, expr, right });
        },
        .@"*=" => {
            self.eatTokens(1);
            const right = try self.tryExpr();
            if (right == 0)
                return self.invalidStatement("missing right operand");
            return try self.pushNode(.{ Tag.assign_mul, expr, right });
        },
        .@"/=" => {
            self.eatTokens(1);
            const right = try self.tryExpr();
            if (right == 0)
                return self.invalidStatement("missing right operand");
            return try self.pushNode(.{ Tag.assign_div, expr, right });
        },
        .@"%=" => {
            self.eatTokens(1);
            const right = try self.tryExpr();
            if (right == 0)
                return self.invalidStatement("missing right operand");
            return try self.pushNode(.{ Tag.assign_mod, expr, right });
        },
        else => {
            return try self.pushNode(.{ Tag.expr_statement, expr });
        },
    }
}

pub fn tryInvariantStatement(self: *Parser) Err!u64 {
    return try tryAStatement(self, .k_invariant);
}

pub fn tryDecreasesStatement(self: *Parser) Err!u64 {
    return try tryAStatement(self, .k_decreases);
}

// -- 基本循环语句
// for_loop -> for label? pattern in expr invariants? block
pub fn tryForLoop(self: *Parser) Err!u64 {
    try self.enter();
    defer self.exit();

    if (!self.eat(.k_for))
        return 0;

    var label: u64 = 0;
    var pattern: u64 = 0;
    var expr: u64 = 0;
    var invariants: u64 = 0;
    var block: u64 = 0;

    if (self.eat(.@":")) label = try common.tryId(self);
    pattern = try self.tryPattern();
    if (pattern == 0)
        return self.invalidPattern("expected a pattern");

    if (!self.eat(.k_in))
        return self.invalidStatement("expected `in` keyword after pattern");

    expr = try exprs.tryExpr(self, .{ .no_record_call = true });
    if (expr == 0)
        return self.invalidExpr("expected a condition expression for matching");

    if (self.eat(.k_where)) {
        const rules = .{
            common.rule("invariants", tryInvariantStatement),
            common.rule("decreases", tryDecreasesStatement),
        };

        const nodes = try common.pMulti(self, rules, null);
        defer nodes.deinit();
        invariants = try self.pushNode(.{ Tag.invariants, nodes.items });
    }

    const rules = .{
        common.ruleWithDelimiter("statements", tryStatement, .@";"),
    };

    const nodes = try common.pMulti(self, rules, .@"{");
    defer nodes.deinit();
    block = try self.pushNode(.{ Tag.block, nodes.items });

    return try self.pushNode(.{
        Tag.for_loop,
        label,
        pattern,
        expr,
        invariants,
        block,
    });
}
// while_is_match -> while label? expr is invariants? { branch* }
// while_is_pattern -> while label? expr is pattern invariants? block
// while_loop -> while label? expr invariants? block
pub fn tryWhileLoop(self: *Parser) Err!u64 {
    try self.enter();
    defer self.exit();

    if (!self.eat(.k_while))
        return 0;

    var label: u64 = 0;
    var expr: u64 = 0;
    var pattern: u64 = 0;
    var invariants: u64 = 0;
    var block: u64 = 0;

    if (self.eat(.@":")) label = try common.tryId(self);
    expr = try exprs.tryExpr(self, .{ .no_record_call = true });
    if (expr == 0)
        return self.invalidExpr("expected a condition expression for while");

    if (self.eat(.k_is)) {
        pattern = try patterns.tryPattern(self, .{ .no_record_call = true });
        if (pattern != 0) {
            invariants = try tryInvariants(self);
            const rules = .{
                common.ruleWithDelimiter("statements", tryStatement, .@";"),
            };
            block = try tryBlock(self, rules);
            return try self.pushNode(.{ Tag.while_is_pattern, label, expr, pattern, invariants, block });
        } else {
            invariants = try tryInvariants(self);
            const rules = .{
                common.rule("pattern branches", tryBranch),
            };
            const nodes = try common.pMulti(self, rules, .@"{");
            defer nodes.deinit();
            return try self.pushNode(.{ Tag.while_is_match, label, expr, invariants, nodes.items });
        }
    }

    invariants = try tryInvariants(self);

    const rules = .{
        common.ruleWithDelimiter("statements", tryStatement, .@";"),
    };
    block = try tryBlock(self, rules);

    return try self.pushNode(.{ Tag.while_loop, label, expr, invariants, block });
}

fn tryInvariants(self: *Parser) Err!u64 {
    try self.enter();
    defer self.exit();

    if (!self.eat(.k_where))
        return 0;

    const rules = .{
        common.rule("invariants", tryInvariantStatement),
        common.rule("decreases", tryDecreasesStatement),
    };

    const nodes = try common.pMulti(self, rules, null);
    defer nodes.deinit();

    return try self.pushNode(.{ Tag.invariants, nodes.items });
}

pub fn tryBlock(self: *Parser, comptime rules: anytype) Err!u64 {
    try self.enter();
    defer self.exit();

    if (!self.peek(&.{.@"{"}))
        return 0;

    const nodes = try common.pMulti(self, rules, .@"{");
    defer nodes.deinit();

    return try self.pushNode(.{ Tag.block, nodes.items });
}

// condition_branch -> expr => stmt | block
fn tryConditionBranch(self: *Parser) Err!u64 {
    try self.enter();
    defer self.exit();

    const expr = try self.tryExpr();
    var stmt_or_block: u64 = 0;
    if (expr == 0)
        return 0;

    if (!self.eat(.@"=>"))
        return self.invalidStatement("expected `=>` after condition");

    stmt_or_block = try self.tryStatement();
    if (stmt_or_block != 0)
        return try self.pushNode(.{ Tag.condition_branch, expr, stmt_or_block });

    const rules = .{
        common.ruleWithDelimiter("statements", tryStatement, .@";"),
    };
    stmt_or_block = try tryBlock(self, rules);

    return try self.pushNode(.{ Tag.condition_branch, expr, stmt_or_block });
}
// when -> when { condition_branch* }
pub fn tryWhenStatement(self: *Parser) Err!u64 {
    try self.enter();
    defer self.exit();

    if (!self.eat(.k_when))
        return 0;

    const rules = .{
        common.rule("condition_branch", tryConditionBranch),
    };

    const nodes = try common.pMulti(self, rules, .@"{");
    defer nodes.deinit();

    return try self.pushNode(.{ Tag.when_statement, nodes.items });
}

// if_is_match -> if expr is { branch* }
// if_is_pattern -> if expr is pattern block (else (if | block))?
// if -> if expr block else?
pub fn tryIfStatement(self: *Parser) Err!u64 {
    try self.enter();
    defer self.exit();

    if (!self.eat(.k_if))
        return 0;

    var expr: u64 = 0;
    var pattern: u64 = 0;
    var block: u64 = 0;
    var else_: u64 = 0;

    expr = try exprs.tryExpr(self, .{ .no_record_call = true });
    if (expr == 0)
        return self.invalidStatement("missing condition expression");

    if (self.peek(&.{ .k_is, .@"{" })) {
        self.eatTokens(1);

        const rules = .{
            common.rule("pattern branches", tryBranch),
        };

        const nodes = try common.pMulti(self, rules, .@"{");
        defer nodes.deinit();
        return try self.pushNode(.{ Tag.if_is_match, expr, nodes.items });
    } else if (self.eat(.k_is)) {
        pattern = try patterns.tryPattern(self, .{ .no_record_call = true });
        if (pattern == 0)
            return self.invalidStatement("missing pattern after `is`");

        const rules = .{
            common.ruleWithDelimiter("statements", tryStatement, .@";"),
        };
        block = try tryBlock(self, rules);
    } else {
        const rules = .{
            common.ruleWithDelimiter("statements", tryStatement, .@";"),
        };
        block = try tryBlock(self, rules);
    }

    // 接下来这部分相对丑陋:(，有机会再重写一下
    if (!self.eat(.k_else))
        return if (pattern == 0)
            try self.pushNode(.{ Tag.if_statement, expr, block, else_ })
        else
            try self.pushNode(.{ Tag.if_is_pattern, expr, pattern, block, else_ });

    else_ = try tryIfStatement(self);
    if (else_ != 0)
        return if (pattern == 0)
            try self.pushNode(.{ Tag.if_statement, expr, block, else_ })
        else
            try self.pushNode(.{ Tag.if_is_pattern, expr, pattern, block, else_ });

    const rules = .{
        common.ruleWithDelimiter("statements", tryStatement, .@";"),
    };
    else_ = try tryBlock(self, rules);
    return if (pattern == 0)
        try self.pushNode(.{ Tag.if_statement, expr, block, else_ })
    else
        try self.pushNode(.{ Tag.if_is_pattern, expr, pattern, block, else_ });
}
// else -> ( else block ) | ( else if)

// branch -> pattern => statement | block,
pub fn tryBranch(self: *Parser) Err!u64 {
    try self.enter();
    defer self.exit();

    const pattern = try self.tryPattern();
    if (pattern == 0)
        return 0;

    if (!self.eat(.@"=>"))
        return self.invalidStatement("expected `=>` after pattern");

    var stmt_or_block = try self.tryStatement();
    if (stmt_or_block != 0)
        return try self.pushNode(.{ Tag.branch, pattern, stmt_or_block });

    const rules = .{
        common.ruleWithDelimiter("statements", tryStatement, .@";"),
    };
    stmt_or_block = try tryBlock(self, rules);

    return try self.pushNode(.{ Tag.branch, pattern, stmt_or_block });
}

// catch_branch -> catch e => statement | block,
pub fn tryCatchBranch(self: *Parser) Err!u64 {
    try self.enter();
    defer self.exit();

    if (!self.eat(.k_catch))
        return 0;

    const error_name: u64 = try common.tryId(self);
    var stmt_or_block: u64 = 0;

    if (!self.eat(.@"=>"))
        return self.invalidStatement("expected `=>` after catch");
    stmt_or_block = try self.tryStatement();
    if (stmt_or_block != 0)
        return try self.pushNode(.{ Tag.catch_branch, error_name, stmt_or_block });

    const rules = .{
        common.ruleWithDelimiter("statements", tryStatement, .@";"),
    };
    stmt_or_block = try tryBlock(self, rules);
    return try self.pushNode(.{ Tag.catch_branch, error_name, stmt_or_block });
}

// use_statement -> use mod_path
// mod_path ->
//     id
//     | path_select
//     | path_select_all
//     | path_select_multi
//     | super_path
//     | package_path
//     | path_as_bind
// path_select -> mod_path . id
// path_select_multi -> mod_path . { mod_path*}
// path_select_all -> mod_path . *
// super_path -> (.)+ mod_path
// package_path -> @ mod_path
// path_as_bind -> mod_path as id
fn tryUseStatement(self: *Parser) Err!u64 {
    try self.enter();
    defer self.exit();

    if (!self.eat(.k_use))
        return 0;

    const path = try tryPath(self);
    if (path == 0) {
        return self.invalidPath("missing mod_path");
    }

    return try self.pushNode(.{ Tag.use_statement, path });
}

fn tryPath(self: *Parser) Err!u64 {
    try self.enter();
    defer self.exit();

    return try tryPratt(self, 0);
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
        .@"." = .{ .prec = 10, .tag = .pattern_select },
        .k_as = .{ .prec = 20, .tag = .pattern_as_bind },
    },
);

fn tryPratt(self: *Parser, min_prec: i8) Err!u64 {
    var left = try common.tryId(self);
    if (left == 0) {
        return 0;
    }

    while (true) {
        const token = self.peekToken();
        const op_info = op_table[@intFromEnum(token.tag)];

        if (op_info.tag == .invalid or op_info.prec < min_prec)
            break;

        const post = try tryPostfixPath(self, token.tag, left);
        if (post != 0) {
            left = post;
            continue;
        }

        self.eatTokens(1);
        const right = try tryPratt(self, op_info.prec + 1);
        if (right == 0)
            return self.invalidPath("missing right operand");

        left = try self.pushNode(.{ op_info.tag, left, right });
    }

    return left;
}

pub fn tryPostfixPath(
    self: *Parser,
    tag: w.Token.Tag,
    left: u64,
) Err!u64 {
    var result: u64 = 0;
    switch (tag) {
        .@"." => {
            // select all
            if (self.peek(&.{ .@".", .@"*" })) {
                self.eatTokens(2);
                result = try self.pushNode(.{ Tag.path_select_all, left });
            } else if (self.peek(&.{ .@".", .@"{" })) {
                self.eatTokens(1);
                const rules = .{
                    common.rule("mod path", tryPath),
                };
                const nodes = try common.pMulti(self, rules, .@"{");
                defer nodes.deinit();
                result = try self.pushNode(.{ Tag.path_select_multi, left, nodes.items });
            } else if (self.peek(&.{ .@".", .id })) {
                self.eatTokens(1);
                const id = try common.tryId(self);
                if (id == 0)
                    return 0;

                result = try self.pushNode(.{ Tag.path_select, left, id });
            } else {
                return 0;
            }
        },
        .k_as => {
            if (!self.peek(&.{ .k_as, .id })) {
                return 0;
            }

            self.eatTokens(1);
            const id = try common.tryId(self);

            result = try self.pushNode(.{ Tag.path_as_bind, left, id });
        },
        else => {},
    }
    return result;
}

// -- 这几个用于形式化验证
// asserts -> asserts expr
// assumes -> assumes expr
// invariant -> invariant expr
// decreases -> decreases expr
// -- 数据结构specification
// axiom -> axiom expr
pub fn tryAStatement(self: *Parser, tag: w.Token.Tag) Err!u64 {
    try self.enter();
    defer self.exit();

    if (!self.eat(tag))
        return 0;
    const result_tag = switch (tag) {
        .k_asserts => Tag.asserts,
        .k_assumes => Tag.assumes,
        .k_invariant => Tag.invariant,
        .k_decreases => Tag.decreases,
        .k_axiom => Tag.axiom,
        .k_requires => Tag.requires_predicate,
        .k_ensures => Tag.ensures_predicate,
        else => unreachable,
    };

    const expr = try exprs.tryExpr(self, .{ .no_record_call = true });
    return try self.pushNode(.{ result_tag, expr });
}

pub fn tryABlock(self: *Parser, tag: w.Token.Tag) Err!u64 {
    try self.enter();
    defer self.exit();

    if (!self.eat(tag))
        return 0;

    const rules = .{
        common.ruleWithDelimiter("statements", tryStatement, .@";"),
    };
    const result_tag = switch (tag) {
        .k_do => Tag.do_block,
        .k_comptime => Tag.comptime_block,
        .k_unsafe => Tag.unsafe_block,
        .k_atomic => Tag.atomic_block,
        .k_async => Tag.async_block,
        else => unreachable,
    };

    return try self.pushNode(.{ result_tag, try tryBlock(self, rules) });
}
