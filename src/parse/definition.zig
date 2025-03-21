const std = @import("std");
const w = @import("../common.zig");
const Err = w.Err;
const Parser = @import("parser.zig").Parser;
const ast = @import("ast.zig");
const Tag = ast.Tag;
const common = @import("common.zig");
const exprs = @import("expr.zig");
const patterns = @import("pattern.zig");
const statements = @import("statement.zig");

// param_optional -> .id: expr = expr
// param_typed -> id: expr
// param_trait_bound -> id: expr :- expr
// param_id -> id
// param_optional_id -> .id
// param_self -> self
// param_self_ref -> *self
// param_itself -> itself
// param_itself_ref -> *itself
// param_rest_bind -> ... id: expr
pub fn tryParam(self: *Parser) Err!u64 {
    try self.enter();
    defer self.exit();

    if (self.eat(.k_comptime)) {
        const param = try tryParam(self);
        if (param == 0)
            return self.invalidStatement("expected a parameter after 'comptime'");

        return try self.pushNode(.{ Tag.comptime_def, param });
    }

    if (self.peek(&.{ .@"*", .k_self })) {
        self.eatTokens(2);
        return try self.pushNode(.{Tag.param_self});
    }

    if (self.peek(&.{ .@"*", .k_itself })) {
        self.eatTokens(2);
        return try self.pushNode(.{Tag.param_itself});
    }

    if (self.eat(.k_self))
        return try self.pushNode(.{Tag.param_self});

    if (self.eat(.k_itself))
        return try self.pushNode(.{Tag.param_itself});

    if (self.peek(&.{ .@".", .@".", .@".", .id })) {
        self.eatTokens(3);
        const id = try common.tryId(self);
        if (id == 0) return self.unexpectedToken("expected identifier after '...'");

        if (!self.eat(.@":")) {
            return self.unexpectedToken("varargs must be specified with a type");
        }

        const type_ = try self.tryExpr();
        if (type_ == 0)
            return self.invalidStatement("expected a type after ':'");

        return try self.pushNode(.{ Tag.param_rest_bind, id, type_ });
    }

    if (self.peek(&.{ .@".", .id })) {
        self.eatTokens(1);
        const id = try common.tryId(self);
        if (id == 0) return self.unexpectedToken("expected identifier after '.' while parsing an optional parameter");
        if (!self.eat(.@":")) {
            return try self.pushNode(.{ Tag.param_optional_id, id });
        }
        const type_ = try self.tryExpr();
        if (type_ == 0)
            return self.invalidStatement("expected a type after ':'");

        return try self.pushNode(.{ Tag.param_optional, id, type_ });
    }

    if (self.peek(&.{.id})) {
        const id = try common.tryId(self);
        if (id == 0) return 0;

        if (self.eat(.@":-")) {
            const trait = try self.tryExpr();
            if (trait == 0)
                return self.invalidStatement("expected a trait after ':-'");

            return try self.pushNode(.{ Tag.param_trait_bound, id, trait });
        }

        if (!self.eat(.@":")) {
            return try self.pushNode(.{ Tag.param_id, id });
        }

        const type_ = try self.tryExpr();
        if (type_ == 0)
            return self.invalidStatement("expected a type after ':'");

        return try self.pushNode(.{ Tag.param_typed, id, type_ });
    }

    return 0;
}

// Try to parse a where clause
pub fn tryClause(self: *Parser) Err!u64 {
    try self.enter();
    defer self.exit();

    const token = self.peekToken();
    return switch (token.tag) {
        .id => try tryClauseDecl(self),
        .@"." => try tryClauseOptionalDecl(self),
        .k_requires => try tryRequiresClause(self),
        .k_ensures => try tryEnsuresClause(self),
        .k_decreases => try tryDecreasesClause(self),
        else => 0,
    };
}

// Parse a normal clause declaration: "id: expr"
fn tryClauseDecl(self: *Parser) Err!u64 {
    try self.enter();
    defer self.exit();

    const id = try common.tryId(self);
    if (id == 0) return 0;

    // Check for trait bound
    if (self.eat(.@":-")) {
        const trait = try exprs.tryExpr(self, .{ .no_record_call = true });
        if (trait == 0)
            return self.invalidStatement("expected a trait after ':-'");

        return try self.pushNode(.{ Tag.clause_trait_bound_decl, id, trait });
    }

    if (self.eat(.@":")) {
        // Regular declaration
        const expr = try exprs.tryExpr(self, .{ .no_record_call = true });
        if (expr == 0)
            return self.invalidStatement("expected an expression after ':'");

        return try self.pushNode(.{ Tag.clause_decl, id, expr });
    }

    return self.pushNode(.{ Tag.clause_type_decl, id });
}

// Parse an optional clause declaration: ".id: expr"
fn tryClauseOptionalDecl(self: *Parser) Err!u64 {
    try self.enter();
    defer self.exit();

    if (!self.eat(.@".")) return 0;

    const id = try common.tryId(self);
    if (id == 0)
        return self.invalidStatement("expected an identifier after '.'");

    if (!self.eat(.@":"))
        return try self.pushNode(.{ Tag.clause_type_decl_optional, id });

    const expr = try exprs.tryExpr(self, .{ .no_record_call = true });
    if (expr == 0)
        return self.invalidStatement("expected an expression after ':'");

    return try self.pushNode(.{ Tag.clause_decl_optional, id, expr });
}

// Parse a requires clause: "requires expr"
fn tryRequiresClause(self: *Parser) Err!u64 {
    try self.enter();
    defer self.exit();

    if (!self.eat(.k_requires)) return 0;

    const expr = try exprs.tryExpr(self, .{ .no_record_call = true });
    if (expr == 0)
        return self.invalidStatement("expected an expression after 'requires'");

    return try self.pushNode(.{ Tag.requires_predicate, expr });
}

// Parse an ensures clause: "ensures expr"
fn tryEnsuresClause(self: *Parser) Err!u64 {
    try self.enter();
    defer self.exit();

    if (!self.eat(.k_ensures)) return 0;

    const expr = try exprs.tryExpr(self, .{ .no_record_call = true });
    if (expr == 0)
        return self.invalidStatement("expected an expression after 'ensures'");

    return try self.pushNode(.{ Tag.ensures_predicate, expr });
}

// Parse a decreases clause: "decreases expr"
fn tryDecreasesClause(self: *Parser) Err!u64 {
    try self.enter();
    defer self.exit();

    if (!self.eat(.k_decreases)) return 0;

    const expr = try exprs.tryExpr(self, .{ .no_record_call = true });
    if (expr == 0)
        return self.invalidStatement("expected an expression after 'decreases'");

    return try self.pushNode(.{ Tag.decreases, expr });
}

// Parse clauses: "where clause*"
pub fn tryClauses(self: *Parser) Err!u64 {
    try self.enter();
    defer self.exit();

    if (!self.eat(.k_where)) return 0;

    const rules = .{
        common.rule("clause", tryClause),
    };

    const nodes = try common.pMulti(self, rules, null);
    defer nodes.deinit();

    return try self.pushNode(.{ Tag.clauses, nodes.items });
}

// Parse return type: "-> expr"
pub fn tryReturnType(self: *Parser) Err!u64 {
    try self.enter();
    defer self.exit();

    if (!self.eat(.@"->")) return 0;

    const expr = try exprs.tryExpr(self, .{ .no_record_call = true });
    if (expr == 0)
        return self.invalidStatement("expected a type after '->'");

    return try self.pushNode(.{ Tag.return_type, expr });
}

// Parse gradual return type: "~> expr"
pub fn tryGradualReturnType(self: *Parser) Err!u64 {
    try self.enter();
    defer self.exit();

    if (!self.eat(.@"~>")) return 0;

    const expr = try exprs.tryExpr(self, .{ .no_record_call = true });
    if (expr == 0)
        return self.invalidStatement("expected a type after '~>'");

    return try self.pushNode(.{ Tag.gradual_return_type, expr });
}

// fn_def -> fn id? ( param* ) ( return_type | gradual_return_type )? clauses? block
pub fn tryFnDef(self: *Parser) Err!u64 {
    try self.enter();
    defer self.exit();

    if (!self.eat(.k_fn)) return 0;

    // Optional function name
    const name = try common.tryId(self);
    var params: u64 = 0;
    var return_type: u64 = 0;

    var kind: w.Token.Tag = .@"(";

    if (self.peek(&.{.@"<"}))
        kind = .@"<"
    else if (self.peek(&.{.@"("}))
        kind = .@"("
    else
        return self.invalidDefinition("expected '(' or '<' after function name");

    const param_rules = .{
        common.rule("param", tryParam),
    };

    if (kind == .@"(") {
        const nodes = try common.pMulti(self, param_rules, .@"(");
        defer nodes.deinit();
        params = try self.pushNode(.{ Tag.params, nodes.items });
    } else if (kind == .@"<") {
        const nodes = try common.pMulti(self, param_rules, .@"<");
        defer nodes.deinit();
        params = try self.pushNode(.{ Tag.params, nodes.items });
    }

    // Return type
    return_type = try tryReturnType(self);
    if (return_type == 0) {
        return_type = try tryGradualReturnType(self);
    }

    // Where clauses
    const clauses = try tryClauses(self);

    // Function body
    const stmt_rules = .{
        common.ruleWithDelimiter("statements", statements.tryStatement, .@";"),
    };
    const body = try statements.tryBlock(self, stmt_rules);
    if (body == 0)
        return self.invalidStatement("expected a function body");

    return try self.pushNode(.{ Tag.fn_def, name, params, return_type, clauses, body });
}

// struct_field -> id : expr (= expr)?
pub fn tryStructField(self: *Parser) Err!u64 {
    try self.enter();
    defer self.exit();

    const id = try common.tryId(self);
    if (id == 0) return 0;

    if (!self.eat(.@":"))
        return self.invalidStatement("expected ':' after field name");

    const type_ = try self.tryExpr();
    if (type_ == 0)
        return self.invalidStatement("expected a type after ':'");

    // Default value
    var default: u64 = 0;
    if (self.eat(.@"="))
        default = try self.tryExpr();

    return try self.pushNode(.{ Tag.struct_field, id, type_, default });
}

// struct_def -> struct id? ( param* ) clauses? block(struct_field*)
pub fn tryStructDef(self: *Parser) Err!u64 {
    try self.enter();
    defer self.exit();

    if (!self.eat(.k_struct)) return 0;

    // Optional struct name
    const name = try common.tryId(self);

    // Where clauses
    const clauses = try tryClauses(self);

    // Struct body - fields
    const field_rules = .{
        common.rule("field", tryStructField),
        common.ruleWithDelimiter("definitions", tryDefinition, .@";"),
        common.ruleWithDelimiter("statements", statements.tryStatement, .@";"),
    };
    const fields = try common.pMulti(self, field_rules, .@"{");
    defer fields.deinit();
    const struct_def_body = try self.pushNode(.{ Tag.struct_def_body, fields.items });

    return try self.pushNode(.{ Tag.struct_def, name, clauses, struct_def_body });
}

// enum_variant_with_struct -> id block(struct_field)
pub fn tryEnumVariantWithStruct(self: *Parser) Err!u64 {
    try self.enter();
    defer self.exit();

    if (!self.peek(&.{ .id, .@"{" })) return 0;

    const id = try common.tryId(self);
    const field_rules = .{
        common.ruleWithDelimiter("field", tryStructField, .@";"),
    };

    const fields = try common.pMulti(self, field_rules, .@"{");
    defer fields.deinit();

    return try self.pushNode(.{ Tag.enum_variant_with_struct, id, fields.items });
}

// enum_variant_with_tuple -> id tuple
pub fn tryEnumVariantWithTuple(self: *Parser) Err!u64 {
    try self.enter();
    defer self.exit();

    if (!self.peek(&.{ .id, .@"(" })) return 0;
    const id = try common.tryId(self);

    const tuple_rules = .{
        common.rule("type", Parser.tryExpr),
    };

    const types = try common.pMulti(self, tuple_rules, .@"(");
    defer types.deinit();

    return try self.pushNode(.{ Tag.enum_variant_with_tuple, id, types.items });
}

// enum_variant_def_with_pattern -> id = pattern
pub fn tryEnumVariantWithPattern(self: *Parser) Err!u64 {
    try self.enter();
    defer self.exit();

    if (!self.peek(&.{ .id, .@"=" })) return 0;
    const id = try common.tryId(self);
    self.eatTokens(1);

    const pattern = try self.tryPattern();
    if (pattern == 0)
        return self.invalidStatement("expected a pattern after '='");

    return try self.pushNode(.{ Tag.enum_variant_def_with_pattern, id, pattern });
}

// enum_variant_with_sub_enum -> id. { enum_variant* }
pub fn tryEnumVariantWithSubEnum(self: *Parser) Err!u64 {
    try self.enter();
    defer self.exit();

    if (!self.peek(&.{ .id, .@"." })) return 0;
    const id = try common.tryId(self);

    self.eatTokens(1);

    const rules = .{
        common.rule("variant", tryEnumVariant),
    };

    const variants = try common.pMulti(self, rules, .@"{");
    defer variants.deinit();

    return try self.pushNode(.{ Tag.enum_variant_with_sub_enum, id, variants.items });
}

// enum_variant -> id | enum_variant_with_struct | enum_variant_with_tuple | enum_variant_with_sub_enum | enum_variant_def_with_pattern
pub fn tryEnumVariant(self: *Parser) Err!u64 {
    try self.enter();
    defer self.exit();

    // Try complex variants first
    const struct_variant = try tryEnumVariantWithStruct(self);
    if (struct_variant != 0) return struct_variant;

    const tuple_variant = try tryEnumVariantWithTuple(self);
    if (tuple_variant != 0) return tuple_variant;

    const pattern_variant = try tryEnumVariantWithPattern(self);
    if (pattern_variant != 0) return pattern_variant;

    const sub_enum_variant = try tryEnumVariantWithSubEnum(self);
    if (sub_enum_variant != 0) return sub_enum_variant;

    // Simple id variant
    const id = try common.tryId(self);
    if (id == 0) return 0;

    return id;
}

// enum_def -> enum id? ( param* ) clauses? block(enum_variant*)
pub fn tryEnumDef(self: *Parser) Err!u64 {
    try self.enter();
    defer self.exit();

    if (!self.eat(.k_enum)) return 0;

    // Optional enum name
    const name = try common.tryId(self);

    // Where clauses
    const clauses = try tryClauses(self);

    // Enum body - variants
    const variant_rules = .{
        common.rule("variant", tryEnumVariant),
        common.ruleWithDelimiter("definitions", tryDefinition, .@";"),
        common.ruleWithDelimiter("statements", statements.tryStatement, .@";"),
    };
    const variants = try common.pMulti(self, variant_rules, .@"{");
    defer variants.deinit();
    const enum_def_body = try self.pushNode(.{ Tag.enum_def_body, variants.items });

    return try self.pushNode(.{ Tag.enum_def, name, clauses, enum_def_body });
}

// union_variant -> id : expr
pub fn tryUnionVariant(self: *Parser) Err!u64 {
    try self.enter();
    defer self.exit();

    const id = try common.tryId(self);
    if (id == 0) return 0;

    if (!self.eat(.@":")) return 0;

    const type_ = try self.tryExpr();
    if (type_ == 0)
        return self.invalidStatement("expected a type after ':'");

    return try self.pushNode(.{ Tag.union_variant, id, type_ });
}

// union_def -> union id? clauses? block(union_variant*)
pub fn tryUnionDef(self: *Parser) Err!u64 {
    try self.enter();
    defer self.exit();

    if (!self.eat(.k_union)) return 0;

    // Optional union name
    const name = try common.tryId(self);

    // Where clauses
    const clauses = try tryClauses(self);

    // Union body - variants
    const variant_rules = .{
        common.rule("variant", tryUnionVariant),
        common.ruleWithDelimiter("definitions", tryDefinition, .@";"),
        common.ruleWithDelimiter("statements", statements.tryStatement, .@";"),
    };
    const variants = try common.pMulti(self, variant_rules, .@"{");
    defer variants.deinit();
    const union_def_body = try self.pushNode(.{ Tag.union_def_body, variants.items });

    return try self.pushNode(.{ Tag.union_def, name, clauses, union_def_body });
}

// impl_def -> impl expr (for expr)? clauses? block
pub fn tryImplDef(self: *Parser) Err!u64 {
    try self.enter();
    defer self.exit();

    if (!self.eat(.k_impl)) return 0;

    const expr = try exprs.tryExpr(self, .{ .no_record_call = true });
    if (expr == 0)
        return self.invalidStatement("expected an expression after 'impl'");

    var for_expr: u64 = 0;
    if (self.eat(.k_for)) {
        for_expr = try exprs.tryExpr(self, .{ .no_record_call = true });
        if (for_expr == 0)
            return self.invalidStatement("expected an expression after 'for'");
    }

    // Where clauses
    const clauses = try tryClauses(self);

    const stmt_rules = .{
        common.ruleWithDelimiter("definitions", tryDefinition, .@";"),
        common.ruleWithDelimiter("statements", statements.tryStatement, .@";"),
    };
    const body = try statements.tryBlock(self, stmt_rules);
    if (body == 0)
        return self.invalidStatement("expected a implementation scope body");

    return try self.pushNode(.{ Tag.impl_def, expr, for_expr, clauses, body });
}
// derive -> derive expr* for expr clauses?
pub fn tryDerive(self: *Parser) Err!u64 {
    try self.enter();
    defer self.exit();

    if (!self.eat(.k_derive)) return 0;

    const exprs_ = try common.pMulti(self, .{
        common.rule("expr", Parser.tryExpr),
    }, null);
    defer exprs_.deinit();

    var for_expr: u64 = 0;
    if (self.eat(.k_for)) {
        for_expr = try exprs.tryExpr(self, .{ .no_record_call = true });
        if (for_expr == 0)
            return self.invalidStatement("expected an expression after 'for'");
    }

    // Where clauses
    const clauses = try tryClauses(self);

    return try self.pushNode(.{ Tag.derive, clauses, for_expr, exprs_.items });
}

// extend -> extend expr (for expr)? clauses? block
pub fn tryExtendDef(self: *Parser) Err!u64 {
    try self.enter();
    defer self.exit();

    if (!self.eat(.k_extend)) return 0;

    const expr = try exprs.tryExpr(self, .{ .no_record_call = true });
    if (expr == 0)
        return self.invalidStatement("expected an expression after 'extend'");

    var for_expr: u64 = 0;
    if (self.eat(.k_for)) {
        for_expr = try exprs.tryExpr(self, .{ .no_record_call = true });
        if (for_expr == 0)
            return self.invalidStatement("expected an expression after 'for'");
    }

    // Where clauses
    const clauses = try tryClauses(self);

    const stmt_rules = .{
        common.ruleWithDelimiter("definitions", tryDefinition, .@";"),
        common.ruleWithDelimiter("statements", statements.tryStatement, .@";"),
    };
    const body = try statements.tryBlock(self, stmt_rules);
    if (body == 0)
        return self.invalidStatement("expected a extension scope body");

    return try self.pushNode(.{ Tag.extend, expr, for_expr, clauses, body });
}

fn tryModuleDef(self: *Parser) Err!u64 {
    try self.enter();
    defer self.exit();

    if (!self.eat(.k_mod)) return 0;

    const id = try common.tryId(self);
    if (id == 0)
        return self.invalidStatement("expected a module name after 'mod'");

    const rules = .{
        common.ruleWithDelimiter("definition", tryDefinition, .@";"),
        common.ruleWithDelimiter("statements", statements.tryStatement, .@";"),
    };

    const block = try statements.tryBlock(self, rules);

    if (block == 0)
        return try self.pushNode(.{ Tag.mod_file, id });

    return try self.pushNode(.{ Tag.mod, id, block });
}

// Helper function to try parsing a definition
pub fn tryDefinition(self: *Parser) Err!u64 {
    try self.enter();
    defer self.exit();

    const token = self.peekToken();
    return switch (token.tag) {
        .k_fn => try tryFnDef(self),
        .k_struct => try tryStructDef(self),
        .k_enum => try tryEnumDef(self),
        .k_union => try tryUnionDef(self),
        .k_derive => try tryDerive(self),
        .k_impl => try tryImplDef(self),
        .k_extend => try tryExtendDef(self),
        .k_mod => try tryModuleDef(self),

        .k_inline => try common.tryInline(self, common.rule("definition", tryDefinition)),
        .k_pub => try common.tryPub(self, common.rule("definition", tryDefinition)),
        .k_pure => try common.tryPure(self, common.rule("definition", tryDefinition)),
        .k_comptime => try common.tryComptime(self, common.rule("definition", tryDefinition)),
        else => 0,
    };
}
