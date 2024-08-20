gpa: Alc,
arena: Alc,

src: []const u8,

map: std.StringArrayHashMap(Index),
str_bytes: std.ArrayList(u8),

root: Index = 0,
nodes: Soa(Node),
nodes_capacity: usize,
prisma: std.ArrayList(Index),

tokens: *const Soa(Token),
cursor: usize = 0,

err: void = undefined,

// pub fn parse(src: []const u8, ts: *const Soa(Token)) void {}

pub fn pNamespace(p: *Self) Index {
    const t = p.nextToken();

    switch (t) {
        .k_fn => {
            return p.pFn();
        },
        else => {},
    }

    return 0;
}

pub fn pFn(p: *Self) Index {
    if (p.nextToken() != .id) {
        unreachable;
    }

    const fn_name =
        p.push(
        .id,
        p.tokens.items(.from)[p.cursor],
        p.tokens.items(.to)[p.cursor],
    ) catch unreachable;
    const dependent_args = blk: {
        if (!p.eatToken(.@"<"))
            break :blk 0;
        const result = p.pDeclList();

        if (!p.eatToken(.@">")) {
            // ERR, todo
        }
        break :blk result;
    };
    const args = blk: {
        if (p.eatToken(.@"(")) {
            // ERR, todo
        }

        const result = p.pDeclList();

        if (p.eatToken(.@")")) {
            // ERR, todo
        }
        break :blk result;
    };
    const return_type = blk: {
        if (!(p.eatToken(.@"-") and p.eatToken(.@">"))) break :blk 0;
        break :blk p.pExpr();
    };
    const fn_proto = @as(Index, @intCast(p.prisma.items.len));
    p.prismaPush(fn_name);
    p.prismaPush(dependent_args);
    p.prismaPush(args);
    p.prismaPush(return_type);

    const fn_def =
        p.push(.fn_def, fn_proto, 0) catch unreachable;

    return fn_def;
}

pub fn srcContent(self: Self, id: Index) []const u8 {
    const from = self.gef(id, .lhs);
    const to = self.gef(id, .rhs);
    return self.src[from..to];
}

fn pDeclList(p: *Self) Index {
    _ = p;
    return 0;
}

fn expectConstruction(p: *Self) Index {
    const e = p.pConstruction();
    if (e == 0) {
        p.panic(unreachable);
    }
    return e;
}

pub fn pConstruction(_: *Self) Index {
    unreachable;
}

fn expectExpr(p: *Self) Index {
    const e = p.pExpr();
    if (e == 0) {
        p.panic(unreachable);
    }
    return e;
}

pub fn pExpr(p: *Self) Index {
    return p.pPratt(0);
}

fn pPrefixExpr(p: *Self) Index {
    const t = p.nextToken();

    return switch (t) {
        // todo: 23u32, "char"cstr, 23.23f32
        .int => p.push(
            .int,
            p.tokens.items(.from)[p.cursor],
            p.tokens.items(.to)[p.cursor],
        ) catch unreachable,
        .real => p.push(
            .real,
            p.tokens.items(.from)[p.cursor],
            p.tokens.items(.to)[p.cursor],
        ) catch unreachable,
        .id => p.push(
            .id,
            p.tokens.items(.from)[p.cursor],
            p.tokens.items(.to)[p.cursor],
        ) catch unreachable,
        .str => p.push(
            .str,
            p.tokens.items(.from)[p.cursor],
            p.tokens.items(.to)[p.cursor],
        ) catch unreachable,
        .k_true => p.push(
            .bool_expr,
            1,
            0,
        ) catch unreachable,
        .k_false => p.push(
            .bool_expr,
            0,
            0,
        ) catch unreachable,
        // bool not
        .k_unreachable => p.push(
            .unreachable_expr,
            0,
            0,
        ) catch unreachable,
        .k_void => p.push(
            .type_void,
            0,
            0,
        ) catch unreachable,
        .k_noreturn => p.push(
            .type_noreturn,
            0,
            0,
        ) catch unreachable,
        .k_unit => p.push(
            .unit_expr,
            0,
            0,
        ) catch unreachable,
        .k_null => p.push(
            .null_expr,
            0,
            0,
        ) catch unreachable,
        .k_any => p.push(
            .any_expr,
            0,
            0,
        ) catch unreachable,
        .k_Any => p.push(
            .type_Any,
            0,
            0,
        ) catch unreachable,

        .k_not => p.push(
            .bool_not,
            p.expectExpr(),
            0,
        ) catch unreachable,
        .@"-" => p.push(
            .negative,
            p.expectExpr(),
            0,
        ) catch unreachable,
        // reference type
        .@"&" => {
            var r: Index = undefined;
            if (p.eatToken(.k_mut))
                r = p.push(
                    .type_ref,
                    p.expectExpr(),
                    0,
                ) catch unreachable
            else
                r = p.push(
                    .type_ref,
                    p.expectExpr(),
                    0,
                ) catch unreachable;
            return r;
        },
        .@"*" => 0,
        .@"^" => 0,
        .@"#" => 0,
        .@"!" => 0,
        .@"[" => 0,
        .@"{" => 0,
        .@"(" => 0,
        .@"@" => 0,
        .k_match => 0,
        .k_if => 0,
        .k_when => 0,
        .k_inline => 0,
        .k_opaque => 0,
        .k_struct => 0,
        .k_enum => 0,
        .k_tuple => 0,
        .k_comptime => 0,
        .k_return => 0,
        .k_handle => 0,
        .k_trait => 0,
        .k_fn => 0,
        .k_error => 0,
        .@"\\" => 0,
        .@"." => {
            _ = p.eatToken(.@".");

            // arbitary struct construction
            if (p.eatToken(.@"{")) {
                return p.push(
                    .arbitrary_construction,
                    p.expectConstruction(),
                    0,
                ) catch unreachable;
            }
            return p.push(
                .symbol,
                p.tokens.items(.from)[p.cursor],
                p.tokens.items(.to)[p.cursor],
            ) catch unreachable;
        },
        else => 0,
    };
}

const OpInfo = struct {
    prec: i8,
    tag: Node.Tag,
    assoc_left: bool = true,
};

const op_table = std.enums.directEnumArrayDefault(
    Token.Tag,
    OpInfo,
    .{ .prec = -1, .tag = .invalid },
    0,
    .{
        .k_or = .{ .prec = 10, .tag = .bool_or },

        .k_and = .{ .prec = 20, .tag = .bool_and },

        .@"==" = .{ .prec = 30, .tag = .bool_eq },
        .@"!=" = .{ .prec = 30, .tag = .bool_not_eq },
        .@">=" = .{ .prec = 30, .tag = .bool_gt_eq },
        .@" > " = .{ .prec = 30, .tag = .bool_gt },
        .@"<=" = .{ .prec = 30, .tag = .bool_lt_eq },
        .@" < " = .{ .prec = 30, .tag = .bool_lt },

        .@".." = .{ .prec = 40, .tag = .range },
        .@"..=" = .{ .prec = 40, .tag = .range_inclusive },

        .@" + " = .{ .prec = 50, .tag = .add },
        .@" - " = .{ .prec = 50, .tag = .minus },

        .@" / " = .{ .prec = 50, .tag = .add },
        .@" * " = .{ .prec = 50, .tag = .add },
        .@" % " = .{ .prec = 50, .tag = .add },

        .@"++" = .{ .prec = 60, .tag = .add_add },
        .@"<:" = .{ .prec = 60, .tag = .is_subtype },
        .@":>" = .{ .prec = 60, .tag = .is_supertype },

        .@"." = .{ .prec = 70, .tag = .select },
        .@"'" = .{ .prec = 70, .tag = .lift },
        .@"|" = .{ .prec = 70, .tag = .pipe },
        .@"#" = .{ .prec = 70, .tag = .eff_elm },
        .@"!" = .{ .prec = 70, .tag = .err_elm },

        .@"[" = .{ .prec = 70, .tag = .index },
        .@"(" = .{ .prec = 70, .tag = .call },
        .@"<" = .{ .prec = 70, .tag = .tcall },
        .@"{" = .{ .prec = 70, .tag = .construction },
    },
);

fn pPratt(p: *Self, min_prec: i8) Index {
    var node = p.pPrefixExpr();
    if (node == 0) {
        return 0;
    }

    while (true) {
        const token_tag = p.peakToken();
        const info = op_table[@as(usize, @intCast(@intFromEnum(token_tag)))];

        switch (token_tag) {
            // tcall
            .@"<" => {
                break;
            },

            // call
            .@"(" => {
                std.debug.print("-----------------loop on {}--------------\n", .{min_prec});
                std.debug.print(
                    "eat first: {s}\neat second: {s}\n",
                    .{
                        @tagName(p.nextToken()),
                        @tagName(p.nextToken()),
                    },
                );

                node = p.push(.call, node, 0) catch unreachable;
                // break;
            },

            // construction
            .@"{" => {
                break;
            },

            // index
            .@"[" => {
                break;
            },

            // ?
            .@"." => {
                break;
            },

            // todo
            .@"#" => {
                break;
            },
            .@"!" => {
                break;
            },

            else => {},
        }

        if (info.prec < min_prec) {
            break;
        }

        _ = p.nextToken();

        const rhs = p.pPratt(info.prec + 1);
        node = p.push(info.tag, node, rhs) catch unreachable;
    }

    return node;
}

fn pArgs(p: *Self) Index {
    const first = p.pExpr();
    if (first == 0) return 0;

    p.prismaPush(0);
    const result = p.prisma.items.len - 1;
    p.prismaPush(first);
    var count: usize = 1;

    while (true) {
        const next = p.nextToken();
        const next_next = p.peakToken();

        if (next == .@")" or next_next == .@")") {
            break;
        }

        p.prismaPush(p.pExpr());
        count += 1;
    }

    p.prisma.items[result] = count;

    return result;
}

// fn pOrderedScope(p: *Self) Index {}
// fn pUnorderedScope(p: *Self) Index {}
// fn pListOf(p: *Self) Index {}
// fn pPair(p: *Self) Index {}

// fn pParameters(p: *Self) Index {}
// fn pArgs(p: *Self) Index {}

// fn pWhereClauses(p: *Self) Index {}
// fn pClauses(p: *Self) Index {}
// fn pArgs(p: *Self) Index {}

// fn pParameters(p: *Self) Index {}
// fn pParameters(p: *Self) Index {}
// fn pParameters(p: *Self) Index {}

pub fn init(gpa: Alc, arena: Alc, src: []const u8, ts: *const Soa(Token)) Self {
    var result = Self{
        .src = src,
        .tokens = ts,
        .gpa = gpa,
        .arena = arena,
        .nodes = .{},
        .nodes_capacity = 128,
        .map = std.StringArrayHashMap(Index).init(gpa),
        .str_bytes = std.ArrayList(u8).init(gpa),
        .prisma = std.ArrayList(Index).init(gpa),
    };

    result.nodes.setCapacity(gpa, 128) catch unreachable;
    _ = result.push(.placeholder, 0, 0) catch unreachable;
    return result;
}

pub fn dump(self: Self, node: Index, offset: usize) void {
    const print = std.debug.print;

    for (0..offset) |_| {
        print(" ", .{});
    }

    if (offset != 0) print("⎣-", .{});

    if (node == 0) {
        print("null\n", .{});
        return;
    }

    print("%{} {s}: ", .{ node, @tagName(self.gef(node, .tag)) });
    switch (self.gef(node, .tag)) {
        .symbol,
        => {
            print(".{s}\n", .{self.srcContent(node)});
            return;
        },
        .id, .int, .real => {
            print("{s}\n", .{self.srcContent(node)});
            return;
        },

        else => {
            print("\n", .{});
        },
    }

    self.dump(self.gef(node, .lhs), offset + 2);
    self.dump(self.gef(node, .rhs), offset + 2);
}

pub fn directDump(self: Self) void {
    for (self.nodes.items(.tag)) |tag| {
        std.debug.print("{s}\n", .{@tagName(tag)});
    }
}

fn push(p: *Self, tag: Node.Tag, lhs: Index, rhs: Index) !Index {
    if (p.nodes.len >= p.nodes_capacity) {
        const new_capacity = @as(usize, @intFromFloat(@as(f64, @floatFromInt(p.nodes_capacity)) * 1.5));
        try p.nodes.setCapacity(p.gpa, new_capacity);
        p.nodes_capacity = new_capacity;
    }
    p.nodes.appendAssumeCapacity(.{ .tag = tag, .lhs = lhs, .rhs = rhs });
    return @as(Index, @intCast(p.nodes.len - 1));
}

fn prismaPush(p: *Self, index: Index) void {
    p.prisma.append(index) catch unreachable;
}

fn gef(p: Self, node: Index, field: anytype) FieldType(field) {
    return p.nodes.items(field)[node];
}

fn nextToken(p: *Self) Token.Tag {
    p.cursor += 1;
    if (p.cursor >= p.tokens.len) return Token.Tag.eof;
    return p.tokens.items(.tag)[p.cursor];
}

// not avaliable, bugged
fn eatToken(p: *Self, tag: Token.Tag) bool {
    if (p.cursor >= p.tokens.len) return false;
    // std.debug.print(
    //     "compare {s} with current cursor: {s}\n",
    //     .{
    //         @tagName(tag),
    //         @tagName(p.tokens.items(.tag)[p.ts_cusor]),
    //     },
    // );
    if (tag == p.tokens.items(.tag)[p.cursor]) {
        std.debug.print("cursor moved\n", .{});
        p.cursor += 1;
        return true;
    }

    return false;
}
fn expectToken(p: *Self, tag: Token.Tag) bool {
    if (p.cursor >= p.tokens.len) return false;

    if (tag == p.tokens.items(.tag)[p.cursor]) {
        return true;
    }

    return false;
}
fn peakToken(p: *Self) Token.Tag {
    if (p.cursor + 1 >= p.tokens.len) return Token.Tag.eof;

    return p.tokens.items(.tag)[p.cursor + 1];
}

fn expectNextToken(p: *Self, tag: Token.Tag) bool {
    if (p.cursor + 1 >= p.tokens.len) return false;

    if (tag == p.tokens.items(.tag)[p.cursor + 1]) {
        return true;
    }

    return false;
}

pub fn dumpTokens(p: Self) void {
    for (
        p.tokens.items(.tag),
        p.tokens.items(.from),
        p.tokens.items(.to),
    ) |t, f, to| {
        std.debug.print(
            "{s}\t{s}\n",
            .{ @tagName(t), p.src[f..to] },
        );
    }
}

fn panic(_: Self, _: Ast.Err) void {}

fn FieldType(field: anytype) type {
    return switch (field) {
        .tag => Node.Tag,
        .lhs, .rhs => Index,
        else => @compileError("Unknown field: " ++ @tagName(field)),
    };
}

const Self = @This();
const Ast = @import("Ast.zig");
const Node = Ast.Node;
const Index = @import("../base/types.zig").Index;
const std = @import("std");
const Soa = std.MultiArrayList;
const Alc = std.mem.Allocator;
const Token = @import("../lex/lex.zig").Token;
