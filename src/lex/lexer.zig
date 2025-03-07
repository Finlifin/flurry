pub const Lexer = struct {
    src: []const u8,
    cursor: Index,

    // todo
    err: void,

    const Self = @This();

    pub fn next(self: *Self) Token {
        const b = self.src;
        var old_cursor = self.cursor;

        while (true) : (self.cursor += 1) {
            if (self.cursor >= self.src.len) return token(.eof, 0, 0);
            const c = b[self.cursor];

            switch (c) {
                '^' => {
                    self.cursor += 1;
                    return token(.@"^", old_cursor, self.cursor);
                },
                '.' => {
                    self.cursor += 1;
                    return token(.@".", old_cursor, self.cursor);
                },
                '\'' => {
                    // 'n' => char
                    if (self.cursor + 2 < self.src.len and b[self.cursor + 2] == '\'') {
                        self.cursor += 3;
                        return token(.char, old_cursor, self.cursor);
                    }
                    // '\n' => char
                    else if (self.cursor + 3 < self.src.len and b[self.cursor + 3] == '\'' and b[self.cursor + 1] == '\\') {
                        self.cursor += 4;
                        return token(.char, old_cursor, self.cursor);
                    }
                    // '   { ANY } => macro_content
                    // TODO
                    self.cursor += 1;
                    return token(.@"'", old_cursor, self.cursor);
                },
                '@' => {
                    self.cursor += 1;
                    return token(.@"@", old_cursor, self.cursor);
                },

                '\\' => {
                    self.cursor += 1;
                    return token(.@"\\", old_cursor, self.cursor);
                },
                '&' => {
                    self.cursor += 1;
                    return token(.@"&", old_cursor, self.cursor);
                },
                '(' => {
                    self.cursor += 1;
                    return token(.@"(", old_cursor, self.cursor);
                },
                ')' => {
                    self.cursor += 1;
                    return token(.@")", old_cursor, self.cursor);
                },
                '[' => {
                    self.cursor += 1;
                    return token(.@"[", old_cursor, self.cursor);
                },
                ']' => {
                    self.cursor += 1;
                    return token(.@"]", old_cursor, self.cursor);
                },
                ';' => {
                    self.cursor += 1;
                    return token(.@";", old_cursor, self.cursor);
                },
                ',' => {
                    self.cursor += 1;
                    return token(.@",", old_cursor, self.cursor);
                },
                '?' => {
                    self.cursor += 1;
                    return token(.@"?", old_cursor, self.cursor);
                },
                '~' => {
                    if (self.eatChar('>')) {
                        self.cursor += 1;
                        return token(.@"~>", old_cursor, self.cursor);
                    } else {
                        self.cursor += 1;
                        return token(.@"~", old_cursor, self.cursor);
                    }
                },
                '#' => {
                    self.cursor += 1;
                    return token(.@"#", old_cursor, self.cursor);
                },
                '|' => {
                    if (self.eatChar('>')) {
                        self.cursor += 1;
                        return token(.@"|>", old_cursor, self.cursor);
                    } else {
                        self.cursor += 1;
                        return token(.@"|", old_cursor, self.cursor);
                    }
                },
                '>' => {
                    if (self.seperated(self.cursor)) {
                        self.cursor += 2;
                        return token(.@" > ", old_cursor, self.cursor);
                    } else {
                        if (self.eatChar('=')) {
                            self.cursor += 1;
                            return token(.@">=", old_cursor, self.cursor);
                        } else {
                            self.cursor += 1;
                            return token(.@">", old_cursor, self.cursor);
                        }
                    }
                },

                '<' => {
                    if (self.seperated(self.cursor)) {
                        self.cursor += 2;
                        return token(.@" < ", old_cursor, self.cursor);
                    } else {
                        if (self.eatChar('=')) {
                            self.cursor += 1;
                            return token(.@"<=", old_cursor, self.cursor);
                        } else {
                            self.cursor += 1;
                            return token(.@"<", old_cursor, self.cursor);
                        }
                    }
                },

                '+' => {
                    if (self.seperated(self.cursor)) {
                        self.cursor += 2;
                        return token(.@" + ", old_cursor, self.cursor);
                    } else {
                        if (self.eatChar('=')) {
                            self.cursor += 1;
                            return token(.@"+=", old_cursor, self.cursor);
                        } else if (self.eatChar('+')) {
                            self.cursor += 1;
                            return token(.@"++", old_cursor, self.cursor);
                        } else {
                            self.cursor += 1;
                            return token(.@"+", old_cursor, self.cursor);
                        }
                    }
                },
                '-' => {
                    if (self.seperated(self.cursor)) {
                        self.cursor += 2;
                        return token(.@" - ", old_cursor, self.cursor);
                    } else {
                        if (self.eatChar('=')) {
                            self.cursor += 1;
                            return token(.@"-=", old_cursor, self.cursor);
                        } else if (self.eatChar('>')) {
                            self.cursor += 1;
                            return token(.@"->", old_cursor, self.cursor);
                        } else if (self.eatChar('-')) {
                            // comment
                            while (true) : (self.cursor += 1) {
                                if (self.cursor >= self.src.len) return token(.comment, old_cursor, self.cursor - 1);
                                const c_ = b[self.cursor];
                                if (c_ == '\n') {
                                    self.cursor += 1;
                                    return token(.comment, old_cursor, self.cursor);
                                }
                            }
                        } else {
                            self.cursor += 1;
                            return token(.@"-", old_cursor, self.cursor);
                        }
                    }
                },

                '!' => {
                    if (self.eatChar('=')) {
                        self.cursor += 1;
                        return token(.@"!=", old_cursor, self.cursor);
                    } else {
                        self.cursor += 1;
                        return token(.@"!", old_cursor, self.cursor);
                    }
                },

                '=' => {
                    if (self.eatChar('=')) {
                        self.cursor += 1;
                        return token(.@"==", old_cursor, self.cursor);
                    } else if (self.eatChar('>')) {
                        self.cursor += 1;
                        return token(.@"=>", old_cursor, self.cursor);
                    } else {
                        self.cursor += 1;
                        return token(.@"=", old_cursor, self.cursor);
                    }
                },

                '%' => {
                    if (self.seperated(self.cursor)) {
                        self.cursor += 2;
                        return token(.@" % ", old_cursor, self.cursor);
                    } else {
                        if (self.eatChar('=')) {
                            self.cursor += 1;
                            return token(.@"%=", old_cursor, self.cursor);
                        } else {
                            self.cursor += 1;
                            return token(.@"%", old_cursor, self.cursor);
                        }
                    }
                },
                '{' => {
                    if (self.eatChar('-')) {
                        // comment
                        while (true) : (self.cursor += 1) {
                            if (self.cursor >= self.src.len) return token(.comment, old_cursor, self.cursor - 1);
                            const c_ = b[self.cursor];
                            if (c_ == '-') {
                                if (self.eatChar('}')) {
                                    self.cursor += 1;
                                    return token(.comment, old_cursor, self.cursor);
                                }
                            }
                        }
                    } else {
                        self.cursor += 1;
                        return token(.@"{", old_cursor, self.cursor);
                    }
                },
                '}' => {
                    self.cursor += 1;
                    return token(.@"}", old_cursor, self.cursor);
                },
                ':' => {
                    if (self.eatChar('-')) {
                        self.cursor += 1;
                        return token(.@":-", old_cursor, self.cursor);
                    } else if (self.eatChar(':')) {
                        self.cursor += 1;
                        return token(.@"::", old_cursor, self.cursor);
                    } else if (self.eatChar('~')) {
                        self.cursor += 1;
                        return token(.@"::", old_cursor, self.cursor);
                    } else {
                        self.cursor += 1;
                        return token(.@":", old_cursor, self.cursor);
                    }
                },
                '*' => {
                    if (self.seperated(self.cursor)) {
                        self.cursor += 2;
                        return token(.@" * ", old_cursor, self.cursor);
                    } else if (self.eatChar('=')) {
                        self.cursor += 1;
                        return token(.@"*=", old_cursor, self.cursor);
                    } else {
                        self.cursor += 1;
                        return token(.@"*", old_cursor, self.cursor);
                    }
                },
                '/' => {
                    if (self.seperated(self.cursor)) {
                        self.cursor += 2;
                        return token(.@" / ", old_cursor, self.cursor);
                    } else if (self.eatChar('=')) {
                        self.cursor += 1;
                        return token(.@"/=", old_cursor, self.cursor);
                    } else {
                        self.cursor += 1;
                        return token(.@"/", old_cursor, self.cursor);
                    }
                },
                '_', 'a'...'z', 'A'...'Z' => return self.recognizeId(),

                '`' => return self.recognizeArbitaryId(),

                '0'...'9' => return self.recognizeDigit(),

                ' ', '\t', '\n' => {
                    old_cursor += 1;
                },

                '"' => return self.recognizeStr(),

                else => {
                    std.debug.print("{c}\n", .{c});
                    unreachable;
                },
            }
        }

        return token(.eof, b.len, b.len);
    }
    pub fn init(src: []const u8) Self {
        return .{
            .src = src,
            .cursor = 0,
            .err = undefined,
        };
    }
    pub fn deinit(_: *Self) void {}

    fn recognizeStr(self: *Self) Token {
        const b = self.src;
        const old_cursor = self.cursor;
        var started: bool = false;

        while (true) : (self.cursor += 1) {
            if (self.cursor >= self.src.len) return token(.eof, 0, 0);
            const c = b[self.cursor];

            switch (c) {
                '"' => {
                    if (!started)
                        started = true
                    else if (self.cursor - 1 >= 0 and (b[self.cursor - 1] != '\\')) {
                        self.cursor += 1;
                        return token(.str, old_cursor, self.cursor);
                    }
                },
                else => {},
            }
        }
        return token(.invalid, old_cursor, self.cursor);
    }
    fn recognizeMultiLineStr() void {}
    fn recognizeComment() void {}

    fn recognizeId(self: *Self) Token {
        const b = self.src;
        const old_cursor = self.cursor;
        while (true) : (self.cursor += 1) {
            if (self.cursor >= self.src.len)
                if (Token.keywords.get(b[old_cursor..self.cursor])) |kw|
                    return token(kw, old_cursor, self.cursor)
                else {
                    return token(.id, old_cursor, self.cursor);
                };
            const c = b[self.cursor];

            switch (c) {
                'a'...'z', 'A'...'Z', '0'...'9', '_' => {},
                else => {
                    if (Token.keywords.get(b[old_cursor..self.cursor])) |kw|
                        return token(kw, old_cursor, self.cursor)
                    else {
                        return token(.id, old_cursor, self.cursor);
                    }
                },
            }
        }
    }

    // 23 => int(23)
    // 23.0 => real(23.0)
    fn recognizeDigit(self: *Self) Token {
        const b = self.src;
        const old_cursor = self.cursor;

        while (true) : (self.cursor += 1) {
            if (self.cursor >= self.src.len) return token(.int, old_cursor, self.cursor);
            const c = b[self.cursor];
            switch (c) {
                '0'...'9', '_' => {},
                '.' => {
                    // 如果b[self.cursor + 1] == '0'...'9'，则是real
                    if (self.cursor + 1 < self.src.len and b[self.cursor + 1] >= '0' and b[self.cursor + 1] <= '9') {
                        self.cursor += 1;
                        while (true) : (self.cursor += 1) {
                            if (self.cursor >= self.src.len) return token(.real, old_cursor, self.cursor);
                            const c_ = b[self.cursor];
                            if (c_ < '0' or c_ > '9') break;
                        }
                        return token(.real, old_cursor, self.cursor);
                    } else {
                        // 如果不是数字，则是int
                        return token(.int, old_cursor, self.cursor);
                    }
                },
                else => return token(.int, old_cursor, self.cursor),
            }
        }
    }
    fn recognizeArbitaryId(self: *Self) Token {
        const b = self.src;
        const old_cursor = self.cursor;
        var started: bool = false;

        while (true) : (self.cursor += 1) {
            if (self.cursor >= self.src.len) return token(.eof, 0, 0);
            const c = b[self.cursor];

            switch (c) {
                '`' => {
                    if (!started)
                        started = true
                    else {
                        self.cursor += 1;
                        return token(.id, old_cursor, self.cursor);
                    }
                },
                else => {},
            }
        }
        return token(.invalid, old_cursor, self.cursor);
    }

    fn peek(self: Self, char: u8) bool {
        const at = self.cursor;
        return if (self.src.len <= at + 1) false else if (self.src[at + 1] == char) true else false;
    }

    fn seperated(self: Self, at: Index) bool {
        return if (self.src.len <= at + 1 or at < 1) false else if (self.src[at + 1] == ' ' and self.src[at - 1] == ' ') true else false;
    }

    fn eatChar(self: *Self, char: u8) bool {
        var result: bool = false;

        if (self.src.len <= self.cursor + 1) return false;

        if (self.src[self.cursor + 1] == char) {
            self.cursor += 1;
            result = true;
        }

        return result;
    }
};

pub const Token = struct {
    tag: Tag,
    from: Index,
    to: Index,

    pub const Tag = enum(Index) {
        // operators
        @"+",
        @"+=",
        @"++",
        @" + ",
        @"<",
        @"<=",
        @" < ",
        @">",
        @">=",
        @" > ",
        @"!",
        @"!=",
        @"-",
        @"->",
        @"-=",
        @" - ",
        @".",
        @"..=",
        @":",
        @"::",
        @":~",
        @":-",
        @"*",
        @"*=",
        @" * ",
        @"/",
        @"/=",
        @" / ",
        @"%",
        @"%=",
        @" % ",
        @"=",
        @"=>",
        @"==",
        @"==>",
        @"~",
        @"~>",
        @"|",
        @"|>",
        @"#",
        @"?",
        @"\\",
        @"&",
        @"[",
        @"]",
        @"(",
        @")",
        @"{",
        @"}",
        @",",
        @"'",
        @";",
        @"^",
        @"$",
        @"@",
        @"_",

        // permitive literals
        str,
        int,
        real,
        char,

        // keywords
        k_and,
        k_as,
        k_asserts,
        k_asume,
        k_async,
        k_atomic,
        k_await,
        k_bool,
        k_break,
        k_case,
        k_catch,
        k_comptime,
        k_const,
        k_continue,
        k_decreases,
        k_define,
        k_derive,
        k_do,
        k_dyn,
        k_effect,
        k_else,
        k_ensures,
        k_enum,
        k_error,
        k_exists,
        k_extend,
        k_extern,
        k_false,
        k_fn,
        k_Fn,
        k_for,
        k_forall,
        k_ghost,
        k_handles,
        k_if,
        k_impl,
        k_in,
        k_inline,
        k_invariant,
        k_is,
        k_itself,
        k_lemma,
        k_let,
        k_match,
        k_matches,
        k_move,
        k_mod,
        k_mut,
        k_newtype,
        k_not,
        k_null,
        k_opaque,
        k_opens,
        k_or,
        k_outcome,
        k_perform,
        k_predicate,
        k_private,
        k_pub,
        k_pure,
        k_ref,
        k_refines,
        k_requires,
        k_resume,
        k_return,
        k_self,
        k_Self,
        k_static,
        k_struct,
        k_test,
        k_trait,
        k_true,
        k_typealias,
        k_union,
        k_unsafe,
        k_use,
        k_when,
        k_while,
        k_where,

        // others
        id,
        macro_content,
        comment,
        invalid,
        sof,
        eof,
    };

    const keywords = std.StaticStringMap(Tag).initComptime(.{
        .{ "and", .k_and },
        .{ "as", .k_as },
        .{ "asserts", .k_asserts },
        .{ "asume", .k_asume },
        .{ "async", .k_async },
        .{ "atomic", .k_atomic },
        .{ "await", .k_await },
        .{ "bool", .k_bool },
        .{ "break", .k_break },
        .{ "case", .k_case },
        .{ "catch", .k_catch },
        .{ "comptime", .k_comptime },
        .{ "const", .k_const },
        .{ "continue", .k_continue },
        .{ "decreases", .k_decreases },
        .{ "define", .k_define },
        .{ "derive", .k_derive },
        .{ "do", .k_do },
        .{ "dyn", .k_dyn },
        .{ "effect", .k_effect },
        .{ "else", .k_else },
        .{ "ensures", .k_ensures },
        .{ "enum", .k_enum },
        .{ "error", .k_error },
        .{ "exists", .k_exists },
        .{ "extend", .k_extend },
        .{ "extern", .k_extern },
        .{ "false", .k_false },
        .{ "fn", .k_fn },
        .{ "Fn", .k_Fn },
        .{ "for", .k_for },
        .{ "forall", .k_forall },
        .{ "ghost", .k_ghost },
        .{ "handles", .k_handles },
        .{ "if", .k_if },
        .{ "impl", .k_impl },
        .{ "in", .k_in },
        .{ "inline", .k_inline },
        .{ "invariant", .k_invariant },
        .{ "is", .k_is },
        .{ "itself", .k_itself },
        .{ "lemma", .k_lemma },
        .{ "let", .k_let },
        .{ "match", .k_match },
        .{ "matches", .k_matches },
        .{ "move", .k_move },
        .{ "mod", .k_mod },
        .{ "mut", .k_mut },
        .{ "newtype", .k_newtype },
        .{ "not", .k_not },
        .{ "null", .k_null },
        .{ "opaque", .k_opaque },
        .{ "opens", .k_opens },
        .{ "or", .k_or },
        .{ "outcome", .k_outcome },
        .{ "perform", .k_perform },
        .{ "predicate", .k_predicate },
        .{ "private", .k_private },
        .{ "pub", .k_pub },
        .{ "pure", .k_pure },
        .{ "ref", .k_ref },
        .{ "refines", .k_refines },
        .{ "requires", .k_requires },
        .{ "resume", .k_resume },
        .{ "return", .k_return },
        .{ "self", .k_self },
        .{ "Self", .k_Self },
        .{ "static", .k_static },
        .{ "struct", .k_struct },
        .{ "test", .k_test },
        .{ "trait", .k_trait },
        .{ "true", .k_true },
        .{ "typealias", .k_typealias },
        .{ "union", .k_union },
        .{ "unsafe", .k_unsafe },
        .{ "use", .k_use },
        .{ "when", .k_when },
        .{ "while", .k_while },
        .{ "where", .k_where },
    });
};

inline fn token(tag: Token.Tag, from: Index, to: Index) Token {
    return .{ .tag = tag, .from = from, .to = to };
}

const Index = u32;
const std = @import("std");

pub fn lex(gpa: std.mem.Allocator, src: []const u8) !std.ArrayList(Token) {
    var lexer = Lexer.init(src);
    var result = std.ArrayList(Token).init(gpa);
    try result.append(token(.sof, 0, 0));
    while (true) {
        const t = lexer.next();
        if (t.tag != .comment)
            try result.append(t);
        if (t.tag == .eof) break;
    }
    return result;
}
