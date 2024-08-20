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
                '@' => {
                    self.cursor += 1;
                    return token(.@"@", old_cursor, self.cursor);
                },

                '#' => {
                    self.cursor += 1;
                    return token(.@"#", old_cursor, self.cursor);
                },
                '%' => {
                    if (self.seperated(self.cursor)) {
                        self.cursor += 2;
                        return token(.@" % ", old_cursor, self.cursor);
                    } else {
                        self.cursor += 1;
                        return token(.@"%", old_cursor, self.cursor);
                    }
                },
                '/' => {
                    if (self.seperated(self.cursor)) {
                        self.cursor += 2;
                        return token(.@" / ", old_cursor, self.cursor);
                    } else {
                        self.cursor += 1;
                        return token(.@"/", old_cursor, self.cursor);
                    }
                },
                '|' => {
                    self.cursor += 1;
                    return token(.@"|", old_cursor, self.cursor);
                },
                '\\' => {
                    self.cursor += 1;
                    return token(.@"\\", old_cursor, self.cursor);
                },
                '&' => {
                    self.cursor += 1;
                    return token(.@"&", old_cursor, self.cursor);
                },
                '*' => {
                    self.cursor += 1;
                    return token(.@"*", old_cursor, self.cursor);
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
                '{' => {
                    self.cursor += 1;
                    return token(.@"{", old_cursor, self.cursor);
                },
                '}' => {
                    self.cursor += 1;
                    return token(.@"}", old_cursor, self.cursor);
                },
                ':' => {
                    if (self.eatChar('-')) {
                        self.cursor += 1;
                        return token(.@":-", old_cursor, self.cursor);
                    } else if (self.eatChar('>')) {
                        self.cursor += 1;
                        return token(.@":>", old_cursor, self.cursor);
                    } else {
                        self.cursor += 1;
                        return token(.@":", old_cursor, self.cursor);
                    }
                },
                ';' => {
                    self.cursor += 1;
                    return token(.@";", old_cursor, self.cursor);
                },
                ',' => {
                    self.cursor += 1;
                    return token(.@",", old_cursor, self.cursor);
                },

                '~' => {
                    self.cursor += 1;
                    return token(.@"~", old_cursor, self.cursor);
                },
                '?' => {
                    self.cursor += 1;
                    return token(.@"?", old_cursor, self.cursor);
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
                        } else if (self.eatChar(':')) {
                            self.cursor += 1;
                            return token(.@"<:", old_cursor, self.cursor);
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
                '.' => {
                    if (self.eatChar('.')) {
                        if (self.eatChar('.')) {
                            self.cursor += 1;
                            return token(.@"...", old_cursor, self.cursor);
                        } else if (self.eatChar('=')) {
                            self.cursor += 1;
                            return token(.@"..=", old_cursor, self.cursor);
                        } else {
                            self.cursor += 1;
                            return token(.@"..", old_cursor, self.cursor);
                        }
                    } else {
                        self.cursor += 1;
                        return token(.@".", old_cursor, self.cursor);
                    }
                },

                'a'...'z', 'A'...'Z' => return self.recognizeId(),

                '`' => return self.recognizeArbitaryId(),

                '_' => unreachable,

                '0'...'9' => return self.recognizeDigit(),

                ' ', '\t', '\n' => {
                    old_cursor += 1;
                },

                '"' => return self.recognizeStr(),

                else => unreachable,
            }
        }

        return token(.eof, 0, 0);
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
            if (self.cursor + 1 >= self.src.len)
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
    fn recognizeDigit(self: *Self) Token {
        const b = self.src;
        const old_cursor = self.cursor;
        var met_dot: bool = false;

        while (true) : (self.cursor += 1) {
            if (self.cursor >= self.src.len) return token(if (met_dot) .real else .int, old_cursor, self.cursor);
            const c = b[self.cursor];

            switch (c) {
                '0'...'9' => {},
                '.' => {
                    if (!met_dot) {
                        met_dot = true;
                    } else {
                        // ERR, todo
                    }
                },
                else => {
                    return token(if (met_dot) .real else .int, old_cursor, self.cursor);
                },
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

    fn peak(self: Self, char: u8) bool {
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
        @"<:",
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
        @"*",
        @" * ",
        @"|",
        @"?",
        @"/",
        @" / ",
        @"\\",
        @"#",
        @"&",
        @"[",
        @"]",
        @"(",
        @")",
        @"{",
        @"}",
        @",",
        @".",
        @"..",
        @"..=",
        @"...",
        @":",
        @":>",
        @":-",
        @"'",
        @";",
        @"~",
        @"^",
        @"$",
        @"@",
        @"_",
        @"%",
        @" % ",
        @"=",
        @"=>",
        @"==",

        // permitive literals
        str,
        str_multi_line,
        int,
        real,

        // keywords
        k_Any,
        k_and,
        k_any,
        k_as,
        k_async,
        k_await,
        k_bool,
        k_break,
        k_case,
        k_comptime,
        k_const,
        k_define,
        k_derive,
        k_do,
        k_dyn,
        k_effect,
        k_else,
        k_ensures,
        k_enum,
        k_error,
        k_extern,
        k_false,
        k_fn,
        k_for,
        k_handle,
        k_if,
        k_impl,
        k_in,
        k_inline,
        k_let,
        k_lifting,
        k_match,
        k_move,
        k_mut,
        k_mutdyn,
        k_mutptr,
        k_mutref,
        k_newtype,
        k_noreturn,
        k_not,
        k_null,
        k_opaque,
        k_or,
        k_perform,
        k_ptr,
        k_pub,
        k_pure,
        k_ref,
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
        k_tuple,
        k_typealias,
        k_undefined,
        k_union,
        k_unit,
        k_unreachable,
        k_unsafe,
        k_void,
        k_when,
        k_while,

        // others
        id,
        comment,
        invalid,
        sof,
        eof,
    };

    const keywords = std.StaticStringMap(Tag).initComptime(.{
        .{ "Any", .k_Any },
        .{ "and", .k_and },
        .{ "any", .k_any },
        .{ "as", .k_as },
        .{ "async", .k_async },
        .{ "await", .k_await },
        .{ "bool", .k_bool },
        .{ "break", .k_break },
        .{ "case", .k_case },
        .{ "comptime", .k_comptime },
        .{ "const", .k_const },
        .{ "define", .k_define },
        .{ "derive", .k_derive },
        .{ "do", .k_do },
        .{ "effect", .k_effect },
        .{ "else", .k_else },
        .{ "ensures", .k_ensures },
        .{ "enum", .k_enum },
        .{ "error", .k_error },
        .{ "extern", .k_extern },
        .{ "false", .k_false },
        .{ "fn", .k_fn },
        .{ "for", .k_for },
        .{ "handle", .k_handle },
        .{ "if", .k_if },
        .{ "impl", .k_impl },
        .{ "in", .k_in },
        .{ "inline", .k_inline },
        .{ "let", .k_let },
        .{ "lifting", .k_lifting },
        .{ "match", .k_match },
        .{ "move", .k_move },
        .{ "mut", .k_mut },
        .{ "newtype", .k_newtype },
        .{ "noreturn", .k_noreturn },
        .{ "not", .k_not },
        .{ "null", .k_null },
        .{ "opaque", .k_opaque },
        .{ "or", .k_or },
        .{ "perform", .k_perform },
        .{ "pub", .k_pub },
        .{ "pure", .k_pure },
        .{ "ref", .k_ref },
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
        .{ "tuple", .k_tuple },
        .{ "typealias", .k_typealias },
        .{ "undefined", .k_undefined },
        .{ "union", .k_union },
        .{ "unit", .k_unit },
        .{ "unreachable", .k_unreachable },
        .{ "unsafe", .k_unsafe },
        .{ "void", .k_void },
        .{ "while", .k_while },
        .{ "when", .k_when },
        .{ "mutref", .k_mutref },
        .{ "mutptr", .k_mutptr },
        .{ "mutdyn", .k_mutdyn },
        .{ "ptr", .k_ptr },
        .{ "dyn", .k_dyn },
    });
};

inline fn token(tag: Token.Tag, from: Index, to: Index) Token {
    return .{ .tag = tag, .from = from, .to = to };
}

const Index = @import("../base/types.zig").Index;
const std = @import("std");

pub fn lex(gpa: std.mem.Allocator, src: []const u8) std.MultiArrayList(Token) {
    var lexer = Lexer.init(src);
    var result = std.MultiArrayList(Token){};
    result.append(gpa, token(.sof, 0, 0)) catch unreachable;
    while (true) {
        const t = lexer.next();
        result.append(gpa, t) catch unreachable;
        if (t.tag == .eof) break;
    }
    return result;
}
