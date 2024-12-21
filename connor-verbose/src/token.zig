const std = @import("std");
const Allocator = std.mem.Allocator;
const Cursor = @import("tools.zig").Cursor;
const Vec = @import("tools.zig").Vec;

pub const Type = union(enum) {
    Function: struct {
        params: []const Type,
        ret: *const Type,
    },
    Tuple: []const Type,
    NoReturn,
    CompInt,
    U8,
    U32,

    pub fn fmt(self: Type) []const u8 {
        return switch (self) {
            .U8 => "b",
            .U32 => "w",
            else => {
                std.debug.print("[error] fmt: {}\n", .{self});
                unreachable;
            },
        };
    }

    pub fn irFmt(self: Type) []const u8 {
        return switch (self) {
            .U8 => "w",
            .U32 => "w",
            else => {
                std.debug.print("[error] irFmt: {}\n", .{self});
                unreachable;
            },
        };
    }

    pub fn eql(self: *const Type, rhs: *const Type) bool {
        return switch (self.*) {
            .Function => |l_tuple| {
                const r_tuple = if (rhs.* == .Function) rhs.Function else return false;
                return l_tuple.ret.eql(r_tuple.ret) and (Type{ .Tuple = l_tuple.params }).eql(&Type{ .Tuple = r_tuple.params });
            },
            .Tuple => |a| {
                const b = if (rhs.* == .Tuple) rhs.Tuple else return false;
                const len = if (a.len == b.len) a.len else return false;
                for (0..len) |i|
                    if (!(a[i].coercible(&b[i]))) return false;
                return true;
            },
            .CompInt => rhs.* == .CompInt,
            .U8 => rhs.* == .U8,
            .U32 => rhs.* == .U32,
            else => false,
        };
    }

    pub fn coercible(self: *const Type, rhs: *const Type) bool {
        if (self.eql(rhs)) return true;
        if (rhs.* == .Tuple and rhs.Tuple.len == 1 and self.coercible(&rhs.Tuple[0])) return true;

        return switch (self.*) {
            .NoReturn => true,
            .CompInt => rhs.* == .U8 or rhs.* == .U32,
            else => false,
        };
    }
};

pub const Token = union(enum) {
    LParen,
    RParen,
    LBracket,
    RBracket,
    Comma,
    SemiColon,
    Type: Type,
    Integer: usize,
    Word: []const u8,

    pub fn parse(allocator: Allocator, input: []const u8) Allocator.Error![]Token {
        var tokens = try Vec(Token).new(allocator);
        var cursor = Cursor(u8).new(input);

        while (cursor.pos < input.len) {
            while (cursor.curr()) |char| : (cursor.inc()) if (!std.ascii.isWhitespace(char)) break;
            if (cursor.done()) break;
            const token =
                if (p_type(&cursor)) |tok| tok else if (p_integer(&cursor)) |tok| tok else if (p_word(&cursor)) |tok| tok else if (p_char('(', .LParen)(&cursor)) |tok| tok else if (p_char(')', .RParen)(&cursor)) |tok| tok else if (p_char('{', .LBracket)(&cursor)) |tok| tok else if (p_char('}', .RBracket)(&cursor)) |tok| tok else if (p_char(',', .Comma)(&cursor)) |tok| tok else if (p_char(';', .SemiColon)(&cursor)) |tok| tok else {
                // TODO: Add Token location
                @panic("Couldn't lex!");
            };

            try tokens.push(token);
        }

        return tokens.slice();
    }

    fn p_type(cursor: *Cursor(u8)) ?Token {
        const slice = cursor.slice();
        const types = [_]struct { name: []const u8, t: Type }{
            .{ .name = "u8", .t = .U8 },
            .{ .name = "u32", .t = .U32 },
        };

        for (types) |ty|
            if (std.mem.startsWith(u8, slice, ty.name)) {
                cursor.pos += ty.name.len;
                return .{ .Type = ty.t };
            };

        return null;
    }

    fn p_integer(cursor: *Cursor(u8)) ?Token {
        const slice = cursor.slice();
        var len: usize = 0;
        while (cursor.curr()) |char| : ({
            cursor.inc();
            len += 1;
        }) if (!std.ascii.isDigit(char)) break;
        return if (len == 0) null else .{ .Integer = std.fmt.parseInt(usize, slice[0..len], 10) catch unreachable };
    }

    fn p_word(cursor: *Cursor(u8)) ?Token {
        const slice = cursor.slice();
        const start = cursor.save();
        while (cursor.curr()) |char| : (cursor.inc()) if (!std.ascii.isAlphabetic(char)) break;
        while (cursor.curr()) |char| : (cursor.inc()) if (!(std.ascii.isAlphabetic(char) or std.ascii.isDigit(char) or char == '_')) break;
        const len = cursor.save() - start;
        return if (len == 0) null else .{ .Word = slice[0..len] };
    }

    fn p_char(comptime c: u8, comptime token: Token) fn (*Cursor(u8)) ?Token {
        const gen = struct {
            fn f(cursor: *Cursor(u8)) ?Token {
                if (cursor.curr()) |char|
                    if (char == c) {
                        cursor.inc();
                        return token;
                    };

                return null;
            }
        };

        return gen.f;
    }
};
