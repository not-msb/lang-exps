const std = @import("std");
const lib = @import("lib.zig");
const panic = std.debug.panic;
const ArrayList = std.ArrayList;
const Allocator = std.mem.Allocator;
const Type = lib.Type;
const Box = lib.box.Box;
const Cursor = @import("cursor.zig");

pub const Token = union(enum) {
    Eof,
    Add,
    Sub,
    Star,
    Ref,
    Assign,
    Eq,
    LParen,
    RParen,
    LBracket,
    RBracket,
    SemiColon,
    Comma,
    If,
    Else,
    Return,
    Export,
    Extern,
    Type: Type,
    Integer: usize,
    Word: []const u8,

    const keywords = std.ComptimeStringMap(Token, .{
        .{ "if", .If },
        .{ "else", .Else },
        .{ "return", .Return },
        .{ "extern", .Extern },
        .{ "export", .Export },
        .{ "u8", .{ .Type = .U8 } },
        .{ "u32", .{ .Type = .U32 } },
        .{ "u64", .{ .Type = .U64 } },
        .{ "noreturn", .{ .Type = .NoReturn } },
    });

    pub fn parse(allocator: Allocator, input: []const u8) Allocator.Error!Cursor {
        var tokens = ArrayList(Token).init(allocator);
        var i: usize = 0;

        while (i < input.len) : (i += 1) {
            while (i < input.len and std.ascii.isWhitespace(input[i])) i += 1;
            if (i >= input.len) break;

            const token: Token = switch (input[i]) {
                '+' => .Add,
                '-' => .Sub,
                '*' => .Star,
                '&' => .{ .Type = try parseType(allocator, input, &i) },
                '=' => if (i+1 < input.len and input[i+1] == '=') b: { i += 1; break :b .Eq; } else .Assign,
                '(' => .LParen,
                ')' => .RParen,
                '{' => .LBracket,
                '}' => .RBracket,
                ';' => .SemiColon,
                ',' => .Comma,
                '0'...'9' => b: {
                    const j = i;
                    while (i < input.len and std.ascii.isDigit(input[i])) i += 1 else i -= 1;
                    const int = std.fmt.parseInt(usize, input[j..i+1], 10) catch unreachable;
                    break :b .{ .Integer = int };
                },
                'a'...'z', 'A'...'Z', '_' => b: {
                    const j = i;
                    while (i < input.len and (std.ascii.isAlphabetic(input[i]) or std.ascii.isDigit(input[i]) or input[i] == '_')) i += 1 else i -= 1;
                    const word = input[j..i+1];
                    break :b keywords.get(word) orelse .{ .Word = word };
                },
                else => unreachable,
            };

            try tokens.append(token);
        }

        return Cursor.from(allocator, try tokens.toOwnedSlice());
    }

    fn parseType(allocator: Allocator, input: []const u8, _i: *usize) Allocator.Error!Type {
        var i = _i.*;
        if (i >= input.len) @panic("Reached Eof");
        const ty: Type = switch (input[i]) {
            '&' => b: {
                i += 1;
                break :b .{ .Pointer = try Box(Type).init(try parseType(allocator, input, &i)) };
            },
            'a'...'z', 'A'...'Z' => b: {
                const j = i;
                while (i < input.len and (std.ascii.isAlphabetic(input[i]) or std.ascii.isDigit(input[i]))) i += 1 else i -= 1;
                const word = input[j..i+1];
                break :b keywords.get(word).?.Type;
            },
            else => @panic("Couldn't parse type"),
        };

        _i.* = i;
        return ty;
    }
};
