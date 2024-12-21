const std = @import("std");
const Allocator = std.mem.Allocator;
const tools = @import("tools.zig");
const Output = tools.Output;
const Cursor = tools.Cursor;
const types = @import("types.zig");
const Type = types.Type;
const Token = types.Token;

fn takeWhile(comptime T: type, input: *Cursor(T), f: fn (T) bool) ?usize {
    var i: usize = 0;
    while (!input.done() and f(input.slice()[0])) : (i += 1)
        input.next();
    return if (i == 0) null else i;
}

fn char(comptime C: u8, input: *Cursor(u8)) ?void {
    if (input.slice()[0] != C) return null;
    input.next();
}

fn word(comptime W: []const u8, input: *Cursor(u8)) ?void {
    const slice = input.slice();
    if (W.len <= slice.len and std.mem.eql(u8, W, slice[0..W.len])) {
        for (0..W.len) |_|
            input.next();
    } else return null;
}

fn isIdentifierChar(byte: u8) bool {
    return std.ascii.isAlphabetic(byte) or std.ascii.isDigit(byte) or byte == '_';
}

fn whitespace(input: *Cursor(u8)) void {
    _ = takeWhile(u8, input, std.ascii.isWhitespace);
}

fn identifier(input: *Cursor(u8)) ?[]const u8 {
    const pos = input.save();
    if (takeWhile(u8, input, std.ascii.isAlphabetic)) |length| {
        const total = length + (takeWhile(u8, input, isIdentifierChar) orelse 0);
        return input.data()[pos .. pos + total];
    } else return null;
}

fn integer(input: *Cursor(u8)) ?usize {
    const pos = input.save();
    return if (takeWhile(u8, input, std.ascii.isDigit)) |length|
        std.fmt.parseInt(usize, input.data()[pos .. pos + length], 10) catch unreachable
    else
        null;
}

fn ty(input: *Cursor(u8)) ?Type {
    return if (word("u8", input)) |_|
        .{ .U8 = undefined }
    else if (word("i32", input)) |_|
        .{ .I32 = undefined }
    else
        null;
}

pub fn tokenizer(input: []const u8, allocator: Allocator) Allocator.Error![]Token {
    var tokens = try allocator.alloc(Token, 0);
    var cursor = Cursor(u8){ .ptr = input };

    while (!cursor.done()) {
        var token: Token = undefined;
        whitespace(&cursor);
        if (cursor.done()) break;

        token = if (word("fn", &cursor)) |_|
            .{ .function = undefined }
        else if (word("return", &cursor)) |_|
            .{ .ret = undefined }
        else if (word("const", &cursor)) |_|
            .{ .constant = undefined }
        else if (ty(&cursor)) |t|
            .{ .ty = t }
        else if (char(':', &cursor)) |_|
            .{ .colon = undefined }
        else if (char(';', &cursor)) |_|
            .{ .semicolon = undefined }
        else if (char(',', &cursor)) |_|
            .{ .comma = undefined }
        else if (char('(', &cursor)) |_|
            .{ .l_paren = undefined }
        else if (char(')', &cursor)) |_|
            .{ .r_paren = undefined }
        else if (char('{', &cursor)) |_|
            .{ .l_bracket = undefined }
        else if (char('}', &cursor)) |_|
            .{ .r_bracket = undefined }
        else if (char('=', &cursor)) |_|
            .{ .equal = undefined }
        else if (char('+', &cursor)) |_|
            .{ .plus = undefined }
        else if (char('-', &cursor)) |_|
            .{ .minus = undefined }
        else if (char('*', &cursor)) |_|
            .{ .star = undefined }
        else if (char('/', &cursor)) |_|
            .{ .slash = undefined }
        else if (identifier(&cursor)) |value|
            .{ .identifier = value }
        else if (integer(&cursor)) |value|
            .{ .integer = value }
        else {
            std.debug.print("Couldn't parse token from \"{s}\"\n", .{cursor.slice()});
            unreachable;
        };

        tokens = try allocator.realloc(tokens, tokens.len + 1);
        tokens[tokens.len - 1] = token;
    }

    return tokens;
}
