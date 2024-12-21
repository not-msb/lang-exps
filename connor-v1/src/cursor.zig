const std = @import("std");
const lib = @import("lib.zig");
const Allocator = std.mem.Allocator;
const Token = lib.Token;
const Self = @This();

allocator: Allocator,
items: []const Token,
pos: usize = 0,

pub fn from(allocator: Allocator, slice: []const Token) Self {
    return .{
        .allocator = allocator,
        .items = slice,
    };
}

pub fn deinit(self: *Self) void {
    self.allocator.free(self.items);
}

pub fn save(self: Self) usize {
    return self.pos;
}

pub fn reset(self: *Self, pos: usize) void {
    self.pos = pos;
}

pub fn done(self: Self) bool {
    return self.pos >= self.items.len;
}

pub fn next(self: *Self) Token {
    if (self.done()) return .Eof;
    self.pos += 1;
    return self.items[self.pos - 1];
}

pub fn peek(self: *Self) Token {
    if (self.done()) return .Eof;
    return self.items[self.pos];
}
