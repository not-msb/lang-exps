const std = @import("std");
const Allocator = std.mem.Allocator;

pub fn Cursor(comptime T: type) type {
    return struct {
        ptr: []const T,
        pos: usize = 0,

        const Self = @This();

        pub fn next(self: *Self) void {
            self.pos += 1;
        }

        pub fn done(self: *const Self) bool {
            return self.pos >= self.ptr.len;
        }

        pub fn save(self: *Self) usize {
            return self.pos;
        }

        pub fn restore(self: *Self, pos: usize) void {
            self.pos = pos;
        }

        pub fn data(self: *const Self) []const T {
            return self.ptr;
        }

        pub fn slice(self: *const Self) []const T {
            return self.ptr[self.pos..];
        }
    };
}

pub fn box(allocator: Allocator, value: anytype) Allocator.Error!*@TypeOf(value) {
    const ptr = try allocator.create(@TypeOf(value));
    ptr.* = value;
    return ptr;
}

pub fn push(allocator: Allocator, comptime T: type, slice: []T, value: T) Allocator.Error![]T {
    var tmp = try allocator.realloc(slice, slice.len + 1);
    tmp[tmp.len - 1] = value;
    return tmp;
}

pub fn append(allocator: Allocator, comptime T: type, slice: []T, source: []const T) Allocator.Error![]T {
    const pos = slice.len;
    var tmp = try allocator.realloc(slice, slice.len + source.len);
    for (source, pos..) |item, i|
        tmp[i] = item;
    return tmp;
}
