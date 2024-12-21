const std = @import("std");
const Allocator = std.mem.Allocator;

pub fn Cursor(comptime T: type) type {
    return struct {
        data: []const T,
        pos: usize = 0,

        const Self = @This();

        pub fn new(data: []const T) Self {
            return .{
                .data = data,
            };
        }

        pub fn save(self: *const Self) usize {
            return self.pos;
        }

        pub fn reset(self: *Self, pos: usize) void {
            self.pos = pos;
        }

        pub fn slice(self: *const Self) []const T {
            return self.data[self.pos..];
        }

        pub fn curr(self: *Self) ?T {
            if (self.pos >= self.data.len) return null;
            return self.data[self.pos];
        }

        pub fn next(self: *Self) ?T {
            const token = self.curr();
            self.inc();
            return token;
        }

        pub fn inc(self: *Self) void {
            self.pos += 1;
        }

        pub fn dec(self: *Self) void {
            self.pos -= 1;
        }

        pub fn done(self: *const Self) bool {
            return self.pos >= self.data.len;
        }
    };
}

pub fn box(allocator: Allocator, value: anytype) Allocator.Error!*@TypeOf(value) {
    const boxed = try allocator.create(@TypeOf(value));
    boxed.* = value;
    return boxed;
}

pub fn push(comptime T: type, allocator: Allocator, slice: []T, value: T) Allocator.Error![]T {
    var new = try allocator.realloc(slice, slice.len+1);
    new[new.len-1] = value;
    return new;
}
