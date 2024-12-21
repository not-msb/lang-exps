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

pub fn Vec(comptime T: type) type {
    return struct {
        allocator: Allocator,
        data: []T,
        len: usize,

        const Self = @This();

        pub fn new(allocator: Allocator) Allocator.Error!Self {
            return .{
                .allocator = allocator,
                .data = try allocator.alloc(T, 1),
                .len = 0,
            };
        }

        pub fn newCapacity(allocator: Allocator, length: usize) Allocator.Error!Self {
            return .{
                .allocator = allocator,
                .data = try allocator.alloc(T, length),
                .len = 0,
            };
        }

        // Source must be allocated on the heap, with the provided allocator
        pub fn fromSlice(allocator: Allocator, source: []T) Self {
            return .{
                .allocator = allocator,
                .data = source,
                .len = source.len,
            };
        }

        pub fn slice(self: Self) []T {
            return self.data[0..self.len];
        }

        pub fn push(self: *Self, value: T) Allocator.Error!void {
            const cap = self.data.len;
            if (self.len == cap)
                self.data = try self.allocator.realloc(self.data, 2 * self.len);

            self.data[self.len] = value;
            self.len += 1;
        }

        fn pushUnsafe(self: *Self, value: T) void {
            self.data[self.len] = value;
            self.len += 1;
        }

        pub fn map(self: *const Self, comptime G: type, func: fn (T) G) Allocator.Error!Vec(G) {
            var new_vec = try Vec(G).newCapacity(self.allocator, self.len);
            for (self.slice()) |item|
                new_vec.pushUnsafe(func(item));
            return new_vec;
        }

        pub fn mapAlloc(self: *const Self, comptime G: type, func: fn (Allocator, T) Allocator.Error!G) Allocator.Error!Vec(G) {
            var new_vec = try Vec(G).newCapacity(self.allocator, self.len);
            for (self.slice()) |item|
                new_vec.pushUnsafe(try func(self.allocator, item));
            return new_vec;
        }

        pub fn mapWith(self: *const Self, comptime G: type, value: anytype, func: fn (@TypeOf(value), T) G) Allocator.Error!Vec(G) {
            var new_vec = try Vec(G).newCapacity(self.allocator, self.len);
            for (self.slice()) |item|
                new_vec.pushUnsafe(func(value, item));
            return new_vec;
        }
    };
}

pub fn box(allocator: Allocator, value: anytype) Allocator.Error!*@TypeOf(value) {
    const boxed = try allocator.create(@TypeOf(value));
    boxed.* = value;
    return boxed;
}
