const std = @import("std");
const Alloc = std.mem.Allocator;

pub fn Vec(comptime T: type) type {
    return struct {
        alloc: Alloc,
        slice: []T,
        len: usize,

        const Self = @This();

        pub fn init(alloc: Alloc) Alloc.Error!Self {
            var slice = try alloc.alloc(T, 0);
            return .{ .alloc = alloc, .slice = slice, .len = 0 };
        }

        pub fn initCapacity(alloc: Alloc, capacity: usize) Alloc.Error!Self {
            var slice = try alloc.alloc(T, capacity);
            return .{ .alloc = alloc, .slice = slice, .len = 0 };
        }

        pub fn deinit(self: *Self) void {
            self.alloc.free(self.slice);
            self.* = undefined;
        }

        pub fn length(self: Self) usize {
            return self.len;
        }

        pub fn cap(self: Self) usize {
            return self.slice.len;
        }

        pub fn shrinkWrap(self: *Self) Alloc.Error![]T {
            self.slice = try self.alloc.realloc(self.slice, self.len);
            return self.slice;
        }

        pub fn push(self: *Self, value: T) Alloc.Error!usize {
            if (self.cap() <= self.len) {
                self.slice = try self.alloc.realloc(self.slice, 2+2*self.len);
            }

            self.slice[self.len] = value;
            self.len += 1;

            return self.len-1;
        }

        pub fn pushUnsafe(self: *Self, value: T) usize {
            if (self.cap() <= self.len) {
                self.slice = self.alloc.realloc(self.slice, 2+2*self.len) catch unreachable;
            }

            self.slice[self.len] = value;
            self.len += 1;

            return self.len-1;
        }

        pub fn pop(self: *Self) ?T {
            self.len -= 1;
            return self.slice[self.len];
        }
    };
}
