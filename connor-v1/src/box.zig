const std = @import("std");
const lib = @import("lib.zig");
const Allocator = std.mem.Allocator;
const ArenaAllocator = std.heap.ArenaAllocator;

// This file uses a global allocator to provide a smart-pointer without any overhead
var BOX_ALLOC: ?ArenaAllocator = null;

pub fn boxInit(allocator: Allocator) bool {
    if (BOX_ALLOC) |_| return false;
    BOX_ALLOC = ArenaAllocator.init(allocator);
    return true;
}

pub fn boxDeinit() void {
    if (BOX_ALLOC) |box| box.deinit();
}

pub fn Box(comptime T: type) type {
    return struct {
        ptr: *T,

        const Self = Box(T);

        fn new() Allocator.Error!Self {
            return .{ .ptr = try BOX_ALLOC.?.allocator().create(T) };
        }

        pub fn init(t: T) Allocator.Error!Self {
            const box = try Self.new();
            box.ptr.* = t;
            return box;
        }

        pub fn deinit(self: Self) void {
            BOX_ALLOC.?.destroy(self.ptr);
        }

        pub fn clone(self: Self) Allocator.Error!Self {
            const box = try Self.new();
            box.ptr.* = self.ptr.*;
            return box;
        }

        pub fn deref(self: Self) T {
            return self.ptr.*;
        }
    };
}
