const std = @import("std");
const Alloc = std.mem.Allocator;
const Arena = std.heap.ArenaAllocator;

pub fn push(comptime T: type, alloc: Alloc, slice: *[]T, value: T) Alloc.Error!void {
    slice.* = try alloc.realloc(slice.*, slice.*.len + 1);
    slice.*[slice.len - 1] = value;
}

pub fn append(comptime T: type, alloc: Alloc, dst: *[]T, src: []const T) Alloc.Error!void {
    const len = dst.len;
    dst.* = try alloc.realloc(dst.*, len + src.len);
    @memcpy(dst.*[len..], src);
}

pub fn repeat(comptime T: type, alloc: Alloc, f: fn() T, length: usize) Alloc.Error![]T {
    var ptr = try alloc.alloc(T, length);

    for (0..length) |i| {
        ptr[i] = f();
    }

    return ptr;
}

pub fn map(comptime T: type, comptime G: type, alloc: Alloc, slice: []const T, f: fn(T) G) Alloc.Error![]G {
    var ptr = try alloc.alloc(G, slice.len);

    for (slice, 0..) |item, i| {
        ptr[i] = f(item);
    }

    return ptr;
}

pub fn mapAlloc(comptime T: type, comptime G: type, alloc: Alloc, slice: []const T, f: fn(Alloc, T) G) Alloc.Error![]G {
    var ptr = try alloc.alloc(G, slice.len);

    for (slice, 0..) |item, i| {
        ptr[i] = f(alloc, item);
    }

    return ptr;
}

pub fn filter(comptime T: type, alloc: Alloc, slice: []const T, f: fn (T) bool) Alloc.Error![]T {
    var ptr = try alloc.alloc(T, slice.len);

    var count: usize = 0;
    for (slice) |item| {
        if (f(item)) {
            ptr[count] = item;
            count += 1;
        }
    }

    ptr = try alloc.realloc(ptr, count);
    return ptr;
}

pub fn boxUnsafe(alloc: Alloc, value: anytype) *@TypeOf(value) {
    return box(alloc, value) catch unreachable;
}

pub fn box(alloc: Alloc, value: anytype) Alloc.Error!*@TypeOf(value) {
    const ptr = try alloc.create(@TypeOf(value));
    ptr.* = value;
    return ptr;
}
