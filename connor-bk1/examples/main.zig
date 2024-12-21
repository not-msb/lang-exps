const std = @import("std");

pub fn main() void {
    const s1: []const u8 = "1234567890";
    const s2 = s1[1..];
    const s3 = s2.*;

    std.debug.print("{}, {}, {}\n", .{@TypeOf(s1), @sizeOf(@TypeOf(s1)), @typeInfo(@TypeOf(s1))});
    std.debug.print("{}, {}, {}\n", .{@TypeOf(s2), @sizeOf(@TypeOf(s2)), @typeInfo(@TypeOf(s2))});
    std.debug.print("{}, {}, {}\n", .{@TypeOf(s3), @sizeOf(@TypeOf(s3)), @typeInfo(@TypeOf(s3))});
}
