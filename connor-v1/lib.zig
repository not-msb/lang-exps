const std = @import("std");

pub extern fn main() u64;

export const x: u64 = 10;

export fn deref(ptr: *const u64) u64 {
    return ptr.*;
}

export fn dump(n: u64) u64 {
    std.debug.print("{d}\n", .{n});
    return 0;
}

export fn dumpByte(n: u8) u8 {
    std.debug.print("{d}\n", .{n});
    return 0;
}
