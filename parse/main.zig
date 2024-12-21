const std = @import("std");
const lib = @import("lib.zig");
const cwd = std.fs.cwd;
const Allocator = std.mem.Allocator;
const Token = lib.Token;
const Ast = lib.Ast;
const Cursor = lib.Cursor;
const Cursor2 = @import("cursor2.zig").Cursor;

pub fn main() !void {
    var gpa = std.heap.GeneralPurposeAllocator(.{}){};
    var arena = std.heap.ArenaAllocator.init(gpa.allocator());
    const allocator = arena.allocator();

    const filename = "main.con";
    const input = try cwd().readFileAlloc(allocator, filename, std.math.maxInt(usize));
    const input_cursor = Cursor2.new(input);

    const tokens = try Token.parse(allocator, input_cursor);
    //try @import("pretty.zig").print(allocator, tokens, .{});

    const file = Ast.parse(allocator, tokens) catch |err| {
        const loc = lib.global_error;
        var offset: usize = 0;

        for (0..loc.row - 1) |_| {
            while (input[offset] != '\n') offset += 1 else offset += 1;
        }

        var end = offset;
        while (input[end] != '\n') end += 1 else end += 1;

        std.debug.print("{} in {s}:{d}:{d}:\n", .{err, filename, loc.row, loc.col});
        std.debug.print("    {s}", .{input[offset..end]});
        for (0..loc.col - 1) |_|
            std.debug.print(" ", .{});
        std.debug.print("    ^\n", .{});
        return;
    };

    try file.scan(allocator);
    try @import("pretty.zig").print(allocator, file, .{ .max_depth = 100 });

    //try res1.dead(allocator);
}
