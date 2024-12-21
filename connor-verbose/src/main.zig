const std = @import("std");
const cwd = std.fs.cwd;
const Allocator = std.mem.Allocator;
const Token = @import("token.zig").Token;
const Type = @import("token.zig").Type;
const Node = @import("ast.zig").Node;
const Ast = @import("ast.zig").Ast;
const PostAst = @import("post.zig").PostAst;

// Removes all single-line comments like this one
fn preprocess(allocator: Allocator, input: []const u8) Allocator.Error![]const u8 {
    var output = try allocator.alloc(u8, 0);

    var i: usize = 0;
    while (i < input.len) : (i += 1) {
        var len: usize = 0;
        while (i + len < input.len and !std.mem.startsWith(u8, input[i + len ..], "//")) : (len += 1) {}
        output = try allocator.realloc(output, output.len + len);
        @memcpy(output[output.len - len ..], input[i .. i + len]);

        i += len;
        while (i < input.len and input[i] != '\n') : (i += 1) {}
    }

    return output;
}

pub fn main() !void {
    var gpa = std.heap.GeneralPurposeAllocator(.{}){};
    defer if (gpa.deinit() == .leak) @panic("Memory leak detected!");

    var arena = std.heap.ArenaAllocator.init(gpa.allocator());
    defer arena.deinit();
    const allocator = arena.allocator();

    const input = try cwd().readFileAlloc(allocator, "main.con", std.math.maxInt(usize));
    defer allocator.free(input);

    const processed = try preprocess(allocator, input);
    defer allocator.free(processed);

    const tokens = try Token.parse(allocator, processed);
    defer allocator.free(tokens);

    //for (tokens) |token| {
    //    std.debug.print("Token: {}\n", .{token});
    //}

    const exprs = try Ast.parse(allocator, tokens);
    const post_exprs = try PostAst.fromAsts(allocator, exprs);

    //for (exprs) |expr| {
    //    expr.print(0);
    //}

    var post_ctx = .{
        .allocator = allocator,
    };
    for (post_exprs) |post_expr|
        _ = try post_expr.compile(&post_ctx);

    std.debug.print("Memory Usage: {d}\n", .{arena.queryCapacity()});
    std.debug.print("SizeOf Token:\t{d}\n", .{ @sizeOf(Token) });
    std.debug.print("SizeOf Node:\t{d}\n", .{ @sizeOf(Node) });
    std.debug.print("SizeOf Type:\t{d}\n", .{ @sizeOf(Type) });
    std.debug.print("SizeOf Ast:\t{d}\n", .{ @sizeOf(Ast) });
    std.debug.print("SizeOf PostAst:\t{d}\n", .{ @sizeOf(PostAst) });
}
