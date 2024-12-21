const std = @import("std");
const cwd = std.fs.cwd;
const Allocator = std.mem.Allocator;
const Token = @import("token.zig").Token;
const Type = @import("token.zig").Type;
const Ast = @import("ast.zig").Ast;
const Context = @import("ctx.zig").Context;
const Cursor = @import("tools.zig").Cursor;
const push = @import("tools.zig").push;
const PostAst = @import("post.zig").PostAst;

// Removes all single-line comments like this one
fn preprocess(allocator: Allocator, input: []const u8) Allocator.Error![]const u8 {
    var output = try allocator.alloc(u8, 0);

    var i: usize = 0;
    while (i < input.len) : (i += 1) {
        var len: usize = 0;
        while (i+len < input.len and !std.mem.startsWith(u8, input[i+len..], "//")) : (len += 1) {}
        output = try allocator.realloc(output, output.len+len);
        @memcpy(output[output.len-len..], input[i..i+len]);

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

    var context = Context.init(allocator);
    defer context.deinit();

    try context.insert("add_u8", .{ .Function = .{
        .params = &[_]Type{ .U8 },
        .ret = &Type{ .Function = .{
            .params = &[_]Type { .U8 },
            .ret = &.U8,
        }},
    }});

    const exprs = try Ast.parse(&context, tokens);
    var post_exprs = try allocator.alloc(PostAst, 0);
    for (exprs) |expr| {
        //_ = context.check(expr);
        //expr.print(0);

        const post_expr = try PostAst.fromAst(allocator, expr);
        post_exprs = try push(PostAst, allocator, post_exprs, post_expr);
        //post_expr.print(0);
    }

    //std.debug.print("Context:\n", .{});
    //var entrys = context.types.iterator();
    //while (entrys.next()) |entry| {
    //    std.debug.print("    {s}, {}\n", .{entry.key_ptr.*, entry.value_ptr.*});
    //}

    var post_ctx = .{ .allocator = allocator, };
    for (post_exprs) |post_expr|
        _ = try post_expr.compile(&post_ctx);
}
