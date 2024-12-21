const std = @import("std");
const Arena = std.heap.ArenaAllocator;
const Alloc = std.mem.Allocator;
const cwd = std.fs.cwd;
const tools = @import("tools");
const push = tools.push;
const append = tools.append;
const filter = tools.filter;
const tokenizer = @import("token.zig").tokenizer;
const parser = @import("ast.zig").parser;
const checker = @import("check.zig").checker;
const statementer = @import("ir.zig").statementer;
const types = @import("types.zig");
const Token = types.Token;
const Statement = types.Statement;
const _asm = @import("asm.zig");
const assemble = _asm.assemble;

const CompileConf = struct {
    show_token: bool = false,
    show_ast: bool = false,
    show_stmt: bool = false,
};

fn preprocess(input: []const u8, alloc: Alloc) Alloc.Error![]const u8 {
    var lines = std.mem.splitSequence(u8, input, "\n");
    var processed = try alloc.alloc(u8, 0);

    outer: while (lines.next()) |line| {
        for (0..line.len) |i| {
            if (std.mem.startsWith(u8, line[i..], "//")) {
                try append(u8, alloc, &processed, line[0..i]);
                try push(u8, alloc, &processed, '\n');
                continue :outer;
            }
        }

        try append(u8, alloc, &processed, line);
        try push(u8, alloc, &processed, '\n');
    }

    return processed;
}

fn compile(comptime conf: CompileConf, alloc: Alloc, input: []const u8) !void {
    const tokens_result = try tokenizer().parse(input, alloc);
    const tokens = try filter(Token, alloc, tokens_result.o, struct {
        fn f(value: Token) bool {
            return value != Token.whitespace;
        }
    }.f);

    if (conf.show_token)
        for (tokens) |token| {
            std.debug.print("{any}\n", .{token});
        };

    const asts_result = try parser().parse(tokens, alloc);
    const asts = asts_result.o;
    checker(alloc, asts);

    if (conf.show_ast)
        for (asts) |ast| {
            ast.print(alloc, "", "", "");
            std.debug.print("\n", .{});
        };

    var stmts = try statementer(alloc, asts);
    var opt_stmts = try alloc.alloc(Statement, 0);
    for (stmts) |*stmt|
        if (stmt.optimize()) |opt|
            try push(Statement, alloc, &opt_stmts, opt.*);

    if (conf.show_stmt)
        for (opt_stmts) |opt_stmt| {
            opt_stmt.print(alloc);
            std.debug.print("\n", .{});
        };

    for (opt_stmts) |opt_stmt|
        assemble(opt_stmt);
}

pub fn main() !void {
    var buffer: [128 * 1024]u8 = undefined;
    var fba = std.heap.FixedBufferAllocator.init(&buffer);
    var arena = Arena.init(fba.allocator());
    const alloc = arena.allocator();
    defer _ = arena.reset(.free_all);

    const file = try cwd().openFile("input.nor", .{});
    const input = try file.readToEndAlloc(alloc, buffer.len);
    const processed = try preprocess(input, alloc);
    defer file.close();

    try compile(.{ .show_ast = true, .show_stmt = true }, alloc, processed);
}
