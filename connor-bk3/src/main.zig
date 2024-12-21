const std = @import("std");
const cwd = std.fs.cwd;
const GeneralPurposeAllocator = std.heap.GeneralPurposeAllocator;
const ArenaAllocator = std.heap.ArenaAllocator;
const tokenizer = @import("token.zig").tokenizer;
const parser = @import("ast.zig").parser;
const checker = @import("check.zig").checker;
const statementer = @import("ir.zig").statementer;
const assembler = @import("asm.zig").assembler;
const types = @import("types.zig");
const Token = types.Token;
const Ast = types.Ast;

pub fn main() !void {
    var gpa = GeneralPurposeAllocator(.{}){};
    var arena = ArenaAllocator.init(gpa.allocator());
    const allocator = arena.allocator();
    defer _ = arena.reset(.free_all);

    const file = try cwd().openFile("input.con", .{});
    const input = try file.readToEndAlloc(allocator, 1024*1024);
    defer file.close();

    const tokens = try tokenizer(input, allocator);
    //for (tokens) |token|
    //    std.debug.print("{any}\n", .{token});

    const asts = try parser(tokens, allocator);
    checker(asts, allocator);
    //for (asts) |ast| {
    //    ast.print(0);
    //    std.debug.print("\n", .{});
    //}

    const ir_functions = try statementer(asts, allocator);
    for (ir_functions) |ir_function| {
        std.debug.print("Function: {s}, {s}\n", .{ir_function.name, @tagName(ir_function.ty)});
        for (ir_function.codes) |code|
            code.print();
        std.debug.print("\n", .{});
    }

    try assembler(ir_functions, allocator);
}
