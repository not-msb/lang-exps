const std = @import("std");
const lib = @import("lib.zig");
const cwd = std.fs.cwd;
const ArrayList = std.ArrayList;
const Allocator = std.mem.Allocator;
const Token = lib.Token;
const File = lib.File;
const Ast = lib.Ast;
const Type = lib.Type;
const Storage = lib.Storage;
const IrFile = lib.IrFile;
const Ir = lib.Ir;
const Context = lib.Context;
const box = lib.box;

fn preprocess(allocator: Allocator, input: []const u8) Allocator.Error![]const u8 {
    var output = ArrayList(u8).init(allocator);
    var i: usize = 0;

    while (i < input.len) {
        const start = i;
        while (i < input.len and !std.mem.startsWith(u8, input[i..], "//")) : (i += 1) {}
        const end = i;

        while (i < input.len and input[i] != '\n') : (i += 1) {}
        try output.appendSlice(input[start..end]);
    }

    return output.toOwnedSlice();
}

pub fn main() !void {
    var gpa = std.heap.GeneralPurposeAllocator(.{}){};
    defer if (gpa.deinit() == .leak) @panic("Memory leak detected!");

    if (!box.boxInit(gpa.allocator())) @panic("Couldn't init BOX_ALLOC");
    defer box.boxDeinit();

    var arena = std.heap.ArenaAllocator.init(gpa.allocator());
    defer arena.deinit();
    const allocator = arena.allocator();

    const input = try cwd().readFileAlloc(allocator, "main.con", std.math.maxInt(usize));
    const processed = try preprocess(allocator, input);

    var context = Context.init(allocator);
    const file = try File.parse(allocator, &context, processed);
    try Context.scan(file);

    //_ = file;
    //{
    //    var iter = context.symbols.iterator();
    //    while (iter.next()) |entry|
    //        try @import("pretty.zig").print(allocator, entry, .{});
    //}
    //const context = try Context.scan(allocator, file);
    //_ = context;

    const ir_file = try IrFile.from(&context, file);
    try ir_file.compile();

    std.debug.print("Arena:     {d}\n", .{arena.queryCapacity()});
    std.debug.print("Type:      {d}\n", .{@sizeOf(Type)});
    std.debug.print("Storage:   {d}\n", .{@sizeOf(Storage)});
    std.debug.print("Context:   {d}\n", .{@sizeOf(Context)});
    std.debug.print("Token:     {d}\n", .{@sizeOf(Token)});
    std.debug.print("Ast:       {d}\n", .{@sizeOf(Ast)});
    std.debug.print("Ir:        {d}\n", .{@sizeOf(Ir)});
}
