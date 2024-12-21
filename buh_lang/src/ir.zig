const std = @import("std");
const Allocator = std.mem.Allocator;
const ArrayList = std.ArrayList;

const Ast = @import("ast.zig").Ast;
const Value = @import("main.zig").Value;
const Context = @import("ctx.zig").Context;
const Graph = @import("main.zig").Graph;

pub const Ir = union(enum) {
    Assign: struct {
        dst: Val,
        src: Val,
    },
    Jnz: struct {
        cond: Val,
        ctrue: usize,
        cfalse: usize,
    },
    Jmp: usize,
    Call: struct {
        dst: Val,
        name: Val,
        args: []const Val,
    },
    CallC: struct {
        dst: Val,
        name: Val,
        args: []const Val,
    },
    Asm: Ast.Asm,
    Ret: Val,

    pub const Val = union(enum) {
        Integer: usize,
        Identifier: []const u8,
        Temporary: usize,
        // Sadly this one *v* added
        Fixed: Value,

        // I cant anymore T_T
        pub var tmp_graph: Graph = undefined;
        pub var tmp_node_index: usize = undefined;
        pub var tmp_instr_offset: usize = undefined;
        //pub var tmp_lifetime_offset: []const Ir = undefined;

        fn lifetime(self: Val) ?usize {
            const old_index = tmp_node_index;
            const old_offset = tmp_instr_offset;
            defer {
                tmp_node_index = old_index;
                tmp_instr_offset = old_offset;
            }

            var time: ?usize = switch (self) {
                .Integer => return null,
                else => null,
            };

            const node = tmp_graph.nodes.items[tmp_node_index].items[tmp_instr_offset..];

            for (node, 0..) |instr, i| switch (instr) {
                .Assign => |t| {
                    if (self.eql(t.dst) or self.eql(t.src))
                        time = i;
                },
                .Jnz => |t| {
                    if (self.eql(t.cond))
                        time = i;

                    tmp_instr_offset = 0;
                    tmp_node_index = t.ctrue;
                    const ttime = self.lifetime();
                    tmp_node_index = t.cfalse;
                    const ftime = self.lifetime();
                    { tmp_node_index = old_index; tmp_instr_offset = old_offset; }

                    time = if (ttime == null and ftime == null)
                        time
                    else if (ttime) |tt|
                        i + tt
                    else if (ftime) |ft|
                        i + ft
                    else
                        i + @max(ttime.?, ftime.?);
                },
                .Jmp => |v| {
                    tmp_instr_offset = 0;
                    tmp_node_index = v;
                    const jtime = self.lifetime();
                    { tmp_node_index = old_index; tmp_instr_offset = old_offset; }

                    if (jtime) |jt|
                        time = i + jt;
                },
                .Call => |t| {
                    if (self.eql(t.dst) or self.eql(t.name))
                        time = i;
                    for (t.args) |arg| {
                        if (self.eql(arg)) time = i;
                    }
                },
                .CallC => |t| {
                    if (self.eql(t.dst) or self.eql(t.name))
                        time = i;
                    for (t.args) |arg| {
                        if (self.eql(arg)) time = i;
                    }
                },
                .Ret => |v| {
                    if (self.eql(v))
                        time = i;
                },
                .Asm => |t| {
                    for (t.regs) |reg| {
                        const val = .{ .Fixed = .{ .register = reg } };
                        if (self.eql(val)) time = i;
                    }

                    const out = t.out orelse continue;
                    const dst = .{ .Fixed = .{ .register = out } };
                    if (self.eql(dst))
                        time = i;
                },
            };

            return time;
        }

        fn eql(self: Val, other: Val) bool {
            return switch (self) {
                .Identifier => |v| other == .Identifier and std.mem.eql(u8, v, other.Identifier),
                .Fixed => |v| other == .Fixed and v.eql(other.Fixed),
                else => std.meta.eql(self, other),
            };
        }

        fn alloc(self: Val, ctx: *Context) Allocator.Error!Value {
            const allocator = ctx.symbols.allocator;
            //std.debug.print("VAL: {s}\n", .{@tagName(self)});

            switch (self) {
                .Integer => |v| return .{ .integer = v },
                .Identifier => |v| {
                    if (ctx.get(v)) |val|
                        return val;

                    //@panic("Nope, ctx must contain this");
                    const time = self.lifetime().?;
                    //if (time == 0) return undefined;
                    if (time == 0) return .{ .integer = 9999 }; // For debugging

                    const val = Value.new(ctx, time).?;
                    try ctx.put(v, val);
                    return val;
                },
                .Temporary => |v| {
                    const fmt = try std.fmt.allocPrint(allocator, "@{d}", .{v});
                    //std.debug.print("TEMPORARY: {s}\n", .{fmt});
                    if (ctx.get(fmt)) |val| {
                        //std.debug.print("  => found: {s}\n", .{fmt});
                        return val;
                    }

                    const time = self.lifetime().?;
                    //if (time == 0) return undefined;
                    if (time == 0) return .{ .integer = 9999 }; // For debugging

                    //std.debug.print("  => added: {s}\n", .{fmt});
                    const val = Value.new(ctx, time).?;
                    try ctx.put(fmt, val);
                    return val;
                },
                .Fixed => |v| return v,
            }
        }
    };

    pub const Ctx = struct {
        temp_count: usize = 0,

        pub fn newTemp(self: *Ir.Ctx) Val {
            self.temp_count += 1;
            return .{ .Temporary = self.temp_count-1 };
        }
    };

    pub fn format(self: Ir, ctx: *Context) Allocator.Error![]const u8 {
        const allocator = ctx.symbols.allocator;
        switch (self) {
            .Assign => |t| {
                if (t.dst.lifetime() == 0)
                    return "; IDK what to put here (useless assign)\n";

                const dst = try t.dst.alloc(ctx);
                const src = try t.src.alloc(ctx);

                return std.fmt.allocPrint(allocator, "\tmov  {s}, {s}\n", .{
                    try dst.format(allocator),
                    try src.format(allocator),
                });
            },
            .Jnz => |t| {
                const cond = try t.cond.alloc(ctx);

                return std.fmt.allocPrint(allocator, "\tcmp  {s}, 0\n\tjnz  .L{d}\n\tjmp  .L{d}\n", .{
                    try cond.format(allocator),
                    t.ctrue,
                    t.cfalse,
                });
            },
            .Jmp => |target| {
                return std.fmt.allocPrint(allocator, "\tjmp  .L{d}\n", .{
                    target,
                });
            },
            .Call => |t| {
                var list = ArrayList(u8).init(allocator);
                const dst  = switch (t.dst.lifetime().?) {
                    0 => "; IDK what to put here (useless call assign)\n",
                    else => try std.fmt.allocPrint(allocator, "\tmov  {s}, [-{d}+rsp]\n", .{
                        try (try t.dst.alloc(ctx)).format(allocator),
                        8+8*ctx.env.stack,
                    }),
                };
                const name = try t.name.alloc(ctx);
                const call_stack = try ctx.prepareCallStack(t.args.len);
                for (t.args, call_stack) |arg, loc| {
                    const a = try arg.alloc(ctx);
                    try list.writer().print("\tmov  {s}, {s}\n", .{try loc.format(allocator), try a.format(allocator)});
                }

                return std.fmt.allocPrint(allocator, "{s}\tsub  rsp, {d}\n\tcall {s}\n\tadd  rsp, {d}\n{s}", .{
                    try list.toOwnedSlice(),
                    8+8*ctx.env.stack,
                    try name.format(allocator),
                    8+8*ctx.env.stack,
                    //try dst.format(allocator),
                    dst,
                    //8*ctx.env.stack-8*t.args.len+8,
                });
            },
            .CallC => |t| {
                var list = ArrayList(u8).init(allocator);
                const name = try t.name.alloc(ctx);
                const call_stack = ctx.prepareCCallStack(t.args.len);

                for (t.args, call_stack) |arg, loc| {
                    const a = try arg.alloc(ctx);
                    try list.writer().print("\tmov  {s}, {s}\n", .{try loc.format(allocator), try a.format(allocator)});
                }

                return std.fmt.allocPrint(allocator, "{s}\tsub  rsp, {d}\n\tcall {s}\n\tadd  rsp, {d}\n", .{
                    try list.toOwnedSlice(),
                    8+8*ctx.env.stack,
                    try name.format(allocator),
                    8+8*ctx.env.stack,
                });
            },
            .Ret => |v| {
                const a = try v.alloc(ctx);
                return std.fmt.allocPrint(allocator, "\tmov  {{d}}, {s}\n{{s}}\tret\n", .{
                    try a.format(allocator),
                });
            },
            .Asm => |lit_asm| {
                var fmt = lit_asm.text;
                var i: usize = 0;

                while (std.mem.indexOfPos(u8, fmt, i, "%")) |index| {
                    i = index;
                    var j = index + 1;
                    while (true) switch (fmt[j]) {
                        'a'...'z', 'A'...'Z', '0'...'9' => j += 1, // This does *technically* allow vars starting with a number
                        else => break,
                    };

                    const val = ctx.get(fmt[index+1..j]).?;
                    fmt = try std.mem.replaceOwned(u8, allocator, fmt, fmt[index..j], try val.format(allocator));
                }

                if (std.mem.containsAtLeast(u8, fmt, 1, "@out")) {
                    const out = lit_asm.out orelse @panic("yeah no, you need to give an output register when using @out");
                    const val = Value{ .register = out };
                    fmt = try std.mem.replaceOwned(u8, allocator, fmt, "@out", try val.format(allocator));
                }

                return fmt;
            },
        }
    }
};
