const std = @import("std");
const ArrayList = std.ArrayList;

const Graph = @import("main.zig").Graph;
const Value = @import("main.zig").Value;
const Register = @import("main.zig").Register;

const Ir  = @import("ir.zig").Ir;
const Context = @import("ctx.zig").Context;

pub const Ast = union(enum) {
    Integer: usize,
    Identifier: []const u8,
    If: struct {
        cond: *const Ast,
        lhs: *const Ast,
        rhs: *const Ast,
    },
    Call: struct {
        name: *const Ast,
        args: []const Ast,
    },
    CallC: struct {
        name: *const Ast,
        args: []const Ast,
    },
    Ret: *const Ast,
    Asm: Asm,

    pub const Asm = struct {
        text: []const u8,
        regs: []const Register,
        out: ?Register,
    };

    pub fn flatten(self: Ast, ctx: *Ir.Ctx, graph: *Graph) !Ir.Val {
        const allocator = graph.allocator;

        switch (self) {
            .Integer => |int| {
                const dst = ctx.newTemp();
                const ir = .{ .Assign = .{
                    .dst = dst,
                    .src = .{ .Integer = int },
                } };

                try graph.append(ir);
                return dst;
            },
            .Identifier => |ident| {
                const dst = ctx.newTemp();
                const ir = .{ .Assign = .{
                    .dst = dst,
                    .src = .{ .Identifier = ident },
                } };

                try graph.append(ir);
                return dst;
            },
            .If => |branch| {
                const dst = ctx.newTemp();

                const lnode = try graph.newNode();
                const rnode = try graph.newNode();
                const mnode = try graph.newNode();

                const cond = try branch.cond.flatten(ctx, graph);
                try graph.appendCanon(.{ .Jnz = .{ .cond = cond, .ctrue = lnode, .cfalse = rnode }});

                graph.current = lnode;
                const lhs = try branch.lhs.flatten(ctx, graph);
                try graph.appendCanon(.{ .Assign = .{ .dst = dst, .src = lhs } });
                try graph.appendCanon(.{ .Jmp = mnode });

                graph.current = rnode;
                const rhs = try branch.rhs.flatten(ctx, graph);
                try graph.appendCanon(.{ .Assign = .{ .dst = dst, .src = rhs } });
                try graph.appendCanon(.{ .Jmp = mnode });

                graph.current = mnode;
                return dst;
            },
            .Call => |call| {
                const dst = ctx.newTemp();
                const name = try call.name.flatten(ctx, graph);
                const args = try allocator.alloc(Ir.Val, call.args.len);
                for (call.args, args) |arg, *new_arg|
                    new_arg.* = try arg.flatten(ctx, graph);

                const ir = .{ .Call = .{
                    .dst = dst,
                    .name = name,
                    .args = args,
                } };

                try graph.append(ir);
                return dst;
            },
            .CallC => |call| {
                const dst = Ir.Val{ .Fixed = Value{ .register = .rax } };
                const name = try call.name.flatten(ctx, graph);
                const args = try allocator.alloc(Ir.Val, call.args.len);

                for (call.args, args) |arg, *new_arg|
                    new_arg.* = try arg.flatten(ctx, graph);

                const ir = .{ .CallC = .{
                    .dst = dst,
                    .name = name,
                    .args = args,
                } };

                try graph.append(ir);

                return dst;
            },
            .Ret => |ret| {
                const val = try ret.flatten(ctx, graph);
                const ir = .{ .Ret = val };

                try graph.append(ir);
                return undefined;
            },
            .Asm => |lit_asm| {
                try graph.append(.{ .Asm = lit_asm });
                return if (lit_asm.out) |out|
                    .{ .Fixed = .{ .register = out } }
                else
                    undefined;
            },
        }
    }
};

