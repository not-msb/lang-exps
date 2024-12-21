const std = @import("std");
const stdout = std.io.getStdOut().writer();
const Allocator = std.mem.Allocator;
const Type = @import("token.zig").Type;
const Node = @import("ast.zig").Node;
const Ast = @import("ast.zig").Ast;
const Vec = @import("tools.zig").Vec;
const box = @import("tools.zig").box;

const Storage = union(enum) {
    temp: usize,
    named: []const u8,

    fn fmt(self: Storage, allocator: Allocator) std.fmt.AllocPrintError![]const u8 {
        return switch (self) {
            .temp => |tmp| try std.fmt.allocPrint(allocator, "%t{d}", .{tmp}),
            .named => |name| try std.fmt.allocPrint(allocator, "%{s}", .{name}),
        };
    }
};

const PostContext = struct {
    allocator: Allocator,
    allocated: usize = 0,

    fn alloc(self: *PostContext) usize {
        self.allocated += 1;
        return self.allocated - 1;
    }
};

const PostNode = union(enum) {
    const Attr_t = Node.Attr_t;

    const Call_t = struct {
        name: []const u8,
        expr: *const PostAst,
    };

    const Function_t = struct {
        name: []const u8,
        params: []const []const u8,
        exprs: []const PostAst,
        attr: Attr_t,
    };

    const BinOpKind = enum {
        Add,

        fn fmt(self: BinOpKind) []const u8 {
            return switch (self) {
                .Add => "add",
            };
        }
    };

    const BinOp_t = struct {
        kind: BinOpKind,
        lhs: *const PostAst,
        rhs: *const PostAst,
    };

    Integer: usize,
    Identifier: []const u8,
    Tuple: []const PostAst,
    Call: Call_t,
    Function: Function_t,
    BinOp: BinOp_t,
    Return: *const PostAst,
};

pub const PostAst = struct {
    node: PostNode,
    ty: Type,

    pub fn fromAsts(allocator: Allocator, exprs: []Ast) Allocator.Error![]PostAst {
        const post_exprs = try Vec(Ast).fromSlice(allocator, exprs).mapAlloc(PostAst, PostAst.fromAst);
        return post_exprs.slice();
    }

    pub fn fromAst(allocator: Allocator, expr: Ast) Allocator.Error!PostAst {
        return switch (expr.node) {
            .Integer => |int| .{ .node = .{ .Integer = int }, .ty = expr.ty },
            .Identifier => |ident| .{ .node = .{ .Identifier = ident }, .ty = expr.ty },
            .Tuple => |exprs| {
                const post_exprs = try Vec(Ast).fromSlice(allocator, exprs).mapAlloc(PostAst, PostAst.fromAst);
                return .{ .node = .{ .Tuple = post_exprs.slice() }, .ty = expr.ty };
            },
            .Call => |tuple| {
                switch (tuple.f.node) {
                    .Identifier => |ident| {
                        const e = try PostAst.fromAst(allocator, tuple.expr.*);
                        const node: PostNode =
                            if (std.mem.eql(u8, ident, "return"))
                                .{ .Return = try box(allocator, e) }
                            else .{ .Call = .{
                                .name = ident,
                                .expr = try box(allocator, e),
                            } };

                        return .{
                            .node = node,
                            .ty = expr.ty,
                        };
                    },
                    .Call => |tup| {
                        if (tup.f.node == .Identifier) {
                            const name = tup.f.node.Identifier;

                            var node: ?PostNode = null;

                            if (std.mem.eql(u8, name, "add_u8")) {
                                const lhs = try PostAst.fromAst(allocator, tup.expr.*);
                                const rhs = try PostAst.fromAst(allocator, tuple.expr.*);

                                node = .{ .BinOp = .{
                                    .kind = .Add,
                                    .lhs = try box(allocator, lhs),
                                    .rhs = try box(allocator, rhs),
                                } };
                            }

                            return if (node) |n| .{
                                .node = n,
                                .ty = expr.ty,
                            } else unreachable;
                        }

                        unreachable; //TODO
                    },
                    else => unreachable, // TODO
                }
            },
            .Function => |tuple| {
                const exprs = try Vec(Ast).fromSlice(allocator, tuple.exprs).mapAlloc(PostAst, PostAst.fromAst);
                return .{
                    .node = .{ .Function = .{
                        .name = tuple.name,
                        .params = tuple.params,
                        .exprs = exprs.slice(),
                        .attr = tuple.attr,
                    } },
                    .ty = expr.ty,
                };
            },
        };
    }

    pub fn compile(self: PostAst, ctx: *PostContext) !Storage {
        switch (self.node) {
            .Integer => |int| {
                const tmp = ctx.alloc();
                try stdout.print("\t%t{d} ={s} copy {d}\n", .{ tmp, self.ty.irFmt(), int });
                return .{ .temp = tmp };
            },
            .Identifier => |ident| return .{ .named = ident },
            .Function => |tuple| {
                const params = self.ty.Function.params;
                const ret = self.ty.Function.ret;
                if (tuple.attr.exported) try stdout.print("export ", .{});
                try stdout.print("function {s} ${s}(", .{ ret.irFmt(), tuple.name });
                for (0..params.len) |i|
                    try stdout.print("{s} %{s}, ", .{ params[i].irFmt(), tuple.params[i] });
                try stdout.print(") {{\n@start\n", .{});
                for (tuple.exprs) |expr|
                    _ = try expr.compile(ctx);
                try stdout.print("}}\n", .{});
                return undefined; // Undefined Behavior, on purpose
            },
            .BinOp => |tuple| {
                const tmp = ctx.alloc();
                const l_tmp = try tuple.lhs.compile(ctx);
                const r_tmp = try tuple.rhs.compile(ctx);
                try stdout.print("\t%t{d} ={s} {s} {s}, {s}\n", .{
                    tmp,
                    self.ty.irFmt(),
                    tuple.kind.fmt(),
                    try l_tmp.fmt(ctx.allocator),
                    try r_tmp.fmt(ctx.allocator),
                });
                return .{ .temp = tmp };
            },
            .Return => |expr| {
                const tmp = try expr.compile(ctx);
                try stdout.print("\tret {s}\n", .{try tmp.fmt(ctx.allocator)});
                return undefined; // Undefined Behavior, on purpose
            },
            else => {
                //self.print(0);
                @panic("Couldn't compile");
            },
        }
    }

    // Debug only
    //pub fn print(self: PostAst, level: usize) void {
    //    const p = std.debug.print;

    //    switch (self.node) {
    //        .Integer => |int| {
    //            for (0..level) |_| p("    ", .{});
    //            p("{d}\n", .{int});
    //        },
    //        .Identifier => |ident| {
    //            for (0..level) |_| p("    ", .{});
    //            p("{s}\n", .{ident});
    //        },
    //        .Tuple => |exprs| {
    //            for (0..level) |_| p("    ", .{});
    //            p("()\n", .{});
    //            for (exprs) |expr|
    //                expr.print(level + 1);
    //        },
    //        .Call => |tuple| {
    //            for (0..level) |_| p("    ", .{});
    //            p("Call: {s}\n", .{tuple.name});
    //            tuple.expr.print(level + 1);
    //        },
    //        .Function => |tuple| {
    //            for (0..level) |_| p("    ", .{});
    //            p("Function: {s}\n", .{tuple.name});
    //            for (tuple.exprs) |expr|
    //                expr.print(level + 1);
    //        },
    //        .BinOp => |tuple| {
    //            for (0..level) |_| p("    ", .{});
    //            p("BinOp: {s}\n", .{@tagName(tuple.kind)});
    //            tuple.lhs.print(level + 1);
    //            tuple.rhs.print(level + 1);
    //        },
    //        .Return => |expr| {
    //            for (0..level) |_| p("    ", .{});
    //            p("Return:\n", .{});
    //            expr.print(level + 1);
    //        },
    //    }

    //    for (0..level) |_| p("    ", .{});
    //    std.debug.print("Type: {}\n", .{self.ty});
    //}
};
