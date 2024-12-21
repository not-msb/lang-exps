const std = @import("std");
const Allocator = std.mem.Allocator;
const Token = @import("token.zig").Token;
const Type = @import("token.zig").Type;
const Cursor = @import("tools.zig").Cursor;
const Vec = @import("tools.zig").Vec;
const box = @import("tools.zig").box;
const Context = @import("ctx.zig").Context;

pub const Node = union(enum) {
    pub const Attr_t = struct {
        exported: bool,
    };

    const Call_t = struct {
        f: *Ast,
        expr: *Ast,
    };

    const Function_t = struct {
        name: []const u8,
        params: []const []const u8,
        exprs: []Ast,
        attr: Attr_t,
    };

    Integer: usize,
    Identifier: []const u8,
    Tuple: []Ast,
    Call: Call_t,
    Function: Function_t,
};

pub const Ast = struct {
    node: Node,
    ty: Type,

    pub fn parse(allocator: Allocator, input: []const Token) Allocator.Error![]Ast {
        var context = Context.init(allocator);
        defer context.deinit();

        try context.insert("add_u8", .{ .Function = .{
            .params = &[_]Type{.U8},
            .ret = &Type{ .Function = .{
                .params = &[_]Type{.U8},
                .ret = &.U8,
            } },
        } });

        var cursor = Cursor(Token).new(input);
        var exprs = try Vec(Ast).new(allocator);

        while (try Ast.p_function(&context, &cursor)) |expr| {
            try exprs.push(expr);
        }

        return exprs.slice();
    }

    fn p_tuple(ctx: *Context, cursor: *Cursor(Token)) Allocator.Error!?Ast {
        var exprs = try Vec(Ast).new(ctx.allocator);
        const pos = cursor.save();

        b: {
            if (cursor.next()) |token| (if (token != .LParen) break :b) else break :b;

            while (try Ast.p_expr(ctx, cursor)) |expr| {
                try exprs.push(expr);
                if (cursor.curr()) |token| if (token == .Comma) cursor.inc() else break;
            }

            if (cursor.next()) |token| (if (token != .RParen) break :b) else break :b;

            const types = try exprs.map(Type, Ast.getType);
            return .{ .node = .{ .Tuple = exprs.slice() }, .ty = .{ .Tuple = types.slice() } };
        }

        cursor.reset(pos);
        return null;
    }

    fn p_expr(ctx: *Context, cursor: *Cursor(Token)) Allocator.Error!?Ast {
        const token = cursor.next() orelse return null;
        return switch (token) {
            .Integer => |int| .{ .node = .{ .Integer = int }, .ty = .CompInt },
            .Word => |word| {
                var ty = ctx.types.get(word).?;
                var root: Ast = .{ .node = .{ .Identifier = word }, .ty = ty };
                while (ty == .Function) {
                    const tuple = ty.Function;
                    ty = tuple.ret.*;
                    if (try Ast.p_expr(ctx, cursor)) |expr| {
                        root = .{
                            .node = .{
                                .Call = .{
                                    .f = try box(ctx.allocator, root),
                                    .expr = try box(ctx.allocator, expr),
                                },
                            },
                            .ty = ty,
                        };
                    } else break;
                }

                return root;
            },
            else => {
                cursor.dec();
                if (try Ast.p_tuple(ctx, cursor)) |expr| return expr;
                return null;
            },
        };
    }

    fn p_statement(ctx: *Context, cursor: *Cursor(Token)) Allocator.Error!?Ast {
        const pos = cursor.save();

        if (try Ast.p_expr(ctx, cursor)) |expr| {
            if (cursor.next()) |token| if (token == .SemiColon) return expr;
        }

        cursor.reset(pos);
        return null;
    }

    fn p_param(ctx: *Context, cursor: *Cursor(Token)) Allocator.Error!?[]const u8 {
        const pos = cursor.save();

        if (cursor.next()) |token1| if (token1 == .Type)
            if (cursor.next()) |token2| if (token2 == .Word) {
                const ty = token1.Type;
                const name = token2.Word;

                try ctx.insert(name, ty);
                return name;
            };

        cursor.reset(pos);
        return null;
    }

    fn p_params(ctx: *Context, cursor: *Cursor(Token)) Allocator.Error!?[][]const u8 {
        var names = try Vec([]const u8).new(ctx.allocator);
        const pos = cursor.save();

        b: {
            if (cursor.next()) |token| (if (token != .LParen) break :b) else break :b;

            while (try Ast.p_param(ctx, cursor)) |name| {
                try names.push(name);
                if (cursor.curr()) |token| if (token == .Comma) cursor.inc() else break;
            }

            if (cursor.next()) |token| (if (token != .RParen) break :b) else break :b;

            return names.slice();
        }

        cursor.reset(pos);
        return null;
    }

    fn p_block(ctx: *Context, cursor: *Cursor(Token)) Allocator.Error!?[]Ast {
        var exprs = try Vec(Ast).new(ctx.allocator);
        const pos = cursor.save();

        b: {
            if (cursor.next()) |token| (if (token != .LBracket) break :b) else break :b;

            while (try Ast.p_statement(ctx, cursor)) |expr| {
                try exprs.push(expr);
            }

            if (cursor.next()) |token| (if (token != .RBracket) break :b) else break :b;

            return exprs.slice();
        }

        cursor.reset(pos);
        return null;
    }

    fn p_function(ctx: *Context, cursor: *Cursor(Token)) Allocator.Error!?Ast {
        const pos = cursor.save();

        b: {
            var exported = false;
            if (cursor.curr()) |token| {
                if (token == .Word and std.mem.eql(u8, token.Word, "export")) {
                    exported = true;
                    cursor.inc();
                }
            } else break :b;

            if (cursor.next()) |token1| if (token1 == .Type)
                if (cursor.next()) |token2| if (token2 == .Word) {
                    const ty = token1.Type;
                    const word = token2.Word;

                    // Context clone
                    var context = try ctx.clone();
                    defer context.deinit();
                    try context.insert("return", .{ .Function = .{
                        .params = &[1]Type{ty},
                        .ret = &.NoReturn,
                    } });

                    // Finish Parsing
                    const params = try Ast.p_params(&context, cursor) orelse break :b;
                    const exprs = try Ast.p_block(&context, cursor) orelse break :b;
                    for (exprs) |*e|
                        e.unComp(null);

                    // Old Context update
                    const param_types = try Vec([]const u8).fromSlice(ctx.allocator, params).mapWith(Type, &context, Context.getUnsafe);
                    const f_ty = .{ .Function = .{ .params = param_types.slice(), .ret = try box(ctx.allocator, ty) } };
                    try ctx.insert(word, f_ty);

                    // Returning
                    const expr = Ast{
                        .node = .{ .Function = .{
                            .name = word,
                            .params = params,
                            .exprs = exprs,
                            .attr = .{
                                .exported = exported,
                            },
                        } },
                        .ty = f_ty,
                    };

                    _ = expr.check();
                    return expr;
                };
        }

        cursor.reset(pos);
        return null;
    }

    fn getType(self: Ast) Type {
        return self.ty;
    }

    pub fn check(self: Ast) void {
        return switch (self.node) {
            .Integer, .Identifier => {},
            .Tuple => |exprs| for (exprs, 0..) |expr, i| {
                expr.check();
                if (!expr.ty.coercible(&self.ty.Tuple[i])) @panic("Incompatible types in tuple");
            },
            .Call => |tuple| {
                tuple.f.check();
                tuple.expr.check();

                const params = tuple.f.ty.Function.params;
                const ty = tuple.expr.ty;

                if (!ty.coercible(&.{ .Tuple = params })) @panic("Incompatible types in call");
            },
            .Function => |tuple| for (tuple.exprs) |expr|
                expr.check(),
        };
    }

    pub fn unComp(self: *Ast, expected: ?Type) void {
        switch (self.node) {
            .Integer, .Identifier => {
                if (expected) |ty| self.ty = ty;
            },
            .Tuple => |exprs| for (0..exprs.len) |i|
                exprs[i].unComp(self.ty.Tuple[i]),
            .Call => |tuple| {
                const params = tuple.f.ty.Function.params;
                tuple.expr.unComp(if (params.len == 0) null else params[0]);
                tuple.f.unComp(null);
            },
            .Function => |tuple| for (0..tuple.exprs.len) |i|
                tuple.exprs[i].unComp(null),
        }
    }

    // Debug only
    //pub fn print(self: Ast, level: usize) void {
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
    //            p("Call:\n", .{});
    //            tuple.f.print(level + 1);
    //            tuple.expr.print(level + 1);
    //        },
    //        .Function => |tuple| {
    //            for (0..level) |_| p("    ", .{});
    //            p("Function: {s}\n", .{tuple.name});
    //            for (tuple.exprs) |expr|
    //                expr.print(level + 1);
    //        },
    //    }

    //    for (0..level) |_| p("    ", .{});
    //    std.debug.print("Type: {}\n", .{self.ty});
    //}
};
