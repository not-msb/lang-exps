const std = @import("std");
const Allocator = std.mem.Allocator;
const Token = @import("token.zig").Token;
const Type = @import("token.zig").Type;
const Cursor = @import("tools.zig").Cursor;
const box = @import("tools.zig").box;
const push = @import("tools.zig").push;
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

    pub fn parse(ctx: *Context, input: []const Token) Allocator.Error![]const Ast {
        var cursor = Cursor(Token).new(input);
        var exprs = try ctx.allocator.alloc(Ast, 0);

        while (try Ast.p_function(ctx, &cursor)) |expr| {
            exprs = try push(Ast, ctx.allocator, exprs, expr);
        }

        return exprs;
    }

    fn p_tuple(ctx: *Context, cursor: *Cursor(Token)) Allocator.Error!?Ast {
        var exprs = try ctx.allocator.alloc(Ast, 0);
        const pos = cursor.save();

        b: {
            if (cursor.next()) |token| (if (token != .LParen) break :b) else break :b;

            while (try Ast.p_expr(ctx, cursor)) |expr| {
                exprs = try push(Ast, ctx.allocator, exprs, expr);
                if (cursor.curr()) |token| if (token == .Comma) cursor.inc() else break;
            }

            if (cursor.next()) |token| (if (token != .RParen) break :b) else break :b;

            var types = try ctx.allocator.alloc(Type, 0);
            for (exprs) |expr|
                types = try push(Type, ctx.allocator, types, expr.ty);

            return .{ .node = .{ .Tuple = exprs }, .ty = .{ .Tuple = types } };
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
            if (cursor.next()) |token2| if (token2 == .Word)
        {
            const ty = token1.Type;
            const name = token2.Word;

            try ctx.insert(name, ty);
            return name;
        };

        cursor.reset(pos);
        return null;
    }

    fn p_params(ctx: *Context, cursor: *Cursor(Token)) Allocator.Error!?[]const []const u8 {
        var names = try ctx.allocator.alloc([]const u8, 0);
        const pos = cursor.save();

        b: {
            if (cursor.next()) |token| (if (token != .LParen) break :b) else break :b;

            while (try Ast.p_param(ctx, cursor)) |name| {
                names = try push([]const u8, ctx.allocator, names, name);
                if (cursor.curr()) |token| if (token == .Comma) cursor.inc() else break;
            }

            if (cursor.next()) |token| (if (token != .RParen) break :b) else break :b;

            return names;
        }

        cursor.reset(pos);
        return null;
    }


    fn p_block(ctx: *Context, cursor: *Cursor(Token)) Allocator.Error!?[]Ast {
        var exprs = try ctx.allocator.alloc(Ast, 0);
        const pos = cursor.save();

        b: {
            if (cursor.next()) |token| (if (token != .LBracket) break :b) else break :b;

            while (try Ast.p_statement(ctx, cursor)) |expr| {
                exprs = try push(Ast, ctx.allocator, exprs, expr);
            }

            if (cursor.next()) |token| (if (token != .RBracket) break :b) else break :b;

            return exprs;
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
                if (cursor.next()) |token2| if (token2 == .Word)
            {
                const ty = token1.Type;
                const word = token2.Word;

                // Context clone
                var context = try ctx.clone();
                defer context.deinit();
                try context.insert("return", .{ .Function = .{
                    .params = &[1]Type{ ty },
                    .ret = &.NoReturn,
                }});

                // Finish Parsing
                const params = try Ast.p_params(&context, cursor) orelse break :b;
                const exprs = try Ast.p_block(&context, cursor) orelse break :b;
                for (exprs) |*e|
                    context.unComp(e, null);

                // Old Context update
                var param_types = try ctx.allocator.alloc(Type, 0);
                for (params) |param|
                    param_types = try push(Type, ctx.allocator, param_types, context.get(param).?);

                const f_ty = .{ .Function = .{
                    .params = param_types,
                    .ret = try box(ctx.allocator, ty)
                }};
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
                    }},
                    .ty = f_ty,
                };

                _ = try context.check(expr);
                return expr;
            };
        }

        cursor.reset(pos);
        return null;
    }

    pub fn print(self: Ast, level: usize) void {
        const p = std.debug.print;

        switch (self.node) {
            .Integer => |int| {
                for (0..level) |_| p("    ", .{});
                p("{d}\n", .{int});
            },
            .Identifier => |ident| {
                for (0..level) |_| p("    ", .{});
                p("{s}\n", .{ident});
            },
            .Tuple => |exprs| {
                for (0..level) |_| p("    ", .{});
                p("()\n", .{});
                for (exprs) |expr|
                    expr.print(level+1);
            },
            .Call => |tuple| {
                for (0..level) |_| p("    ", .{});
                p("Call:\n", .{});
                tuple.f.print(level + 1);
                tuple.expr.print(level + 1);
            },
            .Function => |tuple| {
                for (0..level) |_| p("    ", .{});
                p("Function: {s}\n", .{tuple.name});
                for (tuple.exprs) |expr|
                    expr.print(level + 1);
            },
        }

        for (0..level) |_| p("    ", .{});
        std.debug.print("Type: {}\n", .{self.ty});
    }
};
