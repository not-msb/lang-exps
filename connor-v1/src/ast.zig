const std = @import("std");
const lib = @import("lib.zig");
const panic = std.debug.panic;
const ArrayList = std.ArrayList;
const Allocator = std.mem.Allocator;
const StringArrayHashMap = std.StringArrayHashMap;
const Token = lib.Token;
const Type = lib.Type;
const Context = lib.Context;
const Box = lib.box.Box;
const Cursor = @import("cursor.zig");

fn tokenField(comptime T: @typeInfo(Token).Union.tag_type.?) type {
    const fields = @typeInfo(Token).Union.fields;
    for (fields) |field| {
        if (std.mem.eql(u8, field.name, @tagName(T))) return field.type;
    }
    unreachable;
}

fn take(comptime T: @typeInfo(Token).Union.tag_type.?, cursor: *Cursor) ?tokenField(T) {
    if (cursor.peek() != T) return null;
    return @field(cursor.next(), @tagName(T));
}

pub const File = struct {
    const Function = struct {
        ctx: *Context,
        params: [][]const u8,
        attr: Ast.Attr,
        expr: Ast,
        ty: Type,
    };

    functions: StringArrayHashMap(Function),

    // TODO: Move preprocessor & Context here
    pub fn parse(allocator: Allocator, ctx: *Context, input: []const u8) Allocator.Error!File {
        var cursor = try Token.parse(allocator, input);
        defer cursor.deinit();
        var functions = StringArrayHashMap(Function).init(allocator);

        while (!cursor.done()) {
            if (try Ast._extern(ctx, &cursor)) continue;

            if (try Ast._function(ctx, &cursor)) |t| {
                try functions.put(t.name, t.func);
            } else @panic("Couldn't parse");
        }

        return .{ .functions = functions };
    }
};

pub const Ast = union(enum) {
    const Extern = struct {
        name: []const u8,
        ty: Type,
    };

    pub const Param = struct {
        name: []const u8,
        ty: Type,
    };

    const Params = struct { names: [][]const u8, types: []Type };

    pub const Attr = struct {
        _export: bool = false,
    };

    const Block_t = struct {
        ctx: *Context,
        exprs: []const Ast,
    };

    const Call_t = struct {
        f: Box(Ast),
        exprs: Box([]const Ast),
    };

    pub const BinOpKind = enum {
        Assign,
        Add,
        Sub,
        Mul,
        Eq,

        pub fn fmt(self: BinOpKind, allocator: Allocator, ty: Type) ![]const u8 {
            return switch (self) {
                .Assign => unreachable,
                .Add => "add",
                .Sub => "sub",
                .Mul => "mul",
                .Eq => std.fmt.allocPrint(allocator, "ceq{s}", .{ty.baseFmt()}),
            };
        }
    };

    const If_t = struct {
        cond: Box(Ast),
        lhs: Box(Ast),
        rhs: ?Box(Ast),
    };

    const BinOp_t = struct {
        kind: BinOpKind,
        lhs: Box(Ast),
        rhs: Box(Ast),
    };

    Integer: usize,         //
    Identifier: []const u8, //
    Block: Block_t,
    Call: Call_t,           //
    If: If_t,
    BinOp: BinOp_t,         //
    AddrOf: Box(Ast),
    Return: Box(Ast),       //

    fn _decl(ctx: *Context, cursor: *Cursor) Allocator.Error!?Ast {
        const pos = cursor.save();

        b: {
            const ty = take(.Type, cursor) orelse break :b;
            const name = take(.Word, cursor) orelse break :b;
            _ = take(.Assign, cursor) orelse break :b;
            const e = try expr(ctx, cursor, 0);
            try ctx.put(name, .{ .access = .deref, .scope = .local, .ty = ty });
            return .{ .BinOp = .{
                .kind = .Assign,
                .lhs = try Box(Ast).init(.{ .Identifier = name }),
                .rhs = try Box(Ast).init(e),
            }};
        }

        cursor.reset(pos);
        return null;
    }

    fn _extern_params(cursor: *Cursor) Allocator.Error!?[]Type {
        const pos = cursor.save();

        b: {
            var params = ArrayList(Type).init(cursor.allocator);
            take(.LParen, cursor) orelse break :b;
            while (take(.Type, cursor)) |param| {
                try params.append(param);
                take(.Comma, cursor) orelse break;
            }
            take(.RParen, cursor) orelse break :b;
            return try params.toOwnedSlice();
        }

        cursor.reset(pos);
        return null;
    }

    fn _param(cursor: *Cursor) ?Param {
        const pos = cursor.save();

        b: {
            return .{
                .ty = take(.Type, cursor) orelse break :b,
                .name = take(.Word, cursor) orelse break :b,
            };
        }

        cursor.reset(pos);
        return null;

    }

    fn _params(cursor: *Cursor) Allocator.Error!?Params {
        const pos = cursor.save();

        b: {
            var names = ArrayList([]const u8).init(cursor.allocator);
            var types = ArrayList(Type).init(cursor.allocator);

            take(.LParen, cursor) orelse break :b;
            while (_param(cursor)) |param| {
                try names.append(param.name);
                try types.append(param.ty);
                take(.Comma, cursor) orelse break;
            }
            take(.RParen, cursor) orelse break :b;

            return .{
                .names = try names.toOwnedSlice(),
                .types = try types.toOwnedSlice(),
            };
        }

        cursor.reset(pos);
        return null;
    }

    fn _extern(ctx: *Context, cursor: *Cursor) Allocator.Error!bool {
        take(.Extern, cursor) orelse return false;
        var ty = take(.Type, cursor).?;
        const name = take(.Word, cursor).?;
        // Somehow, I can't inline this, please fix mem issues
        const _ret = try Box(Type).init(ty);
        if (try _extern_params(cursor)) |params|
            ty = .{ .Function = .{
                .params = params,
                .ret = _ret,
            } };
        take(.SemiColon, cursor).?;
        try ctx.put(name, .{ .access = if (ty == .Function) .direct else .deref, .scope = .global, .ty = ty });
        return true;
    }

    fn _function(ctx: *Context, cursor: *Cursor) Allocator.Error!?struct { name: []const u8, func: File.Function } {
        const pos = cursor.save();

        b: {
            const attr = .{
                ._export = take(.Export, cursor) != null,
            };
            const ret = take(.Type, cursor) orelse break :b;
            const name = take(.Word, cursor) orelse break :b;
            const params = try _params(cursor) orelse break :b;
            const ty = .{ .Function = .{
                .params = params.types,
                .ret = try Box(Type).init(ret),
            } };

            try ctx.put(name, .{ .access = .direct, .scope = .global, .ty = ty });
            var context = try ctx.child();
            context.ret = ret;
            for (params.names, params.types) |n, t|
                try context.put(n, .{ .access = .direct, .scope = .param, .ty = t });

            const e = try _expr(context, cursor) orelse break :b;
            take(.SemiColon, cursor) orelse break :b;

            return .{
                .name = name,
                .func = .{
                    .ctx = context,
                    .params = params.names,
                    .expr = e,
                    .attr = attr,
                    .ty = ty,
                },
            };
        }

        cursor.reset(pos);
        return null;
    }

    fn _tuple(ctx: *Context, cursor: *Cursor) Allocator.Error!?[]Ast {
        var exprs = ArrayList(Ast).init(cursor.allocator);
        while (true) {
            const e = try _expr(ctx, cursor) orelse break;
            try exprs.append(e);
            take(.Comma, cursor) orelse break;
        }
        take(.RParen, cursor).?;
        return try exprs.toOwnedSlice();
    }

    fn _block(ctx: *Context, cursor: *Cursor) Allocator.Error!?Ast {
        const context = try ctx.child();
        var exprs = ArrayList(Ast).init(cursor.allocator);
        while (true) {
            const e = try _decl(context, cursor) orelse try _expr(context, cursor) orelse break;
            try exprs.append(e);
            take(.SemiColon, cursor).?;
        }
        take(.RBracket, cursor).?;
        return .{ .Block = .{
            .ctx = context,
            .exprs = try exprs.toOwnedSlice(),
        }};
    }

    fn _if(ctx: *Context, cursor: *Cursor) Allocator.Error!?Ast {
        take(.LParen, cursor) orelse return null;
        const cond = try _expr(ctx, cursor) orelse unreachable;
        take(.RParen, cursor).?;
        const lhs = try _expr(ctx, cursor) orelse unreachable;
        const rhs = if (take(.Else, cursor)) |_| try _expr(ctx, cursor) orelse null else null;
        return .{ .If = .{
            .cond = try Box(Ast).init(cond),
            .lhs = try Box(Ast).init(lhs),
            .rhs = if (rhs) |r| try Box(Ast).init(r) else null,
        } };
    }

    fn _return(ctx: *Context, cursor: *Cursor) Allocator.Error!?Ast {
        const lhs = try expr(ctx, cursor, 0);
        return .{ .Return = try Box(Ast).init(lhs) };
    }

    pub fn _expr(ctx: *Context, cursor: *Cursor) Allocator.Error!?Ast {
        return switch (cursor.peek()) {
            .Eof, .RParen, .RBracket => null,
            else => try expr(ctx, cursor, 0),
        };
    }

    // Pratt Parser
    pub fn expr(ctx: *Context, cursor: *Cursor, min_power: u8) Allocator.Error!Ast {
        if (cursor.peek() == .Eof) panic("Reached Eof", .{});
        var lhs: Ast = switch (cursor.next()) {
            .Eof => panic("Reached Eof", .{}),
            .LBracket => try _block(ctx, cursor) orelse unreachable,
            .If => try _if(ctx, cursor) orelse unreachable,
            .Return => try _return(ctx, cursor) orelse unreachable,
            .Integer => |v| .{ .Integer = v },
            .Word => |v| .{ .Identifier = v },
            else => |token| panic("Unexpected Token: {}", .{token}),
        };

        while (true) {
            const token = cursor.peek();
            const op = switch (token) {
                .Eof, .SemiColon, .Comma, .RParen, .Else => break,
                .Add, .Sub, .Star, .Assign, .Eq, .LParen => token,
                else => panic("Unexpected Token: {}", .{token}),
            };

            if (postfixPower(op)) |power| {
                if (power < min_power) break;
                _ = cursor.next();

                switch (op) {
                    .LParen => {
                        const exprs = try _tuple(ctx, cursor) orelse unreachable;
                        lhs = .{ .Call = .{
                            .f = try Box(Ast).init(lhs),
                            .exprs = try Box([]const Ast).init(exprs),
                        } };
                    },
                    else => unreachable,
                }

                continue;
            }

            if (infixPower(op)) |power| {
                if (power.l < min_power) break;
                _ = cursor.next();

                const rhs = try expr(ctx, cursor, power.r);
                const kind: Ast.BinOpKind = switch (op) {
                    .Add => .Add,
                    .Sub => .Sub,
                    .Star => .Mul,
                    .Assign => .Assign,
                    .Eq => .Eq,
                    else => unreachable,
                };

                lhs = .{ .BinOp = .{
                    .kind = kind,
                    .lhs = try Box(Ast).init(lhs),
                    .rhs = try Box(Ast).init(rhs),
                } };
                continue;
            }

            break;
        }

        return lhs;
    }

    fn prefixPower(op: Token) u8 {
        return switch (op) {
            else => unreachable,
        };
    }

    fn postfixPower(op: Token) ?u8 {
        return switch (op) {
            .LParen => 10,
            else => null,
        };
    }

    fn infixPower(op: Token) ?struct { l: u8, r: u8 } {
        return switch (op) {
            .Assign => .{ .l = 2, .r = 1 },
            .Eq => .{ .l = 3, .r = 4 },
            .Add, .Sub => .{ .l = 5, .r = 6 },
            .Star => .{ .l = 7, .r = 8 },
            else => null,
        };
    }
};
