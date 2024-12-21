const std = @import("std");
const Alloc = std.mem.Allocator;
const StringHashMap = std.StringHashMap;
const types = @import("types.zig");
const Type = types.Type;
const Ast = types.Ast;
const box = @import("tools").box;

const Error = error {
    NotFound,
    ImpOp,
} || Alloc.Error;

const Context = struct {
    alloc: Alloc,
    types: StringHashMap(Type),

    fn init(alloc: Alloc) Context {
        return .{
            .alloc = alloc,
            .types = StringHashMap(Type).init(alloc),
        };
    }

    fn deinit(self: *Context) void {
        self.types.deinit();
        self.* = undefined;
    }

    fn clone(self: Context) Alloc.Error!Context {
        return .{
            .alloc = self.alloc,
            .types = try self.types.clone(),
        };
    }
};

fn identifier(ctx: *Context, value: []const u8) Error!Type {
    return ctx.types.get(value) orelse return Error.NotFound;
}

fn add(ctx: *Context, tuple: Ast.ADD) Error!Type {
    const l = check(ctx, tuple.l.*);
    const r = check(ctx, tuple.r.*);
    const equal = if (l.eq(r)) l else return Error.ImpOp;
    return switch (equal) {
        Type.I64 => Type.I64,
        else => Error.ImpOp,
    };
}

fn neg(ctx: *Context, value: *Ast) Error!Type {
    const ty = check(ctx, value.*);
    return switch (ty) {
        Type.I64 => Type.I64,
        else => Error.ImpOp,
    };
}

fn ret(ctx: *Context, value: *Ast) Error!Type {
    return check(ctx, value.*);
}

fn fDecl(ctx: *Context, tuple: Ast.F_DECL) Error!Type {
    var context = try ctx.clone();
    var prms = try ctx.alloc.alloc(Type, tuple.prms.len);

    for (tuple.prms, 0..) |prm, i| {
        try context.types.put(prm.name, prm.ty);
        prms[i] = prm.ty;
    }

    const ty = Type{ .Function = .{
        .prms = prms,
        .ty = try box(ctx.alloc, tuple.ty),
    }};

    try ctx.types.put(tuple.name, ty);
    try fExpr(&context, tuple.ty, tuple.expr);
    return ty;
}

fn fExpr(ctx: *Context, ty: Type, value: *Ast) Error!void {
    switch (value.*) {
        .identifier, .integer, .add, .neg, .f_call => if (ty.eq(try ret(ctx, value))) return ,
        else => {},
    }

    return Error.ImpOp;
}

fn fCall(ctx: *Context, tuple: Ast.F_CALL) Error!Type {
    const ty = ctx.types.get(tuple.name) orelse return Error.NotFound;
    const function = ty.Function;
    const prms = function.prms;
    for (tuple.args, 0..) |arg, i| {
        const param = prms[i];
        if (!check(ctx, arg).eq(param)) return Error.ImpOp;
    }
    return function.ty.*;
}

pub fn check(ctx: *Context, ast: Ast) Type {
    const ty = switch (ast) {
        .identifier => |v| identifier(ctx, v),
        .integer => Type{ .I64 = undefined },
        .add => |t| add(ctx, t),
        .neg => |v| neg(ctx, v),
        .ret => |v| ret(ctx, v),
        .f_decl => |t| fDecl(ctx, t),
        .f_call => |t| fCall(ctx, t),
    };

    if (ty) |t| {
        return t;
    } else |e| {
        std.debug.print("Error: {any} in {any}\n", .{e, ast});
        unreachable;
    }
}

pub fn checker(alloc: Alloc, slice: []const Ast) void {
    var ctx = Context.init(alloc);
    defer ctx.deinit();

    for (slice) |item| {
        _ = check(&ctx, item);
    }
}
