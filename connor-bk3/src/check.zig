const std = @import("std");
const Allocator = std.mem.Allocator;
const StringHashMap = std.StringHashMap;
const tools = @import("tools.zig");
const box = tools.box;
const types = @import("types.zig");
const Type = types.Type;
const Ast = types.Ast;

const Error = error{
    ImpOp,
    ImpCall,
    NotFound,
    DiffType,
    NoReturn,
} || Allocator.Error;

const Context = struct {
    allocator: Allocator,
    types: StringHashMap(Type),

    fn init(allocator: Allocator) Context {
        return .{
            .allocator = allocator,
            .types = StringHashMap(Type).init(allocator),
        };
    }

    fn deinit(self: *Context) void {
        self.types.deinit();
    }

    fn clone(self: Context) Allocator.Error!Context {
        return .{
            .allocator = self.allocator,
            .types = try self.types.clone(),
        };
    }
};

fn assign(ctx: *Context, input: Ast.Assign) Error!Type {
    if (!input.ty.eq(check(ctx, input.assigner.*))) return Error.DiffType;
    try ctx.types.put(input.assignee, input.ty);
    return input.ty;
}

fn binOp(ctx: *Context, input: Ast.BinOp) Error!Type {
    const l = check(ctx, input.l.*);
    const r = check(ctx, input.r.*);
    return if (l.eqLossy(r)) |ty| ty else Error.DiffType;
}

fn block(ctx: *Context, input: []const Ast) Error!Type {
    var ty: ?Type = null;

    for (input) |ast| {
        switch (ast) {
            .ret => {
                const checked = check(ctx, ast);
                if (ty) |t| {
                    if (!t.eq(checked)) return Error.DiffType;
                } else {
                    ty = checked;
                }
            },
            else => _ = check(ctx, ast),
        }
    }

    return ty orelse Error.NoReturn;
}

fn fDecl(ctx: *Context, input: Ast.FDecl) Error!Type {
    var context = try ctx.clone();
    var prms = try ctx.allocator.alloc(Type, input.prms.len);

    for (input.prms, 0..) |prm, i| {
        try context.types.put(prm.name, prm.ty);
        prms[i] = prm.ty;
    }

    const ty = .{ .Function = .{ .prms = prms, .ty = try box(ctx.allocator, input.ty) } };

    try ctx.types.put(input.name, ty);
    if (!input.ty.eq(check(&context, input.expr.*))) return Error.DiffType;

    return ty;
}

fn fCall(ctx: *Context, input: Ast.FCall) Error!Type {
    const ty = ctx.types.get(input.name) orelse return Error.NotFound;
    const f = ty.Function;

    if (input.args.len != f.prms.len) return Error.ImpCall;
    for (input.args, 0..) |arg, i|
        if (!check(ctx, arg).eq(f.prms[i])) return Error.DiffType;

    return f.ty.*;
}

fn check(ctx: *Context, input: Ast) Type {
    const ty = switch (input) {
        .identifier => |v| ctx.types.get(v) orelse Error.NotFound,
        .integer => .CompInt,
        .assign => |t| assign(ctx, t),
        .add, .sub, .mul, .div => |t| binOp(ctx, t),
        .block => |v| block(ctx, v),
        .ret => |v| check(ctx, v.*),
        .f_decl => |t| fDecl(ctx, t),
        .f_call => |t| fCall(ctx, t),
    };

    return if (ty) |t| t else |e| {
        std.debug.print("[Error]: {any} in {any}\n", .{ e, input });
        unreachable;
    };
}

pub fn checker(input: []const Ast, allocator: Allocator) void {
    var context = Context.init(allocator);
    defer context.deinit();

    for (input) |ast| {
        _ = check(&context, ast);
    }
}
