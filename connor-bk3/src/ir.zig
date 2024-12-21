const std = @import("std");
const Allocator = std.mem.Allocator;
const StringHashMap = std.StringHashMap;
const tools = @import("tools.zig");
const box = tools.box;
const push = tools.push;
const append = tools.append;
const types = @import("types.zig");
const Type = types.Type;
const Ast = types.Ast;
const IrFunction = types.IrFunction;
const IrCode = types.IrCode;
const IrValue = types.IrValue;

const Context = struct {
    allocator: Allocator,
    types: StringHashMap(Type),
    functions: StringHashMap(Ast.FDecl),
    var_count: usize = 0,

    fn init(allocator: Allocator) Context {
        return .{
            .allocator = allocator,
            .types = StringHashMap(Type).init(allocator),
            .functions = StringHashMap(Ast.FDecl).init(allocator),
        };
    }

    fn deinit(self: *Context) void {
        self.types.deinit();
        self.functions.deinit();
    }

    fn clone(self: Context) Allocator.Error!Context {
        return .{
            .allocator = self.allocator,
            .types = try self.types.clone(),
            .functions = try self.functions.clone(),
            .var_count = self.var_count,
        };
    }

    fn getVar(self: *Context, ty: Type) IrValue {
        self.var_count += 1;
        return IrValue.newIndexed(ty, self.var_count - 1);
    }
};

fn unCompter(ctx: *Context, function: Ast.FDecl, input: []IrCode) bool {
    var changed = false;

    for (input, 0..) |*code, i| {
        switch (code.*) {
            .Copy => |*t| {
                if (t.dst.ty == .CompInt and t.src.ty == .CompInt) {
                    for (input[i + 1 ..]) |sub_code|
                        if (sub_code.contains(t.dst)) |ty|
                            if (ty != .CompInt) {
                                t.dst.ty = ty;
                                changed = true;
                            };
                } else if (t.dst.ty == .CompInt) {
                    t.dst.ty = t.src.ty;
                    changed = true;
                }

                if (t.src.ty == .CompInt and t.dst.ty == .CompInt) {
                    for (input[i + 1 ..]) |sub_code|
                        if (sub_code.contains(t.src)) |ty|
                            if (ty != .CompInt) {
                                t.src.ty = ty;
                                changed = true;
                            };
                } else if (t.src.ty == .CompInt) {
                    t.src.ty = t.dst.ty;
                    changed = true;
                }
            },
            .Add, .Sub, .Mul, .Div => |*t| {
                if (t.dst.ty == .CompInt) {
                    changed = true;
                    t.dst.ty = t.src1.ty;
                } else if (t.src1.ty == .CompInt and t.src2.ty != .CompInt) {
                    changed = true;
                    t.src1.ty = t.src2.ty;
                } else if (t.src2.ty == .CompInt and t.src1.ty != .CompInt) {
                    changed = true;
                    t.src2.ty = t.src1.ty;
                }
            },
            .Call => |*t| {
                for (t.args, 0..) |*arg, j| {
                    if (arg.ty == .CompInt)
                        switch (arg.kind) {
                            .value => {
                                const f = ctx.functions.get(t.address).?;
                                arg.ty = f.prms[j].ty;
                                changed = true;
                            },
                            .named, .indexed => for (input) |sub_code| {
                                if (sub_code.contains(arg.*)) |ty| {
                                    if (ty != .CompInt) {
                                        changed = true;
                                        arg.ty = ty;
                                    }
                                }
                            },
                        };
                }
            },
            .Return => |v| {
                if (v.ty == .CompInt) {
                    changed = true;
                    switch (function.ty) {
                        .U8 => input[i].Return.ty = .U8,
                        .I32 => input[i].Return.ty = .I32,
                        else => unreachable,
                    }
                }
            },
        }
    }

    return changed;
}

fn optimize(input: *[]IrCode, allocator: Allocator) Allocator.Error!bool {
    var codes = try allocator.alloc(IrCode, 0);
    var slice = input.*;
    var changed = false;

    for (slice, 0..) |code, i| {
        switch (code) {
            .Copy => |t| blk: {
                if (!t.dst.ty.eqExact(t.src.ty)) break :blk;
                if ((t.dst.kind == .indexed or t.dst.kind == .named) and t.src.kind == .value) {
                    var sub_changed = false;
                    for (slice[i + 1 ..]) |*sub_code| {
                        if (sub_code.contains(t.dst)) |_| sub_changed = true;
                        sub_code.replaceValue(t.src, t.dst);
                    }
                    if (sub_changed) {
                        changed = true;
                        continue;
                    }
                } else if ((t.dst.kind == .indexed or t.dst.kind == .named) and (t.src.kind == .indexed or t.src.kind == .named)) {
                    var contains = false;
                    for (slice[i + 1 ..]) |*sub_code| {
                        const dest = sub_code.dest() orelse continue;
                        if (std.meta.eql(dest.kind, t.src.kind))
                            contains = true;
                    }
                    if (!contains) {
                        var sub_changed = false;
                        for (slice[i + 1 ..]) |*sub_code| {
                            if (sub_code.contains(t.dst)) |_| sub_changed = true;
                            sub_code.replaceValue(t.src, t.dst);
                        }
                        if (sub_changed) {
                            changed = true;
                            continue;
                        }
                    }
                }
            },
            else => {},
        }

        codes = try allocator.realloc(codes, codes.len + 1);
        codes[codes.len - 1] = code;
    }

    allocator.free(slice);
    input.* = codes;
    return changed;
}

fn statement(ctx: *Context, input: Ast) Allocator.Error![]IrCode {
    switch (input) {
        .identifier => |v| {
            const ty = ctx.types.get(v).?;
            return try box(ctx.allocator, [1]IrCode{.{ .Copy = .{ .dst = ctx.getVar(ty), .src = IrValue.newNamed(ty, v) } }});
        },
        .integer => |v| return try box(ctx.allocator, [1]IrCode{.{ .Copy = .{ .dst = ctx.getVar(.CompInt), .src = IrValue.newValue(.CompInt, v) } }}),
        .assign => |t| {
            try ctx.types.put(t.assignee, t.ty);
            var stmts = try statement(ctx, t.assigner.*);
            const dest = stmts[stmts.len - 1].dest().?;
            const copy = .{ .Copy = .{ .dst = IrValue.newNamed(t.ty, t.assignee), .src = dest } };
            return push(ctx.allocator, IrCode, stmts, copy);
        },
        .add => |t| {
            var lstmts = try statement(ctx, t.l.*);
            const rstmts = try statement(ctx, t.r.*);
            const ldest = lstmts[lstmts.len - 1].dest().?;
            const rdest = rstmts[rstmts.len - 1].dest().?;
            const ty = ldest.ty.eqLossy(rdest.ty).?;
            const add = .{ .Add = .{ .dst = ctx.getVar(ty), .src1 = ldest, .src2 = rdest } };
            return push(ctx.allocator, IrCode, try append(ctx.allocator, IrCode, lstmts, rstmts), add);
        },
        .sub => |t| {
            var lstmts = try statement(ctx, t.l.*);
            const rstmts = try statement(ctx, t.r.*);
            const ldest = lstmts[lstmts.len - 1].dest().?;
            const rdest = rstmts[rstmts.len - 1].dest().?;
            const ty = ldest.ty.eqLossy(rdest.ty).?;
            const sub = .{ .Sub = .{ .dst = ctx.getVar(ty), .src1 = ldest, .src2 = rdest } };
            return push(ctx.allocator, IrCode, try append(ctx.allocator, IrCode, lstmts, rstmts), sub);
        },
        .mul => |t| {
            var lstmts = try statement(ctx, t.l.*);
            const rstmts = try statement(ctx, t.r.*);
            const ldest = lstmts[lstmts.len - 1].dest().?;
            const rdest = rstmts[rstmts.len - 1].dest().?;
            const ty = ldest.ty.eqLossy(rdest.ty).?;
            const mul = .{ .Mul = .{ .dst = ctx.getVar(ty), .src1 = ldest, .src2 = rdest } };
            return push(ctx.allocator, IrCode, try append(ctx.allocator, IrCode, lstmts, rstmts), mul);
        },
        .div => |t| {
            var lstmts = try statement(ctx, t.l.*);
            const rstmts = try statement(ctx, t.r.*);
            const ldest = lstmts[lstmts.len - 1].dest().?;
            const rdest = rstmts[rstmts.len - 1].dest().?;
            const ty = ldest.ty.eqLossy(rdest.ty).?;
            const div = .{ .Div = .{ .dst = ctx.getVar(ty), .src1 = ldest, .src2 = rdest } };
            return push(ctx.allocator, IrCode, try append(ctx.allocator, IrCode, lstmts, rstmts), div);
        },
        .block => |v| {
            var stmts = try ctx.allocator.alloc(IrCode, 0);
            for (v) |ast| {
                const astmts = try statement(ctx, ast);
                stmts = try append(ctx.allocator, IrCode, stmts, astmts);
            }
            return stmts;
        },
        .ret => |v| {
            var stmts = try statement(ctx, v.*);
            const ret = .{ .Return = stmts[stmts.len - 1].dest().? };
            return push(ctx.allocator, IrCode, stmts, ret);
        },
        .f_call => |t| {
            const ty = ctx.types.get(t.name).?;
            var args = try ctx.allocator.alloc(IrValue, 0);
            var stmts = try ctx.allocator.alloc(IrCode, 0);
            for (t.args) |arg| {
                const astmts = try statement(ctx, arg);
                args = try push(ctx.allocator, IrValue, args, astmts[astmts.len - 1].dest().?);
                stmts = try append(ctx.allocator, IrCode, stmts, astmts);
            }
            const call = .{ .Call = .{ .dst = ctx.getVar(ty), .address = t.name, .args = args } };
            return push(ctx.allocator, IrCode, stmts, call);
        },
        else => unreachable,
    }
}

pub fn statementer(input: []const Ast, allocator: Allocator) Allocator.Error![]const IrFunction {
    var ir_functions = try allocator.alloc(IrFunction, 0);
    var context = Context.init(allocator);
    defer context.deinit();

    for (input) |ast| {
        switch (ast) {
            .f_decl => |t| {
                try context.types.put(t.name, t.ty);
                try context.functions.put(t.name, t);
            },
            else => unreachable,
        }
    }

    for (input) |ast| {
        switch (ast) {
            .f_decl => |t| {
                var sub_context = try context.clone();
                defer sub_context.deinit();

                for (t.prms) |prm|
                    try sub_context.types.put(prm.name, prm.ty);

                var ir_codes = switch (t.expr.*) {
                    .identifier, .integer, .add, .sub, .mul, .div, .f_call => try statement(&sub_context, .{ .ret = t.expr }),
                    .block => try statement(&sub_context, t.expr.*),
                    else => unreachable,
                };

                while (unCompter(&sub_context, t, ir_codes)) {}
                while (try optimize(&ir_codes, allocator))
                    while (unCompter(&sub_context, t, ir_codes)) {};

                const ir_function = .{ .name = t.name, .prms = t.prms, .ty = t.ty, .codes = ir_codes };
                ir_functions = try push(allocator, IrFunction, ir_functions, ir_function);
            },
            else => unreachable,
        }
    }

    return ir_functions;
}
