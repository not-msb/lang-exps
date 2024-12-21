const std = @import("std");
const lib = @import("lib.zig");
const Allocator = std.mem.Allocator;
const StringHashMap = std.StringHashMap;
const Function = lib.ir.Function;
const Error = lib.ParseError;
const Ast = lib.Ast;
const Type = lib.Type;
const Var = lib.Var;

pub const Context = struct {
    map: StringHashMap(Var),
    count: *usize,

    parent: ?*Context = null,
    ret: ?Type = null,

    pub fn init(allocator: Allocator, ret: ?Type) Allocator.Error!Context {
        return .{
            .map = StringHashMap(Var).init(allocator),
            .count = try lib.box(allocator, @as(usize, 0)),
            .ret = ret,
        };
    }

    pub fn child(self: *Context, ret: ?Type) Allocator.Error!*Context {
        const context: Context = .{
            .map = StringHashMap(Var).init(self.map.allocator),
            .count = self.count,
            .parent = self,
            .ret = ret orelse self.ret,
        };
        return lib.box(self.map.allocator, context);
    }

    pub fn get(self: *const Context, key: []const u8) ?Var {
        return self.map.get(key) orelse if (self.parent) |parent| parent.get(key) else null;
    }

    pub fn put(self: *Context, key: []const u8, value: Var) Allocator.Error!void {
        return self.map.putNoClobber(key, value);
    }

    pub fn newVar(self: *Context, name: ?[]const u8, ty: Type) Allocator.Error!Var {
        const tag: Var.Tag = if (name) |n| .{ .named = n } else .{ .local = self.count.* };
        const v = .{
            .scope = .local,
            .tag = tag,
            .ty = ty,
        };

        if (name) |n|
            try self.put(n, v)
        else
            self.count.* += 1;

        return v;
    }

    pub fn check(self: *Context, expr: *Ast) Error!Type {
        switch (expr.*) {
            .Integer => return .CompInt,
            .Identifier => |v| {
                const symbol = self.get(v) orelse return Error.NonExistent;
                return symbol.ty;
            },
            .Deref => |v| {
                const ty = try self.check(v);
                return switch (ty) {
                    .Ptr => |pv| pv.*,
                    else => Error.TypeNonPointer,
                };
            },
            .Declare => |t| {
                const src = try self.check(t.src);

                if (self.get(t.dst)) |_|
                    return Error.Unrewritable;
                _ = try self.newVar(t.dst, t.ty);

                if (!src.coercible(t.ty)) return Error.TypeIncompatible;
                return .Void;
            },
            .Assign => |t| {
                const src = try self.check(t.src);
                const dst = try self.check(t.dst);
                if (!src.coercible(dst)) return Error.TypeIncompatible;
                return .Void;
            },
            .BinOp => |t| {
                const lhs = try self.check(t.lhs);
                const rhs = try self.check(t.rhs);
                switch (t.kind) {
                    .Add, .Sub, .Mul => {
                        if (!(lhs.isNumeric() or rhs.isNumeric())) return Error.TypeArithmeticNonInteger;
                        if (!lhs.compatible(rhs)) return Error.TypeIncompatible;
                        return Type.maxNumeric(lhs, rhs).?;
                    },
                    .Eq, .Ne => {
                        if (!lhs.compatible(rhs)) return Error.TypeIncompatible;
                        return .Bool;
                    },
                }
            },
            .If => |t| {
                const cond = try self.check(t.cond);
                const succ = try self.check(t.succ);
                const fail = try self.check(t.fail);
                if (!cond.coercible(.Bool)) return Error.TypeIncompatible;
                if (!fail.compatible(succ)) return Error.TypeIncompatible;
                return Type.maxNumeric(succ, fail) orelse succ;
            },
            .While => |t| {
                const cond = try self.check(t.cond);
                const body = try self.check(t.body);
                if (!cond.coercible(.Bool)) return Error.TypeIncompatible;
                _ = body;
                return .Void;
            },
            .Call => |t| {
                const func = try self.check(t.name);
                const prms, const ret = switch (func) {
                    .Function => |t2| .{ t2.params, t2.ret },
                    else => return Error.TypeNonFunction,
                };

                for (t.args, 0..) |*arg, i| {
                    const ty = try self.check(arg);
                    if (!ty.coercible(prms[i]))
                        return Error.TypeIncompatible;
                }

                return ret.*;
            },
            .Block => |*t| {
                t.ctx = try self.child(null);

                var ret: Type = .Void;
                for (t.body) |*e| {
                    const ty = try t.ctx.check(e);
                    if (ty == .NoReturn) ret = .NoReturn;
                }
                return ret;
            },
            .Return => |v| {
                const ty = try self.check(v);
                const ret = self.ret orelse return Error.NonReturnCtx;
                if (!ty.coercible(ret)) return Error.TypeIncompatible;
                return .NoReturn;
            },
        }
    }
};
