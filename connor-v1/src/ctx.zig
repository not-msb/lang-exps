const std = @import("std");
const lib = @import("lib.zig");
const panic = std.debug.panic;
const ArrayList = std.ArrayList;
const Allocator = std.mem.Allocator;
const StringHashMap = std.StringHashMap;
const Type = lib.Type;
const File = lib.File;
const Ast = lib.Ast;
const Box = lib.box.Box;

pub const Symbol = struct {
    const Access = enum {
        direct,
        deref,
    };

    pub const Scope = enum {
        param,
        local,
        global,

        pub fn fmt(self: Scope) u8 {
            return switch (self) {
                .param, .local => '%',
                .global => '$',
            };
        }
    };

    access: Access,
    scope: Scope,
    ty: Type,
};

pub const Context = struct {
    allocator: Allocator,
    parent: ?*const Context = null,
    symbols: StringHashMap(Symbol),
    ret: ?Type = null,

    pub fn init(allocator: Allocator) Context {
        return .{
            .allocator = allocator,
            .symbols = StringHashMap(Symbol).init(allocator),
        };
    }

    pub fn deinit(self: *Context) void {
        self.symbols.deinit();
    }

    pub fn child(self: *Context) Allocator.Error!*Context {
        const ctx = .{
            .allocator = self.allocator,
            .parent = self,
            .symbols = StringHashMap(Symbol).init(self.allocator),
            .ret = self.ret,
        };
        return (try Box(Context).init(ctx)).ptr;
    }

    pub fn get(self: *const Context, key: []const u8) ?Symbol {
        return self.symbols.get(key) orelse if (self.parent) |parent| parent.get(key) else null;
    }

    pub fn put(self: *Context, key: []const u8, value: Symbol) Allocator.Error!void {
        return self.symbols.putNoClobber(key, value);
    }

    pub fn scan(file: File) Allocator.Error!void {
        var functions = file.functions.iterator();

        while (functions.next()) |entry| {
            const function = entry.value_ptr.*;
            const f_ret = function.ty.Function.ret.deref();
            const context = function.ctx;

            const ret = try context.check(function.expr);
            if (!ret.coercible(f_ret)) panic("Incompatible return type", .{});
        }
    }

    fn check(self: *const Context, ast: Ast) Allocator.Error!Type {
        switch (ast) {
            .Integer => return .U64,
            .Identifier => |v| return self.get(v).?.ty,
            .Block => |t| { // TODO: Improve this code for inner blocks
                var ret: ?Type = null;
                for (t.exprs) |expr| {
                    const ty = try t.ctx.check(expr);
                    if (ty == .NoReturn) ret = .NoReturn;
                }
                return ret orelse .Void;
            },
            .Call => |t| {
                const f = try self.check(t.f.deref());
                if (f != .Function) panic("Function call on Non-Function", .{});
                const exprs = t.exprs.deref();
                for (exprs, 0..) |expr, i| {
                    const e = try self.check(expr);
                    if (!e.coercible(f.Function.params[i])) panic("Incompatible parameter type", .{});
                }
                return f.Function.ret.deref();
            },
            .If => |t| {
                const ty = try self.check(t.cond.deref());
                if (!ty.coercible(.Bool)) panic("If expected Bool, found: {}", .{ty});
                const l_ty = try self.check(t.lhs.deref());
                if (t.rhs) |rhs| {
                    const r_ty = try self.check(rhs.deref());
                    if (!r_ty.coercible(l_ty)) panic("Incompatible If type", .{});
                    return l_ty;
                }
                return .Void; // Only return type when else is used
            },
            .BinOp => |t| {
                const lhs = try self.check(t.lhs.deref());
                const rhs = try self.check(t.rhs.deref());
                switch (t.kind) {
                    .Assign => {
                        if (!rhs.coercible(lhs)) panic("BinOp incompatible types", .{});
                        return .Void;
                    },
                    .Add, .Sub, .Mul => {
                        if (!(lhs.isNumeric() or rhs.isNumeric())) panic("BinOp on non-number", .{});
                        return lhs.min(rhs);
                    },
                    .Eq => {
                        if (!rhs.coercible(lhs)) panic("BinOp incompatible types", .{});
                        return .Bool;
                    },
                }
            },
            .AddrOf => |v| {
                const ty = try self.check(v.deref());
                return .{ .Pointer = try Box(Type).init(ty) };
            },
            .Return => |v| {
                const ty = try self.check(v.deref());
                if (!ty.coercible(self.ret.?)) panic("Incompatible return type", .{});
                return .NoReturn;
            },
        }
    }
};
