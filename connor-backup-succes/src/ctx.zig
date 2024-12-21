const std = @import("std");
const Allocator = std.mem.Allocator;
const StringHashMap = std.StringHashMap;
const Type = @import("token.zig").Type;
const Ast = @import("ast.zig").Ast;
const push = @import("tools.zig").push;

pub const Context = struct {
    allocator: Allocator,
    types: StringHashMap(Type),

    pub fn init(allocator: Allocator) Context {
        return .{
            .allocator = allocator,
            .types = StringHashMap(Type).init(allocator),
        };
    }

    pub fn deinit(self: *Context) void {
        self.types.deinit();
    }

    pub fn clone(self: *const Context) Allocator.Error!Context {
        return .{
            .allocator = self.allocator,
            .types = try self.types.clone(),
        };
    }

    pub fn get(self: *Context, key: []const u8) ?Type {
        return self.types.get(key);
    }

    pub fn insert(self: *Context, key: []const u8, value: Type) Allocator.Error!void {
        return self.types.put(key, value);
    }

    pub fn check(self: *Context, expr: Ast) Allocator.Error!Type {
        return switch (expr.node) {
            .Integer, .Identifier, .Tuple => expr.ty,
            .Call => |tuple| {
                _ = try self.check(tuple.f.*);
                const params = tuple.f.ty.Function.params;
                const ret = expr.ty;

                const expr_ty = try self.check(tuple.expr.*);
                if (!expr_ty.coercible(&.{ .Tuple = params })) @panic("Incompatible types in call");
                return ret;
            },
            .Function => |tuple| {
                for (tuple.exprs) |e|
                    _ = try self.check(e);
                return expr.ty;
            },
        };
    }

    pub fn getFullType(self: *Context, expr: Ast) Type {
        return switch (expr.node) {
            .Integer, .Identifier, .Tuple, .Function => expr.ty,
            .Call => |tuple| self.getFullType(tuple.f.*),
        };
    }

    pub fn unComp(self: *Context, expr: *Ast, expected: ?Type) void {
        switch (expr.node) {
            .Integer, .Identifier => {
                if (expected) |ty| expr.ty = ty;
            },
            .Tuple => |exprs|
                for (0..exprs.len) |i|
                    self.unComp(&exprs[i], expr.ty.Tuple[i]),
            .Call => |tuple| {
                const f_ty = self.getFullType(expr.*);
                self.unComp(tuple.expr, if (f_ty.Function.params.len == 0) null else f_ty.Function.params[0]);
                self.unComp(tuple.f, null);
            },
            .Function => |tuple|
                for (0..tuple.exprs.len) |i|
                    self.unComp(&tuple.exprs[i], null),
        }
    }
};

