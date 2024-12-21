const std = @import("std");
const Allocator = std.mem.Allocator;

pub const Token = @import("token.zig").Token;
pub const Ast = @import("ast.zig").Ast;
pub const Cursor = @import("cursor.zig").Cursor;
pub const Context = @import("ctx.zig").Context;
pub const Type = @import("type.zig").Type;

pub const ParseError = error{
    Eof,
    Unexpected,
    Unrewritable,
    NonExistent,
    NonReturnCtx,
    TypeIncompatible,
    TypeArithmeticNonInteger,
    TypeNonPointer,
    TypeNonFunction,
} || Allocator.Error;

pub var global_error: Location = undefined;

pub const Location = struct {
    row: usize,
    col: usize,
};

pub fn Pair(comptime L: type, comptime R: type) type {
    return struct { L, R };
}

pub fn box(allocator: Allocator, value: anytype) Allocator.Error!*@TypeOf(value) {
    const ptr = try allocator.create(@TypeOf(value));
    ptr.* = value;
    return ptr;
}

pub const Var = struct {
    scope: Scope,
    tag: Tag,
    ty: Type,

    pub const Scope = enum {
        global,
        local,
    };

    pub const Tag = union(enum) {
        named: []const u8,
        local: usize,
        liter: usize,
    };

    pub fn eql(self: Var, rhs: Var) bool {
        const activeTag = std.meta.activeTag;
        if (self.scope != rhs.scope) return false;
        if (activeTag(self.tag) != activeTag(rhs.tag)) return false;
        return switch (self.tag) {
            .named => |name| std.mem.eql(u8, name, rhs.tag.named),
            .local => |index| index == rhs.tag.local,
            .liter => |liter| liter == rhs.tag.liter,
        };
    }
};
