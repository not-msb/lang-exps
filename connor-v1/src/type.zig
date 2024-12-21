const std = @import("std");
const lib = @import("lib.zig");
const panic = std.debug.panic;
const Box = lib.box.Box;

pub const Type = union(enum) {
    Function: struct {
        params: []const Type,
        ret: Box(Type),
    },
    Pointer: Box(Type),
    Void,
    NoReturn,
    Bool,
    U8,
    U32,
    U64,

    pub fn size(self: Type) ?usize {
        return switch (self) {
            .Void => 0,
            .Bool, .U8 => 1,
            .U32 => 4,
            .U64, .Pointer => 8,
            else => null,
        };
    }

    pub fn alignment(self: Type) ?usize {
        return switch (self) {
            .Bool, .U8, .U32 => 4,
            .U64, .Pointer => 8,
            else => null,
        };
    }

    pub fn isNumeric(self: Type) bool {
        return switch (self) {
            .U8, .U32, .U64 => true,
            else => false,
        };
    }

    pub fn baseFmt(self: Type) []const u8 {
        return switch (self) {
            .NoReturn => "w",
            .Bool, .U8 => "w",
            .U32 => "w",
            .U64, .Pointer => "l",
            else => panic("Couldn't format {}", .{self}),
        };
    }

    pub fn extFmt(self: Type) []const u8 {
        return switch (self) {
            .NoReturn => "w",
            .Bool, .U8 => "b",
            .U32 => "w",
            .U64, .Pointer => "l",
            else => panic("Couldn't format {}", .{self}),
        };
    }

    pub fn abiFmt(self: Type) []const u8 {
        return switch (self) {
            .NoReturn => "w",
            .Bool, .U8 => "ub",
            .U32 => "w",
            .U64, .Pointer => "l",
            else => panic("Couldn't format {}", .{self}),
        };
    }

    pub fn min(self: Type, rhs: Type) Type {
        return if (self.size().? >= rhs.size().?) rhs else self;
    }

    pub fn eql(self: Type, rhs: Type) bool {
        return switch (self) {
            .Void => rhs == .Void,
            .NoReturn => rhs == .NoReturn,
            .Bool => rhs == .Bool,
            .U8 => rhs == .U8,
            .U32 => rhs == .U32,
            .U64 => rhs == .U64,
            .Pointer => |v| rhs == .Pointer and v.deref().eql(rhs.Pointer.deref()),
            else => false,
        };
    }

    pub fn coercible(self: Type, rhs: Type) bool {
        if (self.eql(rhs)) return true;
        if (self.isNumeric() and rhs.isNumeric() and self.size().? >= rhs.size().?) return true;

        return switch (self) {
            .NoReturn => true,
            else => false,
        };
    }
};
