const std = @import("std");
const Allocator = std.mem.Allocator;
const StringHashMap = std.StringHashMap;
const Type = @import("token.zig").Type;

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

    pub fn get(self: *const Context, key: []const u8) ?Type {
        return self.types.get(key);
    }

    pub fn getUnsafe(self: *const Context, key: []const u8) Type {
        return self.types.get(key).?;
    }

    pub fn insert(self: *Context, key: []const u8, value: Type) Allocator.Error!void {
        return self.types.put(key, value);
    }
};
