const std = @import("std");
const Allocator = std.mem.Allocator;
const ArrayList = std.ArrayList;
const StringHashMap = std.StringHashMap;
const EnumSet = std.EnumSet;
const EnumMap = std.EnumMap;
const EnumArray = std.EnumArray;

const Ir  = @import("ir.zig").Ir;
const Graph = @import("main.zig").Graph;
const Value = @import("main.zig").Value;
const Register = @import("main.zig").Register;

pub const Context = struct {
    parent: ?*const Context,
    env: Environment,
    symbols: Map,

    const Key = []const u8;
    const Map = StringHashMap(Value);

    const Environment = struct {
        used_regs: EnumSet(Register),
        registers: EnumArray(Register, usize), // TODO: This should be changed to a EnumArray
        current: EnumArray(Register, usize), // Idk how I made this, it records the lifetime *of variables* only
        stack: usize = 0,
    };

    pub fn new(allocator: Allocator) Context {
        return .{
            .parent = null,
            .symbols = Map.init(allocator),
            .env = .{
                .used_regs = EnumSet(Register).init(.{}),
                .registers = EnumArray(Register, usize).initFill(0),
                .current = EnumArray(Register, usize).initFill(0),
            },
        };
    }

    pub fn child(self: *const Context) Context {
        return .{
            .parent = self,
            .symbols = Map.init(self.symbols.allocator),
            .env = .{
                .used_regs = EnumSet(Register).init(.{}),
                .registers = EnumArray(Register, usize).initFill(0),
                .current = EnumArray(Register, usize).initFill(0),
            },
        };
    }

    pub fn get(self: *const Context, key: Key) ?Value {
        return self.symbols.get(key) orelse if (self.parent) |parent| parent.get(key) else null;
    }

    pub fn put(self: *Context, key: Key, val: Value) Allocator.Error!void {
        try self.symbols.putNoClobber(key, val);
    }

    // Yeah this function can be removed
    // Use compute space to replace this
    //pub fn prepareAsm(self: *Context, graph: Graph) void {
    //    const old = graph.current;
    //    defer graph.current = old;

    //    for (list) |elem| switch (elem) {
    //        .Asm => |lit| {
    //            for (lit.regs) |reg| {
    //                self.env.used_regs.insert(reg);
    //            }
    //            if (lit.out) |reg| {
    //                self.env.used_regs.insert(reg);
    //            }
    //        },
    //        else => {},
    //    };
    //}

    pub fn prepareCallStack(self: *Context, args: usize) Allocator.Error![]Value {
        const allocator = self.symbols.allocator;
        const buffer = try allocator.alloc(Value, args);

        for (buffer) |*elem| {
            elem.* = .{ .stack = -16 + -8 * @as(isize, @intCast(self.env.stack)) };
            self.env.stack += 1;
        }

        return buffer;
    }

    pub fn prepareCCallStack(self: *Context, args: usize) []const Value {
        _ = self;
        const c_args = [_]Value{
            Value { .register = .rdi },
            Value { .register = .rsi },
            Value { .register = .rdx },
            Value { .register = .rcx },
            Value { .register = .r8 },
            Value { .register = .r9 },
        };
        return c_args[0..args];
    }
};
