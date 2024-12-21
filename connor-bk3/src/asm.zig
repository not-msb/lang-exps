const std = @import("std");
const Allocator = std.mem.Allocator;
const StringHashMap = std.StringHashMap;
const AutoHashMap = std.AutoHashMap;
const tools = @import("tools.zig");
const box = tools.box;
const push = tools.push;
const append = tools.append;
const types = @import("types.zig");
const Type = types.Type;
const IrFunction = types.IrFunction;
const IrCode = types.IrCode;
const IrValue = types.IrValue;

const Location = union(enum) {
    register: usize,
    memory: struct {
        offset: isize,
        size: isize,
    },

    fn fmt(self: Location, ty: Type, allocator: Allocator) Allocator.Error![]const u8 {
        _ = ty;
        return switch (self) {
            .register => |v| std.fmt.allocPrint(allocator, "r{d}", .{v}), //TODO, Use actual register names, with limit
            .memory => |t| std.fmt.allocPrint(allocator, "[ebp-{d}]", .{t.offset + t.size}),
        };
    }
};

const Context = struct {
    allocator: Allocator,
    names: StringHashMap(Location),
    indices: AutoHashMap(usize, Location),

    fn init(allocator: Allocator) Context {
        return .{
            .allocator = allocator,
            .names = StringHashMap(Location).init(allocator),
            .indices = AutoHashMap(usize, Location).init(allocator),
        };
    }

    fn deinit(self: *Context) void {
        self.names.deinit();
        self.indices.deinit();
    }

    fn clone(self: Context) Allocator.Error!Context {
        return .{
            .allocator = self.allocator,
            .names = try self.names.clone(),
            .indices = try self.indices.clone(),
        };
    }

    fn getLocationParam(self: *Context, ty: Type) Location {
        const size: isize = @intCast(ty.size());
        var mem: std.meta.fields(Location)[std.meta.fieldIndex(Location, "memory").?].type = .{ .offset = -8 - size, .size = size };
        var names_iter = self.names.valueIterator();
        var indices_iter = self.indices.valueIterator();

        var changed = true;
        while (changed) {
            changed = false;
            while (names_iter.next()) |loc|
                if (loc.* == .memory and (@max(loc.memory.offset, mem.offset) <= @min(loc.memory.offset + loc.memory.size, mem.offset + mem.size))) {
                    mem.offset = loc.memory.offset - loc.memory.size;
                    changed = true;
                };
            while (indices_iter.next()) |loc|
                if (loc.* == .memory and (@max(loc.memory.offset, mem.offset) <= @min(loc.memory.offset + loc.memory.size, mem.offset + mem.size))) {
                    mem.offset = loc.memory.offset - loc.memory.size;
                    changed = true;
                };
        }

        return .{ .memory = mem };
    }

    //TODO, Use the registers
    fn getLocation(self: *Context, ty: Type) Location {
        const size: isize = @intCast(ty.size());
        var mem: std.meta.fields(Location)[std.meta.fieldIndex(Location, "memory").?].type = .{ .offset = 0, .size = size };
        var names_iter = self.names.valueIterator();
        var indices_iter = self.indices.valueIterator();

        var changed = true;
        while (changed) {
            changed = false;
            while (names_iter.next()) |loc|
                if (loc.* == .memory and (@max(loc.memory.offset, mem.offset) <= @min(loc.memory.offset + loc.memory.size, mem.offset + mem.size))) {
                    mem.offset = loc.memory.offset + loc.memory.size;
                    changed = true;
                };
            while (indices_iter.next()) |loc|
                if (loc.* == .memory and (@max(loc.memory.offset, mem.offset) <= @min(loc.memory.offset + loc.memory.size, mem.offset + mem.size))) {
                    mem.offset = loc.memory.offset + loc.memory.size;
                    changed = true;
                };
        }

        return .{ .memory = mem };
    }

    fn getLastOffset(self: *Context) isize {
        var offset: isize = 0;
        var names_iter = self.names.valueIterator();
        var indices_iter = self.indices.valueIterator();

        var changed = true;
        while (changed) {
            changed = false;
            while (names_iter.next()) |loc|
                if (loc.* == .memory and loc.memory.offset < offset) {
                    offset = loc.memory.offset + loc.memory.size;
                    changed = true;
                };
            while (indices_iter.next()) |loc|
                if (loc.* == .memory and loc.memory.offset < offset) {
                    offset = loc.memory.offset + loc.memory.size;
                    changed = true;
                };
        }

        return offset;
    }

    fn put(self: *Context, key: IrValue, is_param: bool) Allocator.Error!void {
        const location = if (is_param) self.getLocationParam(key.ty) else self.getLocation(key.ty);

        return switch (key.kind) {
            .named => |v| self.names.put(v, location),
            .indexed => |v| self.indices.put(v, location),
            .value => unreachable,
        };
    }

    fn get(self: *Context, key: IrValue) Allocator.Error!Location {
        return switch (key.kind) {
            .named => |v| self.names.get(v) orelse switch (key.ty) {
                .U8, .I32 => {
                    const loc = self.getLocation(key.ty);
                    try self.names.put(v, loc);
                    return loc;
                },
                else => unreachable, //TODO
            },
            .indexed => |v| self.indices.get(v) orelse switch (key.ty) {
                .U8, .I32 => {
                    const loc = self.getLocation(key.ty);
                    try self.indices.put(v, loc);
                    return loc;
                },
                else => unreachable, //TODO
            },
            .value => unreachable,
        };
    }
};

fn assemble(ctx: *Context, input: IrCode) Allocator.Error!void {
    switch (input) {
        .Copy => |t| {
            if ((t.dst.kind == .indexed or t.dst.kind == .named) and t.src.kind == .value) {
                const dest = try ctx.get(t.dst);
                std.debug.print("mov {s} {s}, {d}\n", .{ t.dst.ty.fmtAsmSize(), try dest.fmt(t.dst.ty, ctx.allocator), t.src.kind.value });
            } else if ((t.dst.kind == .indexed or t.dst.kind == .named) and (t.src.kind == .indexed or t.src.kind == .named)) {
                const scratch = t.dst.ty.fmtReg();
                const dest = try ctx.get(t.dst);
                const source = try ctx.get(t.src);
                if (dest == .memory and source == .memory) {
                    std.debug.print("mov {s}, {s}\n", .{ scratch, try source.fmt(t.src.ty, ctx.allocator) });
                    std.debug.print("mov {s}, {s}\n", .{ try dest.fmt(t.dst.ty, ctx.allocator), scratch });
                } else unreachable;
            } else unreachable;
        },
        .Add => |t| {
            if (t.dst.kind == .indexed and (t.src1.kind == .indexed or t.src1.kind == .named) and (t.src2.kind == .indexed or t.src2.kind == .named)) {
                const scratch = t.dst.ty.fmtReg();
                const dest = try ctx.get(t.dst);
                const source1 = try ctx.get(t.src1);
                const source2 = try ctx.get(t.src2);
                std.debug.print("mov {s}, {s}\n", .{ scratch, try source1.fmt(t.src1.ty, ctx.allocator) });
                std.debug.print("add {s}, {s}\n", .{ scratch, try source2.fmt(t.src2.ty, ctx.allocator) });
                std.debug.print("mov {s}, {s}\n", .{ try dest.fmt(t.src1.ty, ctx.allocator), scratch });
            } else if (t.dst.kind == .indexed and t.src1.kind == .named and t.src2.kind == .value) {
                const scratch = t.dst.ty.fmtReg();
                const dest = try ctx.get(t.dst);
                const source1 = try ctx.get(t.src1);
                std.debug.print("mov {s}, {s}\n", .{ scratch, try source1.fmt(t.src1.ty, ctx.allocator) });
                std.debug.print("add {s}, {d}\n", .{ scratch, t.src2.kind.value });
                std.debug.print("mov {s}, {s}\n", .{ try dest.fmt(t.src1.ty, ctx.allocator), scratch });
            } else unreachable;
        },
        .Sub => |t| {
            if (t.dst.kind == .indexed and t.src1.kind == .indexed and t.src2.kind == .indexed) {
                const scratch = t.dst.ty.fmtReg();
                const dest = try ctx.get(t.dst);
                const source1 = try ctx.get(t.src1);
                const source2 = try ctx.get(t.src2);
                std.debug.print("mov {s}, {s}\n", .{ scratch, try source1.fmt(t.src1.ty, ctx.allocator) });
                std.debug.print("sub {s}, {s}\n", .{ scratch, try source2.fmt(t.src2.ty, ctx.allocator) });
                std.debug.print("mov {s}, {s}\n", .{ try dest.fmt(t.src1.ty, ctx.allocator), scratch });
            } else if (t.dst.kind == .indexed and t.src1.kind == .named and t.src2.kind == .value) {
                const scratch = t.dst.ty.fmtReg();
                const dest = try ctx.get(t.dst);
                const source1 = try ctx.get(t.src1);
                std.debug.print("mov {s}, {s}\n", .{ scratch, try source1.fmt(t.src1.ty, ctx.allocator) });
                std.debug.print("sub {s}, {d}\n", .{ scratch, t.src2.kind.value });
                std.debug.print("mov {s}, {s}\n", .{ try dest.fmt(t.src1.ty, ctx.allocator), scratch });
            } else unreachable;
        },
        .Mul => |t| {
            if (t.dst.kind == .indexed and t.src1.kind == .named and t.src2.kind == .named) {
                const scratch = t.dst.ty.fmtReg();
                const dest = try ctx.get(t.dst);
                const source1 = try ctx.get(t.src1);
                const source2 = try ctx.get(t.src2);
                std.debug.print("mov {s}, {s}\n", .{ scratch, try source1.fmt(t.src1.ty, ctx.allocator) });
                std.debug.print("mul {s} {s}\n", .{ t.src2.ty.fmtAsmSize(), try source2.fmt(t.src2.ty, ctx.allocator) });
                std.debug.print("mov {s}, {s}\n", .{ try dest.fmt(t.src1.ty, ctx.allocator), scratch });
            } else if (t.dst.kind == .indexed and t.src1.kind == .named and t.src2.kind == .value) {
                const scratch = t.dst.ty.fmtReg();
                const dest = try ctx.get(t.dst);
                const source1 = try ctx.get(t.src1);
                std.debug.print("mov {s}, {s}\n", .{ scratch, try source1.fmt(t.src1.ty, ctx.allocator) });
                std.debug.print("imul {s}, {d}\n", .{ scratch, t.src2.kind.value });
                std.debug.print("mov {s}, {s}\n", .{ try dest.fmt(t.src1.ty, ctx.allocator), scratch });
            } else unreachable;
        },
        .Div => |t| {
            if (t.dst.kind == .indexed and t.src1.kind == .named and t.src2.kind == .named) {
                const scratch = t.dst.ty.fmtReg();
                const dest = try ctx.get(t.dst);
                const source1 = try ctx.get(t.src1);
                const source2 = try ctx.get(t.src2);
                std.debug.print("xor edx, edx\n", .{});
                std.debug.print("mov {s}, {s}\n", .{ scratch, try source1.fmt(t.src1.ty, ctx.allocator) });
                std.debug.print("div {s} {s}\n", .{ t.src2.ty.fmtAsmSize(), try source2.fmt(t.src2.ty, ctx.allocator) });
                std.debug.print("mov {s}, {s}\n", .{ try dest.fmt(t.src1.ty, ctx.allocator), scratch });
            } else unreachable;
        },
        .Call => |t| {
            if (t.dst.kind == .indexed) {
                const scratch = t.dst.ty.fmtReg();
                const dest = try ctx.get(t.dst);
                var esp_offset: usize = 0;
                for (t.args) |arg|
                    esp_offset += arg.ty.size();
                const last_offset = ctx.getLastOffset();
                var arg_offset: usize = if (last_offset <= 0) 0 else @intCast(last_offset);
                esp_offset += arg_offset;

                std.debug.print("sub esp, {d}\n", .{esp_offset});
                for (t.args) |arg| {
                    arg_offset += arg.ty.size();
                    switch (arg.kind) {
                        .value => |v| std.debug.print("mov {s} [ebp-{d}], {d}\n", .{ arg.ty.fmtAsmSize(), arg_offset, v }),
                        .named, .indexed => {
                            const arg_scratch = arg.ty.fmtReg();
                            const argument = try ctx.get(arg);
                            std.debug.print("mov {s}, {s}\n", .{ arg_scratch, try argument.fmt(arg.ty, ctx.allocator) });
                            std.debug.print("mov [ebp-{d}], {s}\n", .{ arg_offset, arg_scratch });
                        },
                    }
                }
                std.debug.print("call {s}\n", .{t.address});
                std.debug.print("mov {s}, {s}\n", .{ try dest.fmt(t.dst.ty, ctx.allocator), scratch });
                std.debug.print("add esp, {d}\n", .{esp_offset});
            } else unreachable;
        },
        .Return => |v| {
            if (v.kind == .indexed or v.kind == .named) {
                const scratch = v.ty.fmtReg();
                const dest = try ctx.get(v);
                std.debug.print("mov {s}, {s}\n", .{ scratch, try dest.fmt(v.ty, ctx.allocator) });
                std.debug.print("pop ebp\n", .{});
                std.debug.print("ret\n", .{});
            } else if (v.kind == .value) {
                const scratch = v.ty.fmtReg();
                std.debug.print("mov {s} {s}, {d}\n", .{ v.ty.fmtAsmSize(), scratch, v.kind.value });
                std.debug.print("pop ebp\n", .{});
                std.debug.print("ret\n", .{});
            } else unreachable;
        },
    }
}

pub fn assembler(input: []const IrFunction, allocator: Allocator) Allocator.Error!void {
    var context = Context.init(allocator);
    defer context.deinit();

    for (input) |ir_function| {
        var sub_context = try context.clone();
        defer sub_context.deinit();

        std.debug.print("{s}:\n", .{ir_function.name});
        std.debug.print("push ebp\n", .{});
        std.debug.print("mov ebp, esp\n", .{});
        for (ir_function.prms) |prm|
            try sub_context.put(IrValue.newNamed(prm.ty, prm.name), true);
        for (ir_function.codes) |code|
            try assemble(&sub_context, code);
        std.debug.print("\n", .{});
    }
}
