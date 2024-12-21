const std = @import("std");
const lib = @import("lib.zig");
const stdout = std.io.getStdOut().writer();
const panic = std.debug.panic;
const ArrayList = std.ArrayList;
const Allocator = std.mem.Allocator;
const StringArrayHashMap = std.StringArrayHashMap;
const Type = lib.Type;
const File = lib.File;
const Ast = lib.Ast;
const Symbol = lib.Symbol;
const Context = lib.Context;
const box = lib.tools.box;
const Box = lib.box.Box;

pub const Storage = union(enum) {
    literal: usize,
    temp: struct { usize, Type }, // local
    named: struct { []const u8, Symbol },

    fn ty(self: Storage) Type {
        return switch (self) {
            .literal => .U64,
            .temp => |t| t[1],
            .named => |t| t[1].ty,
        };
    }

    fn fmt(self: Storage, allocator: Allocator) std.fmt.AllocPrintError![]const u8 {
        return switch (self) {
            .literal => |v| try std.fmt.allocPrint(allocator, "{d}", .{v}),
            .temp => |t| try std.fmt.allocPrint(allocator, "%t{d}", .{t[0]}),
            .named => |t| try std.fmt.allocPrint(allocator, "{c}{s}", .{ t[1].scope.fmt(), t[0] }),
        };
    }
};

pub const IrFile = struct {
    const Function = struct {
        ctx: *Context,
        params: [][]const u8,
        attr: Ast.Attr,
        block: []const Ir,
        ty: Type,
    };

    ctx: *Context,
    functions: StringArrayHashMap(Function),

    pub fn from(ctx: *Context, file: File) Allocator.Error!IrFile {
        var functions = file.functions.iterator();
        var list = StringArrayHashMap(IrFile.Function).init(ctx.allocator);

        while (functions.next()) |entry| {
            const name = entry.key_ptr.*;
            const function = entry.value_ptr.*;

            var state = State{};
            const context = function.ctx;

            const expr: Ast = switch (function.expr) {
                .Block => function.expr,
                else => .{ .Return = try Box(Ast).init(function.expr) },
            };
            const block = try Ir.from(context, &state, expr);

            try list.put(name, .{
                .ctx = context,
                .params = function.params,
                .attr = function.attr,
                .block = block,
                .ty = function.ty,
            });
        }

        return .{
            .ctx = ctx,
            .functions = list,
        };
    }

    pub fn compile(self: IrFile) !void {
        var functions = self.functions.iterator();
        while (functions.next()) |entry| {
            const name = entry.key_ptr.*;
            const function = entry.value_ptr.*;
            const func = function.ty.Function;
            //const context = function.ctx;

            if (function.attr._export)
                try stdout.writeAll("export ");
            try stdout.print("function {s} ${s}(", .{ func.ret.deref().abiFmt(), name });
            for (function.params, func.params) |param, ty|
                try stdout.print("{s} %{s},", .{ ty.abiFmt(), param });
            try stdout.writeAll(") {\n@start\n");
            //{
            //    var iter = context.symbols.iterator();
            //    while (iter.next()) |symbol_entry| {
            //        const n = symbol_entry.key_ptr.*;
            //        const symbol = symbol_entry.value_ptr.*;
            //        if (symbol.scope == .local)
            //            try stdout.print("\t%{s} =l alloc{d} {d}\n", .{n, symbol.ty.alignment().?, symbol.ty.size().?});
            //    }
            //}
            for (function.block) |ir| {
                try stdout.print("@L{d}\n", .{ir.label});
                try ir.compile(self.ctx.allocator);
            }
            try stdout.writeAll("}\n");
        }
    }
};

const State = struct {
    chunk: usize = 0,
    allocated: usize = 0,

    fn newChunk(self: *State) usize {
        self.chunk += 1;
        return self.chunk - 1;
    }

    fn alloc(self: *State, ty: Type) Storage {
        self.allocated += 1;
        return .{ .temp = .{ self.allocated - 1, ty } };
    }
};

const Node = union(enum) {
    const BinOp = struct {
        dst: Storage,
        src: Storage,
    };

    const Call_t = struct {
        f: Storage,
        dst: Storage,
        src: []const Storage,
    };

    const Jnz_t = struct {
        cond: Storage,
        dst: ?Storage,
        lhs: usize,
        rhs: usize,
    };

    const BinOp_t = struct {
        kind: Ast.BinOpKind,
        dst: Storage,
        lhs: Storage,
        rhs: Storage,
    };

    Nop,
    Echo: Storage,
    Alloc: Storage,
    Store: BinOp,
    Load: BinOp,
    Copy: BinOp,
    Call: Call_t,
    Jmp: usize,
    Jnz: Jnz_t,
    BinOp: BinOp_t,
    Return: Storage,
};

pub const Ir = struct {
    label: usize,
    node: Node,

    fn from(ctx: *Context, state: *State, input: Ast) Allocator.Error![]Ir {
        var list = ArrayList(Ir).init(ctx.allocator);

        // TODO: Prevent ALL return from 'from'
        switch (input) {
            .Integer => |v| try list.append(.{
                .label = state.newChunk(),
                .node = .{ .Echo = .{ .literal = v } },
            }),
            .Identifier => |v| {
                const node: Node = switch (ctx.get(v).?.access) {
                    .direct => .{ .Echo = .{ .named = .{ v, ctx.get(v).? } } },
                    .deref => .{ .Load = .{
                        .dst = state.alloc(ctx.get(v).?.ty),
                        .src = .{ .named = .{ v, ctx.get(v).? } },
                    } },
                };
                try list.append(.{
                    .label = state.newChunk(),
                    .node = node,
                });
            },
            .Block => |t| {
                var iter = t.ctx.symbols.iterator();
                while (iter.next()) |symbol_entry| {
                    const name = symbol_entry.key_ptr.*;
                    const symbol = symbol_entry.value_ptr.*;
                    if (symbol.scope == .local)
                        try list.append(.{
                            .label = state.newChunk(),
                            .node = .{ .Alloc = .{ .named = .{ name, symbol } } }
                        });
                }

                for (t.exprs) |item| {
                    const slice = try from(t.ctx, state, item);
                    try list.appendSlice(slice);
                }
            },
            .Call => |t| b: {
                const f = try from(ctx, state, t.f.deref());
                try list.appendSlice(f);
                if (f[f.len - 1].node == .Return) break :b;
                const ret = f[f.len - 1].dest().ty().Function.ret.deref();

                var src = ArrayList(Storage).init(ctx.allocator);
                const exprs = t.exprs.deref();
                for (exprs) |expr| {
                    const slice = try from(ctx, state, expr);
                    try list.appendSlice(slice);
                    if (slice[slice.len - 1].node == .Return) break :b;
                    try src.append(slice[slice.len - 1].dest());
                }

                try list.append(.{
                    .label = state.newChunk(),
                    .node = .{ .Call = .{
                        .f = f[f.len - 1].dest(),
                        .dst = state.alloc(ret),
                        .src = try src.toOwnedSlice(),
                    } },
                });
            },
            .If => |t| b: { // TODO: Fix all obvious mistakes
                const cond = try from(ctx, state, t.cond.deref());
                try list.appendSlice(cond);
                if (cond[cond.len - 1].node == .Return) break :b;
                const lhs = try from(ctx, state, t.lhs.deref());
                const rhs = if (t.rhs) |rhs| try from(ctx, state, rhs.deref()) else null;
                const ty = lhs[lhs.len - 1].dest().ty();
                const dst = if (rhs) |_| state.alloc(ty) else null;
                const value = rhs != null;
                const end = state.newChunk();

                // COND
                try list.append(.{ .label = state.newChunk(), .node = .{ .Jnz = .{
                    .cond = cond[cond.len - 1].dest(),
                    .dst = dst,
                    .lhs = lhs[0].label,
                    .rhs = if (rhs) |r| r[0].label else end,
                } } });

                try list.appendSlice(lhs);
                if (value) {
                    try list.append(.{
                        .label = state.newChunk(),
                        .node = .{ .Copy = .{
                            .dst = dst.?,
                            .src = lhs[lhs.len - 1].dest(),
                        } },
                    });
                    try list.append(.{
                        .label = state.newChunk(),
                        .node = .{ .Jmp = end },
                    });

                    try list.appendSlice(rhs.?);
                    try list.append(.{
                        .label = state.newChunk(),
                        .node = .{ .Copy = .{
                            .dst = dst.?,
                            .src = rhs.?[rhs.?.len - 1].dest(),
                        } },
                    });
                    try list.append(.{
                        .label = state.newChunk(),
                        .node = .{ .Jmp = end },
                    });
                }

                // END
                const node: Node = if (dst) |d| .{ .Echo = d } else .Nop;
                try list.append(.{
                    .label = end,
                    .node = node,
                });
            },
            .BinOp => |t| b: {
                const _lhs: Ast = if (t.kind == .Assign) .{ .AddrOf = t.lhs } else t.lhs.deref();
                const lhs = try from(ctx, state, _lhs);
                const rhs = try from(ctx, state, t.rhs.deref());

                try list.appendSlice(lhs);
                if (lhs[lhs.len - 1].node == .Return) break :b;
                try list.appendSlice(rhs);
                if (rhs[rhs.len - 1].node == .Return) break :b;

                const ty = switch (t.kind) {
                    .Assign => {
                        try list.append(.{ .label = state.newChunk(), .node = .{ .Store = .{
                            .dst = lhs[lhs.len - 1].dest(),
                            .src = rhs[rhs.len - 1].dest(),
                        } } });
                        break :b;
                    },
                    .Add, .Sub, .Mul => lhs[lhs.len - 1].dest().ty().min(rhs[rhs.len - 1].dest().ty()),
                    .Eq => .Bool,
                };

                try list.append(.{ .label = state.newChunk(), .node = .{ .BinOp = .{
                    .kind = t.kind,
                    .dst = state.alloc(ty),
                    .lhs = lhs[lhs.len - 1].dest(),
                    .rhs = rhs[rhs.len - 1].dest(),
                } } });
            },
            .AddrOf => |v| {
                const node = switch (v.deref()) {
                    .Identifier => |_v| .{ .Echo = .{ .named = .{ _v, ctx.get(_v).? } } },
                    else => |ast| panic("Unhandled Ast: {}", .{ast}),
                };
                try list.append(.{
                    .label = state.newChunk(),
                    .node = node,
                });
            },
            .Return => |v| b: {
                const ret = try from(ctx, state, v.deref());
                try list.appendSlice(ret);
                if (ret[ret.len - 1].node == .Return) break :b;
                try list.append(.{ .label = state.newChunk(), .node = .{ .Return = ret[ret.len - 1].dest() } });
            },
        }

        return try list.toOwnedSlice();
    }

    fn compile(self: Ir, allocator: Allocator) !void {
        switch (self.node) {
            .Nop, .Echo => {},
            //.Nop => try stdout.writeAll("\t# Nop\n"),
            //.Echo => |v| try stdout.print("\t# Echo {s}\n", .{try v.fmt(allocator)}),
            .Alloc => |v| try stdout.print("\t{s} =l alloc{d} {d}\n", .{try v.fmt(allocator), v.ty().alignment().?, v.ty().size().?}),
            .Store => |t| try stdout.print("\tstore{s} {s}, {s}\n", .{ t.dst.ty().extFmt(), try t.src.fmt(allocator), try t.dst.fmt(allocator) }),
            .Load => |t| try stdout.print("\t{s} ={s} load{s} {s}\n", .{
                try t.dst.fmt(allocator),
                t.dst.ty().baseFmt(),
                t.dst.ty().abiFmt(),
                try t.src.fmt(allocator)
            }),
            .Copy => |t| try stdout.print("\t{s} ={s} copy {s}\n", .{ try t.dst.fmt(allocator), t.dst.ty().baseFmt(), try t.src.fmt(allocator) }),
            .Call => |t| {
                try stdout.print("\t{s} ={s} call {s}(", .{
                    try t.dst.fmt(allocator),
                    t.dst.ty().baseFmt(),
                    try t.f.fmt(allocator),
                });
                for (t.src) |ir|
                    try stdout.print("{s} {s},", .{ ir.ty().abiFmt(), try ir.fmt(allocator) });
                try stdout.writeAll(")\n");
            },
            .Jmp => |v| try stdout.print("\tjmp @L{d}\n", .{v}),
            .Jnz => |t| {
                try stdout.print("\tjnz {s}, @L{d}, @L{d}\n", .{
                    try t.cond.fmt(allocator),
                    t.lhs,
                    t.rhs,
                });
            },
            .BinOp => |t| {
                try stdout.print("\t{s} ={s} {s} {s}, {s}\n", .{
                    try t.dst.fmt(allocator),
                    t.dst.ty().baseFmt(),
                    try t.kind.fmt(allocator, t.dst.ty()),
                    try t.lhs.fmt(allocator),
                    try t.rhs.fmt(allocator),
                });
            },
            .Return => |v| {
                try stdout.print("\tret {s}\n", .{try v.fmt(allocator)});
            },
        }
    }

    fn destOrNull(self: Ir) ?Storage {
        return switch (self.node) {
            .Echo, .Alloc => |v| v,
            .Store, .Load, .Copy => |t| t.dst,
            .BinOp => |t| t.dst,
            .Call => |t| t.dst,
            .Nop, .Jmp, .Jnz, .Return => null,
        };
    }

    fn dest(self: Ir) Storage {
        return self.destOrNull() orelse unreachable; // This must be enforced
    }
};
