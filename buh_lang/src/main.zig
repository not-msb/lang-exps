const std = @import("std");
const Allocator = std.mem.Allocator;
const ArrayList = std.ArrayList;
const ArrayListUnmanaged = std.ArrayListUnmanaged;
const StringHashMap = std.StringHashMap;
const EnumSet = std.EnumSet;
const EnumMap = std.EnumMap;
const EnumArray = std.EnumArray;

const Ast = @import("ast.zig").Ast;
const Ir  = @import("ir.zig").Ir;
const Context = @import("ctx.zig").Context;

pub const Graph = struct {
    allocator: Allocator,
    nodes: ArrayListUnmanaged(ArrayListUnmanaged(Ir)),
    current: usize = 0,

    pub fn init(allocator: Allocator) Allocator.Error!Graph {
        var empty = try ArrayListUnmanaged(ArrayListUnmanaged(Ir)).initCapacity(allocator, 0);
        try empty.append(allocator, try ArrayListUnmanaged(Ir).initCapacity(allocator, 0));

        return .{
            .allocator = allocator,
            .nodes = empty,
        };
    }

    pub fn append(self: Graph, val: Ir) Allocator.Error!void {
        try self.nodes.items[self.current].append(self.allocator, val);
    }

    pub fn appendCanon(self: *Graph, val: Ir) !void {
        const node = self.nodes.items[self.current];
        const last = node.getLast();

        const old_current = self.current;
        defer self.current = old_current;

        switch (last) {
            .Jmp => |v| {
                self.current = v;
                try self.appendCanon(val);
            },
            .Jnz => |t| {
                self.current = t.ctrue;
                try self.appendCanon(val);

                self.current = t.cfalse;
                try self.appendCanon(val);
            },
            else => try self.append(val),
        }
    }

    pub fn newNode(self: *Graph) Allocator.Error!usize {
        try self.nodes.append(self.allocator, try ArrayListUnmanaged(Ir).initCapacity(self.allocator, 0));
        return self.nodes.items.len-1;
    }
};

const Function = struct {
    name: []const u8,
    prms: []const Param,
    body: []const Stmt,
    //graph: []const Ir,
    graph: Graph,
    attrs: Attributes,
    ctx: Context,

    const Attributes = struct {
        inlined: bool = false,
    };

    const Param = struct {
        name: []const u8,
    };

    fn flatten(self: *Function, allocator: Allocator) !void {
        //var list = ArrayList(Ir).init(allocator);
        var ctx = Ir.Ctx{};

        //var pointer = &list;
        var graph = try Graph.init(allocator);

        for (self.body) |elem| {
            const val = try elem.expr.flatten(&ctx, &graph);
            const name = elem.name orelse continue;

            const ir = .{ .Assign = .{
                .dst = .{ .Identifier = name },
                .src = val,
            } };

            try graph.append(ir);
            //try self.ctx.put(name, try val.alloc(&self.ctx));
        }

        self.graph = graph;
        //self.graph = try list.toOwnedSlice();
        //self.ctx.prepareAsm(self.graph);
    }

    // weird and dumb name
    fn format(self: *Function, allocator: Allocator, writer: anytype, node_index: usize) !void {
        const space = (try computeSpace(allocator, self.graph))[node_index];
        const node = self.graph.nodes.items[node_index].items;

        for (space) |*constraint| {
            var iter = constraint.iterator();
            while (iter.next()) |entry| switch (entry.key) {
                .rbp, .rsp => continue,
                else => if (entry.value.* == 0) self.ctx.env.used_regs.insert(entry.key),
            };
        }

        self.ctx.env.registers = space[0];
        try writer.print(".L{d}:\n", .{node_index});

        for (node, 0..) |instr, instr_offset| {

            Ir.Val.tmp_node_index = node_index; // BAAAD
            Ir.Val.tmp_instr_offset = instr_offset; // BAAAD

            inline for (std.meta.fields(Register)) |field| {
                const reg: Register = @enumFromInt(field.value);

                const lifetime = self.ctx.env.current.get(reg);
                const replacement = switch (lifetime) {
                    0 => space[instr_offset].get(reg),
                    else => 0,
                };

                self.ctx.env.registers.set(reg, replacement);
                self.ctx.env.current.set(reg, lifetime-|1);
            }

            //std.debug.print("index: {}, offset: {}\n", .{node_index, instr_offset});
            const fmt = try instr.format(&self.ctx);
            try writer.writeAll(fmt);

            //switch (instr) {
            //    .Jmp => |v| {
            //        const current = self.ctx.env.current;
            //        try self.format(allocator, writer, v);
            //        self.ctx.env.current = current;
            //    },
            //    .Jnz => |t| {
            //        const current = self.ctx.env.current;
            //        try self.format(allocator, writer, t.ctrue);
            //        self.ctx.env.current = current;
            //        try self.format(allocator, writer, t.cfalse);
            //        self.ctx.env.current = current;
            //    },
            //    else => {}
            //}
        }
    }

    fn print(self: *Function) !void {
        const allocator = self.ctx.symbols.allocator;
        const stdout = std.io.getStdOut().writer();
        try stdout.print("{s}:\n", .{self.name});

        var list = ArrayList(u8).init(allocator);
        const writer = list.writer();

        Ir.Val.tmp_graph = self.graph; // BAAAD
        for (0..self.graph.nodes.items.len) |node_index| {
            try self.format(allocator, writer, node_index);
        }

        //
        //const space = try computeSpace(allocator, self.graph);

        //for (space) |elem| {
        //    var iter = elem.iterator();
        //    while (iter.next()) |entry| {
        //        self.ctx.env.used_regs.set(entry.key);
        //    }
        //}

        //self.ctx.env.registers = space[0];

        //for (self.graph, 0..) |elem, i| {
        //    Ir.Val.tmp_lifetime_offset = self.graph[i..];

        //    inline for (std.meta.fields(Register)) |field| {
        //        const reg: Register = @enumFromInt(field.value);

        //        const value = self.ctx.env.current.get(reg) orelse 0;
        //        const replacement = switch (value) {
        //            0 => space[i].get(reg).?,
        //            else => 0,
        //        };

        //        self.ctx.env.registers.put(reg, replacement);

        //        switch (value) {
        //            0 => self.ctx.env.current.remove(reg),
        //            else => |time| self.ctx.env.current.put(reg, time-1),
        //        }
        //    }

        //    const fmt = try elem.format(&self.ctx);
        //    try writer.writeAll(fmt);
        //}
        //

        const slice = try list.toOwnedSlice();

        var regs = ArrayList(Register).init(allocator);
        var regs_iter = self.ctx.env.used_regs.iterator();
        while (regs_iter.next()) |reg|
            try regs.append(reg);

        for (regs.items) |reg|
            try stdout.print("\tpush {s}\n", .{@tagName(reg)});

        var buffer = try allocator.alloc([]const u8, regs.items.len);
        defer allocator.free(buffer);

        std.mem.reverse(Register, regs.items);
        for (regs.items, 0..) |reg, i|
            buffer[i] = try std.fmt.allocPrint(allocator, "\tpop  {s}\n", .{@tagName(reg)});

        const retaddr = try std.fmt.allocPrint(allocator, "[{d}+rsp]", .{8+8*regs.items.len+8*self.prms.len});
        const offaddr = try std.fmt.allocPrint(allocator, "{d}", .{8*regs.items.len});
        const fmt = try std.mem.concat(allocator, u8, buffer);

        var out = slice;
        out = try std.mem.replaceOwned(u8, allocator, out, "{s}", fmt);
        out = try std.mem.replaceOwned(u8, allocator, out, "{d}", retaddr);
        out = try std.mem.replaceOwned(u8, allocator, out, "{off}", offaddr);
        try stdout.writeAll(out);
    }
};

// This does *not* handle cyclic graphs
fn findFirstReg(reg: Register, graph: Graph, node_index: usize, instr_offset: usize) ?usize {
    const list = graph.nodes.items[node_index].items[instr_offset..];

    for (list, 0..) |elem, i| switch (elem) {
        .Assign => |t| {
            const cond =
                t.dst == .Fixed and
                t.dst.Fixed == .register and
                t.dst.Fixed.register == reg;
            if (cond) return i;
        },
        .Jnz => |t| {
            const t_limit = findFirstReg(reg, graph, t.ctrue, 0);
            const f_limit = findFirstReg(reg, graph, t.cfalse, 0);

            return if (t_limit == null and f_limit == null)
                null
            else if (t_limit == null)
                f_limit.? + i
            else if (f_limit == null)
                t_limit.? + i
            else
                @min(t_limit.?, f_limit.?) + i;
        },
        .Jmp => |v| {
            const limit = findFirstReg(reg, graph, v, 0) orelse return null;
            return limit + i;
        },
        .Call => |t| {
            const cond =
                t.dst == .Fixed and
                t.dst.Fixed == .register and
                t.dst.Fixed.register == reg;
            if (cond) return i;
        },
        .CallC => |t| {
            const cond =
                t.dst == .Fixed and
                t.dst.Fixed == .register and
                t.dst.Fixed.register == reg;
            if (cond) return i;
        },
        .Asm => |t| {
            for (t.regs) |scratch|
                if (scratch == reg) return i;
            const dst = t.out orelse continue;
            if (dst == reg) return i;
        },
        .Ret => continue,
    };

    return null;
}

// computes lifetimes within a function
const Constraint = EnumArray(Register, usize);
const SubSpace = []Constraint;
const Space = []SubSpace;

fn computeSpace(allocator: Allocator, graph: Graph) Allocator.Error!Space {
    const space = try allocator.alloc(SubSpace, graph.nodes.items.len);

    for (space, graph.nodes.items, 0..) |*subspace, node, node_index| {
        subspace.* = try allocator.alloc(Constraint, node.items.len);

        for (subspace.*, 0..) |*constraint, instr_offset| {
            constraint.* = Constraint.initFill(0);

            inline for (std.meta.fields(Register)) |field| {
                const reg: Register = @enumFromInt(field.value);
                if (reg == .rbp or reg == .rsp) continue;

                const lifetime = findFirstReg(reg, graph, node_index, instr_offset) orelse std.math.maxInt(usize);
                constraint.set(reg, lifetime);
            }
        }
    }

    return space;
}

//fn computeSpace(allocator: Allocator, list: []const Ir) Allocator.Error![]EnumMap(Register, usize) {
//    const maps = try allocator.alloc(EnumMap(Register, usize), list.len);
//    //var used = EnumMap(Register, ArrayList(Val)).init(.{});
//
//    for (maps, 0..) |*map, i| {
//        map.* = EnumMap(Register, usize).init(.{ .rbp = 0, .rsp = 0 });
//
//        inline for (std.meta.fields(Register)) |field| {
//            const reg: Register = @enumFromInt(field.value);
//            switch (reg) {
//                .rbp, .rsp => continue,
//                else => {},
//            }
//
//            const limit = limit: for (list[i..], 0..) |elem, j| switch (elem) {
//                .Assign => |t| {
//                    const cond =
//                        t.dst == .Fixed and
//                        t.dst.Fixed == .register and
//                        t.dst.Fixed.register == reg;
//                    if (cond) break j;
//                },
//                .Call => |t| {
//                    const cond =
//                        t.dst == .Fixed and
//                        t.dst.Fixed == .register and
//                        t.dst.Fixed.register == reg;
//                    if (cond) break j;
//                },
//                .CallC => |t| {
//                    const cond =
//                        t.dst == .Fixed and
//                        t.dst.Fixed == .register and
//                        t.dst.Fixed.register == reg;
//                    if (cond) break j;
//                },
//                .Asm => |t| {
//                    for (t.regs) |scratch|
//                        if (scratch == reg) break :limit j;
//                    const dst = t.out orelse continue;
//                    if (dst == reg) break j;
//                },
//                else => continue,
//            } else std.math.maxInt(usize);
//
//            putNoClobber(map, reg, limit);
//        }
//    }
//
//    return maps;
//}

fn putNoClobber(map: *EnumMap(Register, usize), key: Register, value: usize) void {
    //std.debug.assert(!set.contains(key));
    if (map.contains(key))
        std.log.info("Found duplicate of {} in computeSpace", .{key});
    map.put(key, value);
}

const Stmt = struct {
    name: ?[]const u8,
    expr: Ast,
};

pub const Register = enum {
    rax,
    rbx,
    rcx,
    rdx,
    rdi,
    rsi,
    rbp,
    rsp,
    r8,
    r9,
    r10,
    r11,
    r12,
    r13,
    r14,
    r15,
};

pub const Value = union(enum) {
    integer: usize,
    register: Register,
    stack: isize,
    label: []const u8,

    pub fn new(ctx: *Context, lifetime: usize) ?Value {
        if (lifetime == 0)
            return null;

        inline for (std.meta.fields(Register)) |tag| {
            const reg: Register = @enumFromInt(tag.value);
            const time = ctx.env.registers.get(reg);
            if (lifetime <= time) {
                ctx.env.used_regs.insert(reg);
                ctx.env.registers.set(reg, 0);
                ctx.env.current.set(reg, lifetime);
                return .{ .register = reg };
            }
        }

        //ctx.env.stack += 1;
        //return .{ .stack = @intCast(ctx.env.stack) };

        return null;
    }

    pub fn eql(self: Value, other: Value) bool {
        return switch (self) {
            .label => |v| other == .label and std.mem.eql(u8, v, other.label),
            else => std.meta.eql(self, other),
        };
    }

    pub fn format(self: Value, allocator: Allocator) Allocator.Error![]const u8 {
        return switch (self) {
            .integer => |int| std.fmt.allocPrint(allocator, "{d}", .{int}),
            .register => |reg| @tagName(reg),
            .stack => |mem| {
                const fmt = if (mem > 0) "{off}" else "";
                return std.fmt.allocPrint(allocator, "[{s}+{d}+rsp]", .{fmt, mem});
            },
            .label => |name| name,
        };
    }
};

pub fn main() !void {
    const allocator = std.heap.page_allocator;

    var ctx = Context.new(allocator);
    try ctx.put("main", .{ .label = "main" });
    try ctx.put("second", .{ .label = "second" });
    try ctx.put("add", .{ .label = "add" });
    try ctx.put("derefSetb", .{ .label = "derefSetb" });
    try ctx.put("c_malloc", .{ .label = "c_malloc" });
    try ctx.put("syscall3", .{ .label = "syscall3" });

    try ctx.put("const_buffer", .{ .label = "const_buffer" });

    var function = Function{
        .name = "main",
        .prms = &.{},
        .body = &.{
            Stmt{
                .name = "buh",
                .expr = .{ .Asm = .{
                    .text = "mov rax, 420\n",
                    .regs = &.{ .rax },
                    .out = .rax,
                } },
            },

            Stmt{
                .name = "tmp",
                .expr = .{ .Asm = .{
                    .text = "mov rbx, 69\n",
                    .regs = &.{},
                    .out = .rbx,
                } },
            },

            //Stmt{
            //    .name = "buffer",
            //    .expr = .{
            //        .Call = .{
            //            .name = &.{ .Identifier = "c_malloc" },
            //            .args = &.{ .{ .Integer = 16 } },
            //        },
            //    }
            //},
            Stmt{
                .name = null,
                .expr = .{ .Call = .{
                        .name = &.{ .Identifier = "derefSetb" },
                        .args = &.{ .{ .Identifier = "const_buffer" }, .{ .Integer = 'H' } },
                }},
            },
            Stmt{
                .name = null,
                .expr = .{ .Call = .{
                        .name = &.{ .Identifier = "derefSetb" },
                        .args = &.{
                            .{ .Call = .{ .name = &.{ .Identifier = "add" }, .args = &.{ .{ .Identifier = "const_buffer" }, .{ .Integer = 1 } } } },
                            .{ .Integer = 'e' }
                        },
                }},
            },
            Stmt{
                .name = null,
                .expr = .{ .Call = .{
                        .name = &.{ .Identifier = "derefSetb" },
                        .args = &.{
                            .{ .Call = .{ .name = &.{ .Identifier = "add" }, .args = &.{ .{ .Identifier = "const_buffer" }, .{ .Integer = 2 } } } },
                            .{ .Integer = 'l' }
                        },
                }},
            },
            Stmt{
                .name = null,
                .expr = .{ .Call = .{
                        .name = &.{ .Identifier = "derefSetb" },
                        .args = &.{
                            .{ .Call = .{ .name = &.{ .Identifier = "add" }, .args = &.{ .{ .Identifier = "const_buffer" }, .{ .Integer = 3 } } } },
                            .{ .Integer = 'l' }
                        },
                }},
            },
            Stmt{
                .name = null,
                .expr = .{ .Call = .{
                        .name = &.{ .Identifier = "derefSetb" },
                        .args = &.{
                            .{ .Call = .{ .name = &.{ .Identifier = "add" }, .args = &.{ .{ .Identifier = "const_buffer" }, .{ .Integer = 4 } } } },
                            .{ .Integer = 'o' }
                        },
                }},
            },
            Stmt{
                .name = null,
                .expr = .{ .Call = .{
                        .name = &.{ .Identifier = "derefSetb" },
                        .args = &.{
                            .{ .Call = .{ .name = &.{ .Identifier = "add" }, .args = &.{ .{ .Identifier = "const_buffer" }, .{ .Integer = 5 } } } },
                            .{ .Integer = '!' }
                        },
                }},
            },
            Stmt{
                .name = null,
                .expr = .{ .Call = .{
                        .name = &.{ .Identifier = "derefSetb" },
                        .args = &.{
                            .{ .Call = .{ .name = &.{ .Identifier = "add" }, .args = &.{ .{ .Identifier = "const_buffer" }, .{ .Integer = 6 } } } },
                            .{ .Integer = '\n' }
                        },
                }},
            },
            Stmt{
                .name = null,
                .expr = .{ .Call = .{
                    .name = &.{ .Identifier = "syscall3" },
                    .args = &.{
                        .{ .Integer = 1 },
                        .{ .Integer = 1 },
                        .{ .Identifier = "const_buffer" },
                        .{ .Integer = 7 },
                    },
                }},
            },
            Stmt{
                .name = null,
                .expr = .{
                    .Ret = &.{ .Integer = 0 }
                }
            },
        },
        .graph = undefined,
        .attrs = .{},
        .ctx = ctx.child(),
    };

    var second = Function{
        .name = "second",
        .prms = &.{
            .{ .name = "x" },
        },
        .body = &.{
            Stmt{
                .name = null,
                .expr = .{ .Ret = &.{ .Identifier = "x" } },
            },
        },
        .graph = undefined,
        .attrs = .{},
        .ctx = ctx.child(),
    };
    try second.ctx.put("x", .{ .stack = 8 });

    var add = Function{
        .name = "add",
        .prms = &.{
            .{ .name = "a" },
            .{ .name = "b" },
        },
        .body = &.{
            Stmt{
                .name = null,
                .expr = .{ .Ret = &.{ .Asm = .{
                    .text = "mov @out, %a\nadd @out, %b\n",
                    .regs = &.{ .rax },
                    .out = .rax,
                }} },
            },
        },
        .graph = undefined,
        .attrs = .{},
        .ctx = ctx.child(),
    };
    try add.ctx.put("a", .{ .stack = 16 });
    try add.ctx.put("b", .{ .stack = 8 });

    var malloc = Function{
        .name = "c_malloc",
        .prms = &.{
            .{ .name = "size" },
        },
        .body = &.{
            Stmt{
                .name = "out",
                .expr = .{ .Asm = .{
                    .text = "extern malloc\nmov rdi, %size\ncall malloc\n",
                    .regs = &.{ .rax, .rcx, .rdx, .rdi, .rsi, .r8, .r9, .r10, .r11 },   // TODO: *REALLY* check if this is correct
                    .out = .rax,                                                        // Based on a very short internet search on gcc
                }},
            },
            Stmt{
                .name = null,
                .expr = .{ .Ret = &.{ .Identifier = "out" } },
            },
        },
        .graph = undefined,
        .attrs = .{},
        .ctx = ctx.child(),
    };
    try malloc.ctx.put("size", .{ .stack = 8 });

    var derefSetb = Function{
        .name = "derefSetb",
        .prms = &.{
            .{ .name = "ptr" },
            .{ .name = "value" },
        },
        .body = &.{
            Stmt{
                .name = null,
                .expr = .{ .Asm = .{
                    .text = "mov rax, %ptr\nmov bl, %value\nmov [rax], bl\n", // Assumes lsb
                    .regs = &.{ .rax, .rbx },
                    .out = null,
                }},
            },
            Stmt{
                .name = null,
                .expr = .{ .Ret = &.{ .Integer = 0 } },
            },
        },
        .graph = undefined,
        .attrs = .{},
        .ctx = ctx.child(),
    };
    try derefSetb.ctx.put("ptr", .{ .stack = 16 });
    try derefSetb.ctx.put("value", .{ .stack = 8 });

    var syscall3 = Function{
        .name = "syscall3",
        .prms = &.{
            .{ .name = "inst" },
            .{ .name = "arg0" },
            .{ .name = "arg1" },
            .{ .name = "arg2" },
        },
        .body = &.{
            Stmt{
                .name = "out",
                .expr = .{ .Asm = .{
                    .text = "mov rax, %inst\nmov rdi, %arg0\nmov rsi, %arg1\nmov rdx, %arg2\nsyscall\n",
                    .regs = &.{ .rax, .rdi, .rsi, .rdx },
                    .out = .rax,
                }},
            },
            Stmt{
                .name = null,
                .expr = .{ .Ret = &.{ .Identifier = "out" } },
            },
        },
        .graph = undefined,
        .attrs = .{},
        .ctx = ctx.child(),
    };
    try syscall3.ctx.put("inst", .{ .stack = 32 });
    try syscall3.ctx.put("arg0", .{ .stack = 24 });
    try syscall3.ctx.put("arg1", .{ .stack = 16 });
    try syscall3.ctx.put("arg2", .{ .stack = 8 });

    var conditionals = Function{
        .name = "main",
        .prms = &.{
        },
        .body = &.{
            Stmt{
                .name = "wow",
                .expr = .{ .If = .{
                    .cond = &.{ .Integer = 10 },
                    .lhs = &.{ .Integer = 20 },
                    .rhs = &.{ .Integer = 30 },
                }},
            },
            Stmt{
                .name = null,
                .expr = .{ .Ret = &.{ .Identifier = "wow" } },
            },
        },
        .graph = undefined,
        .attrs = .{},
        .ctx = ctx.child(),
    };

    try function.flatten(allocator);
    try second.flatten(allocator);
    try add.flatten(allocator);
    try malloc.flatten(allocator);
    try derefSetb.flatten(allocator);
    try syscall3.flatten(allocator);
    try conditionals.flatten(allocator);

    //{
    //    //try @import("pretty.zig").print(allocator, function.graph, .{});
    //    const list = try computeSpace(allocator, function.graph);
    //    for (list, 0..) |*elem, i| {
    //        var iterator = elem.iterator();
    //        while (iterator.next()) |entry| switch (entry.value.*) {
    //            std.math.maxInt(usize) =>
    //                std.debug.print("{s}:inf, ", .{@tagName(entry.key)}),
    //            else => |n|
    //                std.debug.print("{s}:{x}, ", .{@tagName(entry.key), n}),
    //        };
    //        std.debug.print("\n", .{});
    //        try @import("pretty.zig").print(allocator, function.graph[i], .{});
    //    }
    //}

    //try function.print();
    //try second.print();
    //try add.print();
    ////try malloc.print();
    //try derefSetb.print();
    //try syscall3.print();
    try conditionals.print();
    //try @import("pretty.zig").print(allocator, conditionals.graph.nodes, .{ .max_depth = 0 });

    const stdout = std.io.getStdOut().writer();
    try stdout.print("section .bss\nconst_buffer: resb 16\n", .{});




    //nst graph = b: {
    //  var graph = try Graph.init(allocator);

    //  // L0
    //  try graph.append(.{ .Assign = .{ .dst = .{ .Identifier = "a" }, .src = .{ .Integer = 10 } } });
    //  try graph.append(.{ .Jnz = .{ .cond = .{ .Identifier = "a" }, .ctrue = 1, .cfalse = 2 } });

    //  //L1
    //    graph.current = try graph.newNode();
    //    try graph.append(.{ .Assign = .{ .dst = .{ .Identifier = "a" }, .src = .{ .Integer = 20 } } });
    //    try graph.append(.{ .Assign = .{ .dst = .{ .Fixed = .{ .register = .rax } }, .src = .{ .Identifier = "a" } } });
    //    try graph.append(.{ .Jmp = 3 });

    //    //L2
    //    graph.current = try graph.newNode();
    //    try graph.append(.{ .Assign = .{ .dst = .{ .Fixed = .{ .register = .rax } }, .src = .{ .Identifier = "a" } } });
    //    try graph.append(.{ .Jmp = 3 });

    //    //L3
    //    graph.current = try graph.newNode();
    //    //try graph.append(.{ .Ret = .{ .Identifier = "a" } });
    //    try graph.append(.{ .Ret = .{ .Fixed = .{ .register = .rax } } });

    //    break :b graph;
    //};

    //var newfunc = Function{
    //    .name = "newfunc",
    //    .prms = &.{},
    //    .body = &.{
    //    },
    //    .graph = graph,
    //    .attrs = .{},
    //    .ctx = ctx.child(),
    //};

    //try newfunc.print();



    //const space = try computeSpace(allocator, graph);
    ////const first = findFirstReg(.rbx, graph, 0, 0);
    ////try @import("pretty.zig").print(allocator, graph, .{ .max_depth = 0 });
    ////try @import("pretty.zig").print(allocator, first, .{ .max_depth = 0 });

    //inline for (std.meta.fields(Register)) |field| {
    //    const reg: Register = @enumFromInt(field.value);
    //    std.debug.print("[{d: >2}]: {s}\n", .{field.value, @tagName(reg)});
    //}
    ////try @import("pretty.zig").print(allocator, std.meta.fields(Register), .{ .max_depth = 0 });
    //try @import("pretty.zig").print(allocator, space, .{ .max_depth = 0 });
}
