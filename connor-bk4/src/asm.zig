const std = @import("std");
const types = @import("types.zig");
const Statement = types.Statement;

// TODO: Write function to convert reg-index to actual location

pub fn assemble(statement: Statement) void {
    switch (statement.kind) {
        .Init => |t| std.debug.print("mov r{d}, {d}\n", .{ t.dst.index, t.value }),
        .InitLabel => |t| {
            std.debug.print("mov r{d}, {s}\n", .{ t.dst.index, t.name });
        },
        .Copy => |t| std.debug.print("mov r{d}, r{d}\n", .{ t.dst.index, t.src.index }),
        .Add => |t| {
            if (t.src1 == .reg and t.src2 == .reg) {
                if (t.dst.index != t.src1.reg.index)
                    std.debug.print("mov r{d}, r{d}\n", .{ t.dst.index, t.src1.reg.index });
                std.debug.print("add r{d}, r{d}\n", .{ t.dst.index, t.src2.reg.index });
            } if (t.src1 == .reg and t.src2 == .imd) {
                if (t.dst.index != t.src1.reg.index)
                    std.debug.print("mov r{d}, r{d}\n", .{ t.dst.index, t.src1.reg.index });
                switch (t.src2.imd.ast) {
                    .integer => |v| switch (v) {
                        0 => {},
                        1 => std.debug.print("inc r{d}\n", .{ t.dst.index }),
                        -1 => std.debug.print("dec r{d}\n", .{ t.dst.index }),
                        else => std.debug.print("add r{d}, {d}\n", .{ t.dst.index, v }),
                    },
                    else => unreachable,
                }
            } if (t.src1 == .imd and t.src2 == .reg) {
                if (t.dst.index != t.src2.reg.index)
                    std.debug.print("mov r{d}, r{d}\n", .{ t.dst.index, t.src2.reg.index });
                switch (t.src1.imd.ast) {
                    .integer => |v| switch (v) {
                        0 => {},
                        1 => std.debug.print("inc r{d}\n", .{ t.dst.index }),
                        -1 => std.debug.print("dec r{d}\n", .{ t.dst.index }),
                        else => std.debug.print("add r{d}, {d}\n", .{ t.dst.index, v }),
                    },
                    else => unreachable,
                }
            } if (t.src1 == .imd and t.src2 == .imd) {
                switch (t.src1.imd.ast) {
                    .integer => |v| {
                        const sum = v + t.src2.imd.ast.integer;
                        std.debug.print("mov r{d}, {d}\n", .{ t.dst.index, sum });
                    },
                    else => unreachable,
                }
            }
        },
        .Neg => |t| {
            switch (t.src) {
                .reg => |v| {
                    std.debug.print("mov r{d}, r{d}\n", .{ t.dst.index, v.index });
                    std.debug.print("not r{d}\n", .{ t.dst.index });
                    std.debug.print("inc r{d}\n", .{ t.dst.index });
                },
                .imd => |it| switch (it.ast) {
                    .integer => |v| {
                        const signed: isize = @intCast(v);
                        std.debug.print("mov r{d}, {d}\n", .{ t.dst.index, -signed });
                    },
                    else => unreachable,
                },
            }
        },
        .Call => |t| {
            for (t.args, 1..) |arg, i| {
                std.debug.print("mov rf{d}, r{d}\n", .{ i, arg.index });
            }
            switch (t.address) {
                .value => |sub_t| std.debug.print("call r{d}\n", .{sub_t.index}),
                .named => |sub_t| std.debug.print("call {s}\n", .{sub_t.name}),
            }
            std.debug.print("mov r{d}, r0\n", .{t.dst.index});
        },
        .Label => |v| std.debug.print("{s}:\n", .{v}),
        .Ret => |v| {
            std.debug.print("mov r0, r{d}\n", .{v.index});
            std.debug.print("ret\n", .{});
        },
    }

    if (statement.next) |stmt|
        assemble(stmt.*);
}
