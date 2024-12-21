const std = @import("std");

pub const Type = union(enum) {
    CompInt,
    U8,
    I32,
    Function: struct {
        prms: []const Type,
        ty: *Type,
    },

    pub fn size(self: Type) usize {
        return switch (self) {
            .CompInt, .Function => unreachable,
            .U8 => 1,
            .I32 => 4,
        };
    }

    pub fn eq(self: Type, other: Type) bool {
        return switch (self) {
            .CompInt => switch (other) {
                .CompInt, .U8, .I32 => true,
                else => false,
            },
            .U8 => switch (other) {
                .U8, .CompInt => true,
                else => false,
            },
            .I32 => switch (other) {
                .I32, .U8, .CompInt => true,
                else => false,
            },
            .Function => other == .Function, // Undefined, temporary patch, should be fixed
        };
    }

    pub fn eqLossy(self: Type, other: Type) ?Type {
        return switch (self) {
            .CompInt => switch (other) {
                .CompInt, .U8, .I32 => .I32,
                else => null,
            },
            .U8 => switch (other) {
                .U8, .CompInt => .U8,
                .I32 => .I32,
                else => null,
            },
            .I32 => switch (other) {
                .I32, .U8, .CompInt => .I32,
                else => null,
            },
            .Function => null, // Undefined, temporary patch, should be fixed
        };
    }

    pub fn eqExact(self: Type, other: Type) bool {
        return switch (self) {
            .CompInt => other == .CompInt,
            .U8 => other == .U8,
            .I32 => other == .I32,
            .Function => other == .Function, // Undefined, temporary patch, should be fixed
        };
    }

    pub fn fmt(self: Type) []const u8 {
        return switch (self) {
            .CompInt => "CompInt",
            .U8 => "U8",
            .I32 => "I32",
            .Function => "Fn",
        };
    }

    pub fn fmtAsmSize(self: Type) []const u8 {
        return switch (self) {
            .CompInt, .Function => unreachable,
            .U8 => "byte",
            .I32 => "dword",
        };
    }

    pub fn fmtReg(self: Type) []const u8 {
        return switch (self) {
            .CompInt, .Function => unreachable,
            .U8 => "al",
            .I32 => "eax",
        };
    }
};

pub const Token = union(enum) {
    function,
    ret,
    constant,
    ty: Type,
    colon,
    semicolon,
    comma,
    l_paren,
    r_paren,
    l_bracket,
    r_bracket,
    equal,
    plus,
    minus,
    star,
    slash,
    identifier: []const u8,
    integer: usize,
};

pub const Ast = union(enum) {
    pub const Assign = struct {
        assignee: []const u8,
        assigner: *Ast,
        ty: Type,
    };

    pub const BinOp = struct {
        l: *Ast,
        r: *Ast,
    };

    pub const FDecl = struct {
        name: []const u8,
        prms: []const Ast.Param,
        ty: Type,
        expr: *Ast,
    };

    pub const FCall = struct {
        name: []const u8,
        args: []const Ast,
    };

    pub const Param = struct {
        name: []const u8,
        ty: Type,
    };

    identifier: []const u8,
    integer: usize,
    assign: Assign,
    add: Ast.BinOp,
    sub: Ast.BinOp,
    mul: Ast.BinOp,
    div: Ast.BinOp,
    block: []const Ast,
    ret: *Ast,
    f_decl: Ast.FDecl,
    f_call: Ast.FCall,

    pub fn print(self: Ast, depth: usize) void {
        for (0..depth) |_|
            std.debug.print("  ", .{});

        switch (self) {
            .identifier => |v| std.debug.print("{s}\n", .{v}),
            .integer => |v| std.debug.print("{d}\n", .{v}),
            .assign => |t| {
                std.debug.print("assign\n", .{});
                for (0..depth + 1) |_|
                    std.debug.print("  ", .{});
                std.debug.print("{s}\n", .{t.assignee});
                t.assigner.print(depth + 1);
            },
            .add => |t| {
                std.debug.print("add\n", .{});
                t.l.print(depth + 1);
                t.r.print(depth + 1);
            },
            .sub => |t| {
                std.debug.print("sub\n", .{});
                t.l.print(depth + 1);
                t.r.print(depth + 1);
            },
            .mul => |t| {
                std.debug.print("mul\n", .{});
                t.l.print(depth + 1);
                t.r.print(depth + 1);
            },
            .div => |t| {
                std.debug.print("div\n", .{});
                t.l.print(depth + 1);
                t.r.print(depth + 1);
            },
            .block => |v| {
                std.debug.print("block\n", .{});
                for (v) |ast|
                    ast.print(depth + 1);
            },
            .ret => |v| {
                std.debug.print("ret\n", .{});
                v.print(depth + 1);
            },
            .f_decl => |t| {
                std.debug.print("decl {s}: {s}\n", .{ t.name, t.ty.fmt() });

                for (0..depth + 1) |_|
                    std.debug.print("  ", .{});
                std.debug.print("prms:\n", .{});

                for (t.prms) |prm| {
                    for (0..depth + 2) |_|
                        std.debug.print("  ", .{});
                    std.debug.print("{s}: {s}\n", .{ prm.name, prm.ty.fmt() });
                }

                t.expr.print(depth + 1);
            },
            .f_call => |t| {
                std.debug.print("call {s}\n", .{t.name});
                for (t.args) |arg|
                    arg.print(depth + 1);
            },
        }
    }
};

pub const IrFunction = struct {
    name: []const u8,
    prms: []const Ast.Param,
    ty: Type,
    codes: []const IrCode,
};

pub const IrCode = union(enum) {
    const BinOp = struct {
        dst: IrValue,
        src: IrValue,
    };

    const TriOp = struct {
        dst: IrValue,
        src1: IrValue,
        src2: IrValue,
    };

    Copy: BinOp,
    Add: TriOp,
    Sub: TriOp,
    Mul: TriOp,
    Div: TriOp,
    Call: struct {
        dst: IrValue,
        address: []const u8,
        args: []IrValue,
    },
    Return: IrValue,

    pub fn dest(self: IrCode) ?IrValue {
        return switch (self) {
            .Copy => |t| t.dst,
            .Add, .Sub, .Mul, .Div => |t| t.dst,
            .Call => |t| t.dst,
            .Return => null,
        };
    }

    pub fn contains(self: IrCode, value: IrValue) ?Type {
        return switch (self) {
            .Copy => |t| if (t.dst.eq(value)) t.dst.ty else if (t.src.eq(value)) t.src.ty else null,
            .Add, .Sub, .Mul, .Div => |t| if (t.dst.eq(value)) t.dst.ty else if (t.src1.eq(value)) t.src1.ty else if (t.src2.eq(value)) t.src2.ty else null,
            .Call => |t| if (t.dst.eq(value)) t.dst.ty else {
                for (t.args) |arg|
                    if (arg.eq(value)) return arg.ty;
                return null;
            },
            .Return => |v| if (v.eq(value)) v.ty else null,
        };
    }

    pub fn replaceValue(self: *IrCode, dst: IrValue, value: IrValue) void {
        switch (self.*) {
            .Copy => |*t| {
                if (t.dst.eq(value)) t.dst = dst;
                if (t.src.eq(value)) t.src = dst;
            },
            .Add, .Sub, .Mul, .Div => |*t| {
                if (t.dst.eq(value)) t.dst = dst;
                if (t.src1.eq(value)) t.src1 = dst;
                if (t.src2.eq(value)) t.src2 = dst;
            },
            .Call => |*t| {
                if (t.dst.eq(value)) t.dst = dst;
                for (t.args) |*arg| {
                    if (arg.eq(value)) arg.* = dst;
                }
            },
            .Return => |*v| {
                if (v.eq(value)) v.* = dst;
            },
        }
    }

    pub fn print(self: IrCode) void {
        switch (self) {
            .Copy => |t| std.debug.print("Copy ({s}, {}) ({s}, {})\n", .{ @tagName(t.dst.ty), t.dst.kind, @tagName(t.src.ty), t.src.kind }),
            .Add => |t| std.debug.print("Add ({s}, {}) ({s}, {}) ({s}, {})\n", .{ @tagName(t.dst.ty), t.dst.kind, @tagName(t.src1.ty), t.src1.kind, @tagName(t.src2.ty), t.src2.kind }),
            .Sub => |t| std.debug.print("Sub ({s}, {}) ({s}, {}) ({s}, {})\n", .{ @tagName(t.dst.ty), t.dst.kind, @tagName(t.src1.ty), t.src1.kind, @tagName(t.src2.ty), t.src2.kind }),
            .Mul => |t| std.debug.print("Mul ({s}, {}) ({s}, {}) ({s}, {})\n", .{ @tagName(t.dst.ty), t.dst.kind, @tagName(t.src1.ty), t.src1.kind, @tagName(t.src2.ty), t.src2.kind }),
            .Div => |t| std.debug.print("Div ({s}, {}) ({s}, {}) ({s}, {})\n", .{ @tagName(t.dst.ty), t.dst.kind, @tagName(t.src1.ty), t.src1.kind, @tagName(t.src2.ty), t.src2.kind }),
            .Call => |t| std.debug.print("Call ({s}, {}) {s} {any}\n", .{ @tagName(t.dst.ty), t.dst.kind, t.address, t.args }),
            .Return => |v| std.debug.print("Return ({s}, {})\n", .{ @tagName(v.ty), v.kind }),
        }
    }
};

pub const IrValue = struct {
    pub const Kind = union(enum) {
        named: []const u8,
        indexed: usize,
        value: usize,
    };

    ty: Type,
    kind: Kind,

    pub fn newNamed(ty: Type, value: []const u8) IrValue {
        return .{ .ty = ty, .kind = .{ .named = value } };
    }

    pub fn newIndexed(ty: Type, value: usize) IrValue {
        return .{ .ty = ty, .kind = .{ .indexed = value } };
    }

    pub fn newValue(ty: Type, value: usize) IrValue {
        return .{ .ty = ty, .kind = .{ .value = value } };
    }

    pub fn eq(self: IrValue, other: IrValue) bool {
        return switch (self.kind) {
            .named => |v| other.kind == .named and std.mem.eql(u8, v, other.kind.named),
            .indexed => |v| other.kind == .indexed and other.kind.indexed == v,
            .value => |v| other.kind == .value and other.kind.value == v,
        };
    }
};
