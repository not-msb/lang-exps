const std = @import("std");
const Alloc = std.mem.Allocator;
const tools = @import("tools");
const box = tools.box;

const OptimizeError = error {
    Unoptimizable,
};

pub const Type = union(enum) {
    I64,
    Function: struct {
        prms: []const Type,
        ty: *Type,
    },
    Tuple: []const Type,

    pub fn newI64() Type {
        return .{ .I64 = undefined };
    }

    pub fn newFunction(prms: []const Type, ty: *Type) Type {
        return .{ .Function = .{ .prms = prms, .ty = ty } };
    }

    pub fn newTuple(fields: []const Type) Type {
        return .{ .Tuple = fields };
    }

    pub fn fmt(self: Type) []const u8 {
        return switch (self) {
            .I64 => "i64",
            .Function => "fn",
            .Tuple => "(,)",
        };
    }

    pub fn eq(self: Type, rhs: Type) bool {
        return switch (self) {
            .I64 => std.meta.eql(self, rhs),
            .Function => |i| switch (rhs) {
                .Function => |j| Type.eqSlice(i.prms, j.prms) and i.ty.eq(j.ty.*),
                else => false,
            },
            .Tuple => |i| switch (rhs) {
                .Tuple => |j| Type.eqSlice(i, j),
                else => false,
            },
        };
    }

    pub fn eqSlice(self: []const Type, rhs: []const Type) bool {
        for (self, rhs) |l, r|
            if (!l.eq(r)) return false;
        return true;
    }

    pub fn size(self: Type) usize {
        switch (self) {
            .I64 => return 8,
            .Function => |t| return t.ty.size(),
            .Tuple => |t| {
                var s: usize = 0;
                for (t) |i| s += i.size();
                return s;
            },
        }
    }
};

pub const Token = union(enum) {
    whitespace,
    function,
    ret,
    ty: Type,
    colon,
    semicolon,
    comma,
    l_paren,
    r_paren,
    plus,
    minus,
    identifier: []const u8,
    integer: isize,

    pub fn print(self: Token) void {
        switch (self) {
            .whitespace => std.debug.print("Token: Whitespace:\n", .{}),
            .function => std.debug.print("Token: Function\n", .{}),
            .ret => std.debug.print("Token: Ret\n", .{}),
            .ty => |v| std.debug.print("Token: Type: {any}\n", .{v}),
            .colon => std.debug.print("Token: Colon\n", .{}),
            .semicolon => std.debug.print("Token: SemiColon\n", .{}),
            .comma => std.debug.print("Token: Comma\n", .{}),
            .l_paren => std.debug.print("Token: LParen\n", .{}),
            .r_paren => std.debug.print("Token: RParen\n", .{}),
            .plus => std.debug.print("Token: Plus\n", .{}),
            .minus => std.debug.print("Token: Minus\n", .{}),
            .identifier => |v| std.debug.print("Token: Identifier: {s}\n", .{v}),
            .integer => |v| std.debug.print("Token: Integer: {d}\n", .{v}),
        }
    }
};

pub const Ast = union(enum) {
    pub const ADD = struct {
        l: *Ast,
        r: *Ast,
    };

    pub const F_DECL = struct {
        name: []const u8,
        prms: []const Ast.PARAM,
        ty: Type,
        expr: *Ast,
    };

    pub const F_CALL = struct {
        name: []const u8,
        args: []const Ast,
    };

    pub const PARAM = struct {
        name: []const u8,
        ty: Type,
    };

    identifier: []const u8,
    integer: isize,
    add: Ast.ADD,
    neg: *Ast,
    ret: *Ast,
    f_decl: Ast.F_DECL,
    f_call: Ast.F_CALL,

    pub fn print_slice(self: []const Ast, alloc: Alloc, prefix: []const u8) void {
        if (self.len == 0) return;
        for (0..self.len - 1) |i| {
            self[i].print(alloc, prefix, "├─", "| ");
        }
        self[self.len - 1].print(alloc, prefix, "└─", "  ");
    }

    pub fn print(self: Ast, alloc: Alloc, prefix: []const u8, im_prefix: []const u8, suffix: []const u8) void {
        const c_prefix = std.fmt.allocPrint(alloc, "{s}{s}", .{ prefix, suffix }) catch unreachable;

        std.debug.print("{s}{s}", .{ prefix, im_prefix });
        switch (self) {
            .identifier => |v| std.debug.print("{s}\n", .{v}),
            .integer => |v| std.debug.print("{d}\n", .{v}),
            .add => |t| {
                std.debug.print("add\n", .{});
                t.l.print(alloc, c_prefix, "├─", "| ");
                t.r.print(alloc, c_prefix, "└─", "  ");
            },
            .neg => |v| {
                std.debug.print("neg\n", .{});
                v.print(alloc, c_prefix, "└─", "  ");
            },
            .ret => |v| {
                std.debug.print("ret\n", .{});
                v.print(alloc, c_prefix, "└─", "  ");
            },
            .f_decl => |t| {
                std.debug.print("f_decl {s}\n", .{t.name});
                for (t.prms) |prm| {
                    std.debug.print("{s}└─{s}: {s}\n", .{ c_prefix, prm.name, prm.ty.fmt() });
                }
                std.debug.print("{s}{s}", .{ prefix, im_prefix });
                std.debug.print("└─{s}\n", .{t.ty.fmt()});
                t.expr.print(alloc, c_prefix, "└─", "  ");
            },
            .f_call => |t| {
                std.debug.print("f_call {s}\n", .{t.name});
                Ast.print_slice(t.args, alloc, c_prefix);
            },
        }
    }
};

pub const Statement = struct {
    next: ?*Statement = null,
    kind: union(enum) {
        Init: struct {
            dst: Register,
            value: isize,
        },
        InitLabel: struct {
            dst: Register,
            name: []const u8,
        },
        Copy: struct {
            dst: Register,
            src: Register,
        },
        Add: struct {
            dst: Register,
            src1: State,
            src2: State,
        },
        Neg: struct {
            dst: Register,
            src: State,
        },
        Call: struct {
            dst: Register,
            address: CallAddress,
            args: []Register,
        },
        Label: []const u8,
        Ret: Register,
    },

    pub fn newInit(dst: usize, value: isize) Statement {
        return .{ .kind = .{ .Init = .{ .dst = .{ .index = dst, .ty = .{ .I64 = undefined } }, .value = value } } };
    }

    pub fn newInitLabel(dst: usize, name: []const u8, ty: Type) Statement {
        return .{ .kind = .{ .InitLabel = .{ .dst = .{ .index = dst, .ty = ty }, .name = name } } };
    }

    pub fn newCopy(dst: usize, src: Register) Statement {
        return .{ .kind = .{ .Copy = .{ .dst = .{ .index = dst, .ty = src.ty }, .src = src } } };
    }

    pub fn newAdd(dst: usize, src1: State, src2: State) Statement {
        const ty = if (src1.ty().eq(src2.ty())) src1.ty() else unreachable;
        return .{ .kind = .{ .Add = .{ .dst = .{ .index = dst, .ty = ty }, .src1 = src1, .src2 = src2 } } };
    }

    pub fn newNeg(dst: usize, src: State) Statement {
        return .{ .kind = .{ .Neg = .{ .dst = .{ .index = dst, .ty = src.ty() }, .src = src } } };
    }

    pub fn newCall(dst: usize, address: CallAddress, args: []Register) Statement {
        return .{ .kind = .{ .Call = .{ .dst = Register.new(dst, address.ty().Function.ty.*), .address = address, .args = args } } };
    }

    pub fn newLabel(src: []const u8) Statement {
        return .{ .kind = .{ .Label = src } };
    }

    pub fn newRet(src: Register) Statement {
        return .{ .kind = .{ .Ret = src } };
    }

    pub fn collapse(self: Statement) ?State {
        return switch (self.kind) {
            .Neg => |t| switch (t.src) {
                .reg => null,
                .imd => |it| switch (it.ast) {
                    .integer => |v| .{ .imd = .{ .ast = .{ .integer = -v }, .ty = .I64 } },
                    else => null,
                },
            },
            else => null,
        };
    }

    pub fn usesReg(self: Statement, reg: Register) bool {
        switch (self.kind) {
            .Init => |t|
                return t.dst.index == reg.index,
            .InitLabel => |t|
                return t.dst.index == reg.index,
            .Copy => |t|
                return t.dst.index == reg.index or
                t.src.index == reg.index,
            .Add => |t|
                return t.dst.index == reg.index or
                (t.src1 == .reg and t.src1.reg.index == reg.index) or
                (t.src2 == .reg and t.src2.reg.index == reg.index),
            .Neg => |t|
                return t.dst.index == reg.index or
                (t.src == .reg and t.src.reg.index == reg.index),
            .Call => |t| {
                for (t.args) |arg|
                    if (arg.index == reg.index) return true;
                return t.dst.index == reg.index or
                    (t.address == .value and t.address.value.index == reg.index);
            },
            .Label =>
                return false,
            .Ret => |v|
                return v.index == reg.index,
        }
    }

    pub fn replaceReg(self: *Statement, dst: Register, src: Register) void {
        switch (self.kind) {
            .Init => |*t| if (t.dst.index == src.index) {
                    t.dst = dst;
            },
            .InitLabel => |*t| if (t.dst.index == src.index) {
                t.dst = dst;
            },
            .Copy => |*t| {
                if (t.dst.index == src.index) t.dst = dst;
                if (t.src.index == src.index) t.src = dst;
            },
            .Add => |*t| {
                if (t.dst.index == src.index) t.dst = dst;
                if (t.src1 == .reg and t.src1.reg.index == src.index) t.src1 = .{ .reg = dst };
                if (t.src2 == .reg and t.src2.reg.index == src.index) t.src2 = .{ .reg = dst };
            },
            .Neg => |*t| {
                if (t.dst.index == src.index) t.dst = dst;
                if (t.src == .reg and t.src.reg.index == src.index) t.src = .{ .reg = dst };
            },
            .Call => |*t| {
                for (t.args, 0..) |arg, i| if (arg.index == src.index) {
                    t.args[i] = dst;
                };
                if (t.dst.index == src.index) t.dst = dst;
                if (t.address == .value and t.address.value.index == src.index) t.address = .{ .value = dst };
            },
            .Label => {},
            .Ret => |*v| if (v.index == src.index) {
                v.* = dst;
            }
        }
    }

    pub fn directDestSource(self: *Statement) ?[2]Register {
        switch (self.kind) {
            .Copy => |*t| return [2]Register{ t.dst, t.src },
            .Add => |*t| {
                // TODO: Fix (in some way) the second register to be checked too
                if (t.src1 == .reg) {
                    return [2]Register{ t.dst, t.src1.reg };
                } else if (t.src2 == .reg) {
                    return [2]Register{ t.dst, t.src2.reg };
                } else return null;
            },
            .Neg => |*t| {
                if (t.src == .reg) {
                    return [2]Register{ t.dst, t.src.reg };
                } else return null;
            },
            else => return null,
        }
    }

    pub fn setDirectDest(self: *Statement) void {
        switch (self.kind) {
            .Copy => |*t| t.dst = t.src,
            .Add => |*t| {
                if (t.src1 == .reg) {
                    t.dst = t.src1.reg;
                } else if (t.src2 == .reg) {
                    t.dst = t.src2.reg;
                }
            },
            .Neg => |*t| {
                if (t.src == .reg) {
                    t.dst = t.src.reg;
                }
            },
            else => {},
        }
    }

    pub fn optimize(self: *Statement) ?*Statement {
        var statement = self;
        statement = statement.optimizeCopy() orelse return null;
        if (statement.optimizeDirect()) |_| {}
        return statement;
    }

    pub fn optimizeDirect(self: *Statement) ?void {
        var changed: ?void = null;
        var statement = self;

        while (true) {
            if (statement.optimizeDirectSingle()) |_|
                changed = undefined;
            statement = statement.next orelse break;
        }

        return changed;
    }

    pub fn optimizeDirectSingle(self: *Statement) ?void {
        const regs = self.directDestSource() orelse return null;
        const dst = regs[0];
        const src = regs[1];

        var start = self;
        while (start.next) |next| {
            if (next.usesReg(src)) return null;
            start = next;
        }

        start = self;
        while (start.next) |next| {
            if (next.usesReg(dst)) {
                next.replaceReg(src, dst);
            }
            start = next;
        }

        self.setDirectDest();
    }

    pub fn optimizeCopy(self: *Statement) ?*Statement {
        var changed = false;
        var start = self;
        var statement = self;

        while (statement.next) |next| {
            statement.next = next.optimizeCopySingle(&changed) orelse break;
            if (!changed) statement = statement.next.?;
        }

        return start.optimizeCopySingle(&changed);
    }

    pub fn optimizeCopySingle(self: *Statement, changed: *bool) ?*Statement {
        changed.* = false;
        if (self.kind != .Copy) return self;
        const dst = self.kind.Copy.dst;
        const src = self.kind.Copy.src;

        var start = self;
        while (start.next) |next| {
            if (next.usesReg(src)) return self;
            start = next;
        }

        start = self;
        while (start.next) |next| {
            if (next.usesReg(dst)) {
                next.replaceReg(src, dst);
            }
            start = next;
        }

        changed.* = true;
        return self.next;
    }

    pub fn dest(self: Statement) Register {
        return switch (self.kind) {
            .Init => |t| t.dst,
            .InitLabel => |t| t.dst,
            .Copy => |t| t.dst,
            .Add => |t| t.dst,
            .Call => |t| t.dst,
            .Neg => |t| t.dst,
            else => unreachable,
        };
    }

    pub fn last(self: *const Statement) *const Statement {
        var stmt = self;
        while (stmt.next) |next|
            stmt = next;
        return stmt;
    }

    pub fn lastMut(self: *Statement) *Statement {
        var stmt = self;
        while (stmt.next) |next|
            stmt = next;
        return stmt;
    }

    pub fn push(self: *Statement, alloc: Alloc, stmt: Statement) Alloc.Error!void {
        self.lastMut().next = try box(alloc, stmt);
    }

    pub fn print(self: Statement, alloc: Alloc) void {
        switch (self.kind) {
            .Init => |t| std.debug.print("Init {s} {d}\n", .{t.dst.fmt(alloc) catch unreachable, t.value}),
            .InitLabel => |t| std.debug.print("InitLabel {s} {s}\n", .{t.dst.fmt(alloc) catch unreachable, t.name}),
            .Copy => |t| std.debug.print("Copy {s} {s}\n", .{t.dst.fmt(alloc) catch unreachable, t.src.fmt(alloc) catch unreachable}),
            .Add => |t| std.debug.print("Add {s} {s} {s}\n", .{t.dst.fmt(alloc) catch unreachable, t.src1.fmt(alloc) catch unreachable, t.src2.fmt(alloc) catch unreachable}),
            .Neg => |t| std.debug.print("Neg {s} {s}\n", .{t.dst.fmt(alloc) catch unreachable, t.src.fmt(alloc) catch unreachable}),
            .Call => |t| {
                std.debug.print("Call {s} {s} ", .{t.dst.fmt(alloc) catch unreachable, t.address.fmt(alloc) catch unreachable});
                Register.printSlice(t.args, alloc) catch unreachable;
                std.debug.print("\n", .{});
            },
            .Label => |v| std.debug.print("Label: {s}\n", .{v}),
            .Ret => |v| std.debug.print("Ret {s}\n", .{v.fmt(alloc) catch unreachable}),
        }

        if (self.next) |next|
            next.print(alloc);
    }
};

pub const CallAddress = union(enum) {
    value: Register,
    named: struct {
        name: []const u8,
        ty: Type,
    },

    pub fn newValue(reg: Register) CallAddress {
        return .{ .value = reg };
    }

    pub fn newNamed(name: []const u8, t: Type) CallAddress {
        return .{ .named = .{ .name = name, .ty = t } };
    }

    pub fn ty(self: CallAddress) Type {
        return switch (self) {
            .value => |t| t.ty,
            .named => |t| t.ty,
        };
    }

    pub fn fmt(self: CallAddress, alloc: Alloc) Alloc.Error![]const u8 {
        return switch (self) {
            .value => |v| v.fmt(alloc),
            .named => |v| v.name,
        };
    }
};

pub const Register = struct {
    index: usize,
    ty: Type,

    pub fn new(index: usize, ty: Type) Register {
        return .{ .index = index, .ty = ty };
    }

    pub fn printSlice(self: []const Register, alloc: Alloc) Alloc.Error!void {
        std.debug.print("{{ ", .{});
        for (self) |reg| {
            std.debug.print("{s}, ", .{try reg.fmt(alloc)});
        }
        std.debug.print("}}", .{});
    }

    fn fmt(self: Register, alloc: Alloc) Alloc.Error![]const u8 {
        return std.fmt.allocPrint(alloc, "R{{{d}, {s}}}", .{self.index, self.ty.fmt()});
    }
};

pub const State = union(enum) {
    reg: Register,
    imd: struct {
        ast: Ast,
        ty: Type,
    },

    pub fn ty(self: State) Type {
        return switch (self) {
            .reg => |v| v.ty,
            .imd => |t| t.ty,
        };
    }

    pub fn fmt(self: State, alloc: Alloc) Alloc.Error![]const u8 {
        return switch (self) {
            .reg => |v| v.fmt(alloc),
            .imd => |t| std.fmt.allocPrint(alloc, "Imd{{ {s} }}", .{ t.ty.fmt() }),
        };
    }
};
