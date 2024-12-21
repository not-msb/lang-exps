const std = @import("std");
const StringHashMap = std.StringHashMap;
const Alloc = std.mem.Allocator;
const tools = @import("tools");
const box = tools.box;
const types = @import("types.zig");
const Type = types.Type;
const Ast = types.Ast;
const Statement = types.Statement;
const CallAddress = types.CallAddress;
const Register = types.Register;
const State = types.State;

const Error = error {
    NotFound,
} || Alloc.Error;

const Context = struct {
    alloc: Alloc,
    reg_count: usize = 0,
    regs: StringHashMap(Register),
    ftable: StringHashMap(Type),

    fn init(alloc: Alloc) Context {
        return .{ .alloc = alloc, .regs = StringHashMap(Register).init(alloc), .ftable = StringHashMap(Type).init(alloc) };
    }

    fn deinit(self: *Context) void {
        self.regs.deinit();
        self.ftable.deinit();
    }

    fn getReg(self: *Context) usize {
        self.reg_count += 1;
        return self.reg_count;
    }

    fn clone(self: Context) Alloc.Error!Context {
        return .{
            .alloc = self.alloc,
            .reg_count = self.reg_count,
            .regs = try self.regs.clone(),
            .ftable = try self.ftable.clone(),
        };
    }
};

fn identifier(ctx: *Context, value: []const u8) Error!Statement {
    const reg = ctx.getReg();
    return if (ctx.regs.get(value)) |r|
        Statement.newCopy(reg, r)
    else if (ctx.ftable.get(value)) |ty|
        Statement.newInitLabel(reg, value, ty)
    else
        Error.NotFound;
}

fn integer(ctx: *Context, value: isize) Statement {
    const reg = ctx.getReg();
    return Statement.newInit(reg, value);
}

fn add(ctx: *Context, tuple: Ast.ADD) Error!Statement {
    const reg = ctx.getReg();

    var start: Statement = undefined;
    var ls: State = undefined;
    switch (tuple.l.*) {
        .identifier, .add, .neg, .f_call => {
            const l = try statement(ctx, tuple.l.*);
            if (l.collapse()) |collapsed| {
                ls = collapsed;
            } else {
                start = l;
                ls = .{ .reg = l.last().dest() };
            }
        },
        .integer => ls = .{ .imd = .{ .ast = tuple.l.*, .ty = .I64, } },
        else => unreachable,
    }

    var rs: State = undefined;
    switch (tuple.r.*) {
        .identifier, .add, .neg, .f_call => {
            const r = try statement(ctx, tuple.r.*);
            if (r.collapse()) |collapsed| {
                rs = collapsed;
            } else {
                try start.push(ctx.alloc, r);
                rs = .{ .reg = r.last().dest() };
            }
        },
        .integer => rs = .{ .imd = .{ .ast = tuple.r.*, .ty = .I64, } },
        else => unreachable,
    }

    const f = Statement.newAdd(reg, ls, rs);
    try start.push(ctx.alloc, f);
    return start;
}

fn neg(ctx: *Context, value: *Ast) Error!Statement {
    const reg = ctx.getReg();
    var start: ?Statement = null;

    var ns: State = undefined;
    switch (value.*) {
        .identifier, .add, .neg, .f_call => {
            const n = try statement(ctx, value.*);
            start = n;
            ns = .{ .reg = n.last().dest() };
        },
        .integer => ns = .{ .imd = .{ .ast = value.*, .ty = .I64, } },
        else => unreachable,
    }

    const f = Statement.newNeg(reg, ns);
    if (start) |*s|
        try s.push(ctx.alloc, f)
    else
        start = f;
    return start.?;
}

fn ret(ctx: *Context, value: *Ast) Error!Statement {
    var r = try statement(ctx, value.*);
    const f = Statement.newRet(r.last().dest());

    try r.push(ctx.alloc, f);
    return r;
}

fn fDecl(ctx: *Context, tuple: Ast.F_DECL) Error!Statement {
    const prms = try ctx.alloc.alloc(Type, tuple.prms.len);
    for (tuple.prms, 0..) |prm, i| {
        prms[i] = prm.ty;
    }
    const fty = Type.newFunction(prms, try box(ctx.alloc, tuple.ty));
    try ctx.ftable.put(tuple.name, fty);

    var context = try ctx.clone();
    defer context.deinit();

    for (tuple.prms) |prm|
        try context.regs.put(prm.name, Register.new(context.getReg(), prm.ty));

    var i = Statement.newLabel(tuple.name);
    const s = try fExpr(&context, tuple.expr);

    try i.push(ctx.alloc, s);
    return i;
}

fn fExpr(ctx: *Context, value: *Ast) Error!Statement {
    return switch (value.*) {
        .identifier, .integer, .add, .neg, .f_call => ret(ctx, value),
        else => unreachable,
    };
}

fn fCall(ctx: *Context, tuple: Ast.F_CALL) Error!Statement {
    var start: ?Statement = null;
    var args = try ctx.alloc.alloc(Register, tuple.args.len);
    for (tuple.args, 0..) |arg, i| {
        const stmt = try statement(ctx, arg);
        args[i] = stmt.last().dest();
        if (start) |*s|
            try s.push(ctx.alloc, stmt)
        else
            start = stmt;
    }

    const f = if (ctx.regs.get(tuple.name)) |address|
        Statement.newCall(ctx.getReg(), CallAddress.newValue(address), args)
    else if (ctx.ftable.get(tuple.name)) |fty|
        Statement.newCall(ctx.getReg(), CallAddress.newNamed(tuple.name, fty), args)
    else
        return Error.NotFound;

    if (start) |*s|
        try s.push(ctx.alloc, f)
    else
        start = f;
    return start.?;
}

pub fn statement(ctx: *Context, ast: Ast) Error!Statement {
    return switch (ast) {
        .identifier => |v| identifier(ctx, v),
        .integer => |v| integer(ctx, v),
        .add => |t| add(ctx, t),
        .neg => |v| neg(ctx, v),
        .ret => |v| ret(ctx, v),
        .f_decl => |t| fDecl(ctx, t),
        .f_call => |t| fCall(ctx, t),
    };
}

pub fn statementer(alloc: Alloc, input: []const Ast) Error![]Statement {
    const ptr = try alloc.alloc(Statement, input.len);
    var ctx = Context.init(alloc);
    defer ctx.deinit();

    for (input, 0..) |ast, i|
        ptr[i] = try statement(&ctx, ast);

    return ptr;
}
