const std = @import("std");
const lib = @import("lib.zig");
const Allocator = std.mem.Allocator;
const StringHashMap = std.StringHashMap;
const ArrayList = std.ArrayList;
const reverseIterator = std.mem.reverseIterator;
const ParseError = lib.Error;
const Token = lib.token.Token;
const Function = lib.ir.Function;
const Context = lib.Context;
const Pair = lib.Pair;
const Type = lib.Type;
const Block = lib.ir.Block;
const Var = lib.Var;
const Cursor = lib.TokenCursor;

pub const File = struct {
    externs: []const Extern,
    consts: []const Const,
    functions: []Function,
    order: []const Order,

    const Order = union(enum) {
        ext: usize,
        con: usize,
        fun: usize,
    };

    const Extern = struct {
        name: []const u8,
        ty: Type,
    };

    const Const = struct {
        dst: []const u8,
        src: *Ast,
        ty: *Ast,
    };

    pub fn scan(file: *const File, allocator: Allocator) ParseError!void {
        const local = try Context.init(file, allocator);
        var context = try lib.box(allocator, local);

        for (file.order) |order| {
            switch (order) {
                .ext => |v| {
                    const ext = file.externs[v];
                    try context.put(ext.name, .{
                        .scope = .global,
                        .access = switch (ext.ty) {
                            .Function => .direct,
                            else => .deferred,
                        },
                        .tag = .{ .named = ext.name },
                        .ty = ext.ty,
                    });
                },
                .con => |v| {
                    const con = file.consts[v];
                    const eval_ty = try con.ty.eval(context);
                    const eval_tag = try con.src.eval(context);

                    try context.put(con.dst, .{
                        .scope = .compile_time,
                        .access = .direct,
                        .tag = eval_tag.tag,
                        .ty = eval_ty.tag.ty,
                    });
                },
                .fun => |v| {
                    const function = &file.functions[v];
                    const is_comptime = function.attr.is_comptime;

                    // TODO: Again... improve this
                    lib.global_error = function.loc;

                    if (function.ret.onlyComptime() and !is_comptime)
                        return ParseError.NonComptimeFunc;
                    for (function.params.types) |ty|
                        if (ty.onlyComptime() and !is_comptime)
                            return ParseError.NonComptimeArg;

                    try context.put(function.name, .{
                        .scope = if (is_comptime) .compile_time else .global,
                        .access = .direct,
                        .tag = .{ .named = function.name },
                        .ty = .{ .Function = .{
                            .params = function.params.types,
                            .ret = try lib.box(allocator, function.ret),
                        } },
                    });

                    function.ctx = try context.child(function.ret);

                    for (function.params.names, function.params.types) |name, ty| {
                        try function.ctx.put(name, .{
                            .scope = if (is_comptime) .compile_time else .local,
                            .access = .direct,
                            .tag = .{ .named = name },
                            .ty = ty,
                        });
                    }

                    const ret = try function.ctx.check(function.tree);
                    if (!ret.coercible(function.ret)) return ParseError.TypeIncompatible;
                },
            }
        }
    }
};

pub const Stmt = union(enum) {
    Ast: Ast,
    Declare: struct {
        dst: []const u8,
        src: *Ast,
        ty: *Ast,
        is_comptime: bool,
    },
};

pub const Ast = union(enum) {
    Integer: usize,
    Identifier: []const u8,
    Type: Type,
    Deref: *Ast,
    Assign: struct {
        dst: *Ast,
        src: *Ast,
    },
    BinOp: struct {
        kind: Kind,
        lhs: *Ast,
        rhs: *Ast,
    },
    If: struct {
        cond: *Ast,
        succ: *Ast,
        fail: *Ast,
    },
    While: struct {
        cond: *Ast,
        body: *Ast,
    },
    Call: struct {
        name: *Ast,
        args: []Ast,
    },
    Block: struct {
        ctx: *Context,
        body: []Stmt,
    },
    Return: *Ast,

    const Input = Cursor;
    pub const Kind = enum {
        Add,
        Sub,
        Mul,
        Eq,
        Ne,
    };

    const Eval = struct {
        value: Var,

        is_return: bool,
    };

    pub fn isComptime(self: Ast, ctx: *Context) bool {
        return switch (self) {
            .Integer, .Type => true,
            // Only limited ident
            .Identifier => |v| {
                const value = ctx.get(v).?;
                const basic = value.scope == .compile_time;

                unreachable; // DONT forget this part
                return switch (value.ty) {
                    .Function => {
                        const func = ctx.funcs.get(v).?;
                        return func.attr.is_comptime;
                    },
                    else => basic,
                };

            },
            .BinOp => |t| t.lhs.isComptime(ctx) and t.rhs.isComptime(ctx),
            .If => |t| t.cond.isComptime(ctx) and t.succ.isComptime(ctx) and t.fail.isComptime(ctx),
            .Call => |t| {
                if (!t.name.isComptime(ctx))
                    return false;

                for (t.args) |arg|
                    if (!arg.isComptime(ctx))
                        return false;

                return true;
            },
            // TODO: Somehow edit the function when return is comptime
            //.Return => |v| v.isComptime(ctx),
            .Return => false,
            else => false,
        };
    }

    pub fn eval(self: Ast, ctx: *Context) ParseError!Var {
        if (!self.isComptime(ctx))
            return ParseError.NonComptimeEval;

        const ext = try self.evalExt(ctx);
        return ext.value;
    }


    fn evalExt(self: Ast, ctx: *Context) Allocator.Error!Eval {
        //std.debug.print("Trying to eval ", .{});
        //@import("pretty.zig").print(ctx.map.allocator, self, .{}) catch unreachable;

        const empty = .{
            .value = .{
                .scope = .global,
                .access = .direct,
                .tag = .none,
                .ty = .Void,
            },
            .is_return = false,
        };

        return switch (self) {
            .Integer => |v| .{
                .value = .{
                    .scope = .compile_time,
                    .access = .direct,
                    .tag = .{ .liter = v },
                    .ty = .CompInt,
                },
                .is_return = false,
            },
            .Identifier => |v| .{
                .value = ctx.get(v).?,
                .is_return = false,
            },
            .Type => |v| .{
                .value = .{
                    .scope = .compile_time,
                    .access = .direct,
                    .tag = .{ .ty = v },
                    .ty = .Type,
                },
                .is_return = false,
            },
            .BinOp => |t| {
                const lhs_ext = try t.lhs.evalExt(ctx);
                const rhs_ext = try t.rhs.evalExt(ctx);
                if (lhs_ext.is_return) return lhs_ext;
                if (rhs_ext.is_return) return rhs_ext;
                const lhs = lhs_ext.value;
                const rhs = rhs_ext.value;

                const max = Type.maxNumeric(lhs.ty, rhs.ty) orelse lhs.ty;

                const lit = switch (t.kind) {
                    .Add => lhs.tag.liter + rhs.tag.liter,
                    .Sub => lhs.tag.liter - rhs.tag.liter,
                    .Mul => lhs.tag.liter * rhs.tag.liter,
                    .Eq => b: {
                        const cond = switch (max) {
                            .Type => Type.eql(lhs.tag.ty, rhs.tag.ty),
                            else => lhs.tag.liter == rhs.tag.liter,
                        };

                        break :b if (cond) @as(usize, 1) else 0;
                    },
                    .Ne => b: {
                        const cond = switch (max) {
                            .Type => Type.eql(lhs.tag.ty, rhs.tag.ty),
                            else => lhs.tag.liter == rhs.tag.liter,
                        };

                        break :b if (!cond) @as(usize, 1) else 0;
                    },
                };

                const ty = switch (t.kind) {
                    .Add, .Sub, .Mul => max,
                    .Eq, .Ne => .Bool,
                };

                return .{
                    .value = .{
                        .scope = .local,
                        .access = .direct,
                        .tag = .{ .liter = lit },
                        .ty = ty,
                    },
                    .is_return = false,
                };
            },
            .If => |t| {
                const cond_ext = try t.cond.evalExt(ctx);
                if (cond_ext.is_return) return cond_ext;
                const cond = cond_ext.value;

                std.debug.assert(cond.ty == .Bool);

                return if (cond.tag.liter != 0)
                    t.succ.evalExt(ctx)
                else
                    t.fail.evalExt(ctx);
            },
            .Call => |t| {
                const name_ext = try t.name.evalExt(ctx);
                if (name_ext.is_return) return name_ext;
                const name = name_ext.value;
                const func = ctx.funcs.get(name.tag.named).?;

                var context = try func.ctx.child(null);
                for (t.args, func.params.names) |arg, n| {
                    const ext = try arg.evalExt(ctx);
                    if (ext.is_return) return ext;
                    try context.put(n, ext.value);
                }

                var result = try func.tree.evalExt(context);
                std.debug.assert(result.is_return);
                result.is_return = false;
                return result;
            },
            .Block => |t| {
                var context = try ctx.child(null);

                for (t.body) |e| {
                    switch (e) {
                        .Ast => |v| {
                            const ext = try v.evalExt(context);
                            if (ext.is_return) return ext;
                        },
                        .Declare => |t2| {
                            const src_ext = try t2.src.evalExt(context);
                            if (src_ext.is_return) return src_ext;
                            const src = src_ext.value;
                            const val = .{
                                .scope = if (t2.is_comptime) Var.Scope.compile_time else .local,
                                .access = .deferred,
                                .tag = src.tag,
                                .ty = b: {
                                    const ext = try t2.ty.evalExt(context);
                                    break :b ext.value.tag.ty;
                                },
                            };

                            try context.put(t2.dst, val);
                        },
                    }
                }

                return empty;
            },
            .Return => |v| {
                const ext = try v.evalExt(ctx);
                return .{
                    .value = ext.value,
                    .is_return = true,
                };
            },
            else => unreachable,
        };
    }

    pub fn flatten(self: Ast, ctx: *Context, func: *Function, blk: **Block) ParseError!Var {
        if (self.eval(ctx)) |value| {
            return value; 
        } else |_| {}

        const empty = .{
            .scope = .global,
            .access = .direct,
            .tag = .none,
            .ty = .Void,
        };

        switch (self) {
            .Integer => |v| return .{
                .scope = .compile_time,
                .access = .direct,
                .tag = .{ .liter = v },
                .ty = .CompInt,
            },
            .Identifier => |v| {
                const src = ctx.lvalue(v).?;
                if (src.access == .direct)
                    return src;

                const dst = ctx.newVar(.direct, src.ty);
                
                try blk.*.ops.append(.{ .Load = .{
                    .dst = dst,
                    .src = src,
                }});
                
                return dst;
            },
            .Type => |v| return .{
                .scope = .compile_time,
                .access = .direct,
                .tag = .{ .ty = v },
                .ty = .Type,
            },
            .Deref => |v| {
                const src = try v.flatten(ctx, func, blk);
                const dst = ctx.newVar(.direct, src.ty);

                try blk.*.ops.append(.{ .Load = .{
                    .dst = dst,
                    .src = src,
                }});

                return dst;
            },
            .Assign => |t| {
                const src = try t.src.flatten(ctx, func, blk);
                const dst = try t.dst.lvalue(ctx, func, blk);

                if (dst.ty.hasData())
                    try blk.*.ops.append(.{ .Store = .{
                        .dst = dst,
                        .src = src,
                    }});

                return src;
            },
            .BinOp => |t| {
                const lhs = try t.lhs.flatten(ctx, func, blk);
                const rhs = try t.rhs.flatten(ctx, func, blk);
                const max = Type.maxNumeric(lhs.ty, rhs.ty).?;
                const dst = ctx.newVar(.direct, max);

                try blk.*.ops.append(.{ .BinOp = .{
                    .kind = t.kind,
                    .dst = dst,
                    .lhs = lhs,
                    .rhs = rhs,
                }});

                return dst;
            },
            .If => |t| {
                const cond = try t.cond.flatten(ctx, func, blk);
                var succ = try func.newBlock();
                var fail = try func.newBlock();
                const merge = try func.newBlock();

                const s = try t.succ.flatten(ctx, func, &succ);
                const f = try t.fail.flatten(ctx, func, &fail);

                const ty = Type.maxNumeric(s.ty, f.ty) orelse s.ty;
                const dest = ctx.newVar(.direct, ty);
                const loaded = ctx.newVar(.direct, ty);

                if (ty.hasData()) {
                    try blk.*.ops.append(.{ .Alloc = .{
                        .dst = dest,
                        .amount = ty.size().?,
                    }});

                    try succ.*.ops.append(.{ .Store = .{
                        .dst = dest,
                        .src = s,
                    }});

                    try fail.*.ops.append(.{ .Store = .{
                        .dst = dest,
                        .src = f,
                    }});

                    try merge.*.ops.append(.{ .Load = .{
                        .dst = loaded,
                        .src = dest,
                    }});
                }

                try func.addJmp(succ, merge);
                try func.addJmp(fail, merge);
                try func.addJnz(blk.*, cond, succ, fail);

                blk.* = merge;
                return loaded;
            },
            .While => |t| {
                var cond = try func.newBlock();
                var body = try func.newBlock();
                const merge = try func.newBlock();

                const c = try t.cond.flatten(ctx, func, &cond);
                const b = try t.body.flatten(ctx, func, &body);
                _ = b;

                try func.addJmp(blk.*, cond);
                try func.addJnz(cond, c, body, merge);
                try func.addJmp(body, cond);

                blk.* = merge;
                return empty;
            },
            .Call => |t| {
                const name = try t.name.flatten(ctx, func, blk);
                // TODO: Globalize this
                const ret = name.ty.Function.ret.*;
                const dest = switch (ret) {
                    .NoReturn, .Void => null,
                    else => ctx.newVar(.direct, ret),
                };

                var args = try ctx.map.allocator.alloc(Var, t.args.len);
                for (t.args, 0..) |arg, i|
                    args[i] = try arg.flatten(ctx, func, blk);

                try blk.*.ops.append(.{ .Call = .{
                    .dest = dest,
                    .name = name,
                    .args = args,
                }});

                return dest orelse empty;
            },
            .Block => |t| {
                for (t.body, 0..) |e, i| {
                    switch (e) {
                        .Ast => |v| _ = try v.flatten(t.ctx, func, blk),
                        .Declare => |t2| {
                            const src = try t2.src.flatten(t.ctx, func, blk);
                            const val = .{
                                .scope = if (t2.is_comptime)
                                    Var.Scope.compile_time
                                else
                                    .local,
                                .access = .deferred,
                                .tag = src.tag,
                                .ty = b: {
                                    const value = try t2.ty.eval(t.ctx);
                                    break :b value.tag.ty;
                                },
                            };

                            try t.ctx.put(t2.dst, val);
                            const dst = t.ctx.lvalue(t2.dst).?;
                            if (dst.scope == .compile_time or !dst.ty.hasData())
                                continue;

                            try blk.*.ops.append(.{ .Alloc = .{
                                .dst = dst,
                                .amount = dst.ty.size().?,
                            }});

                            try blk.*.ops.append(.{ .Store = .{
                                .dst = dst,
                                .src = src,
                            }});
                        },
                    }

                    if (blk.*.ops.getLastOrNull()) |last|
                        switch (last) {
                            .Jnz, .Jmp, .Ret => if (i != t.body.len - 1) {
                                blk.* = try func.newBlock();
                            },
                            else => {},
                        };
                }

                return empty;
            },
            .Return => |v| {
                const ret = try v.flatten(ctx, func, blk);
                try blk.*.ops.append(.{ .Ret = ret });

                return empty;
            },
        }
    }

    pub fn lvalue(self: Ast, ctx: *Context, func: *Function, blk: **Block) !Var {
        switch (self) {
            .Identifier => |v| return ctx.lvalue(v).?,
            .Deref => |v| return v.flatten(ctx, func, blk),
            else => unreachable,
        }
    }

    pub fn parse(allocator: Allocator, source: []const Token) ParseError!File {
        var externs = ArrayList(File.Extern).init(allocator);
        var consts = ArrayList(File.Const).init(allocator);
        var functions = ArrayList(Function).init(allocator);
        var order = ArrayList(File.Order).init(allocator);
        var input = Cursor.new(source);

        while (input.items.len != 0) {
            while (input.isNext(.Extern)) {
                input, const ext = try functionExtern(allocator, input);
                try order.append(.{ .ext = externs.items.len });
                try externs.append(ext);
            }

            if (input.items.len == 0) break;

            while (input.isNext(.Comptime)) {
                input, const con = try globalConst(allocator, input) orelse break;
                try order.append(.{ .con = consts.items.len });
                try consts.append(con);
            }

            if (input.items.len == 0) break;

            input, const func = try function(allocator, input);
            try order.append(.{ .fun = functions.items.len });
            try functions.append(func);
        }

        return .{
            .externs = try externs.toOwnedSlice(),
            .consts = try consts.toOwnedSlice(),
            .functions = try functions.toOwnedSlice(),
            .order = try order.toOwnedSlice(),
        };
    }

    pub fn function(allocator: Allocator, _input: Input) ParseError!Pair(Input, Function) {
        var input = _input;
        var attr: Function.Attr = .{};

        if (input.take(.Export)) |res| {
            input, _ = res;
            attr.exported = true;
        }

        if (input.take(.Comptime)) |res| {
            input, _ = res;
            attr.is_comptime = true;
        }

        const first, const ret = try expectType(allocator, input);
        input, const name = try first.expect(.Identifier);
        input, const prms = try params(allocator, input);
        input, const body = try expr(allocator, input);
        input, _ = try input.expect(.SemiColon);

        return .{
            input,
            .{
                .ctx = undefined,
                .name = name,
                .params = prms,
                .ret = ret,
                .attr = attr,
                .tree = try lib.box(allocator, body),
                .blocks = ArrayList(Block).init(allocator),
                .loc = first.loc,
            }
        };
    }

    pub fn functionExtern(allocator: Allocator, _input: Input) ParseError!Pair(Input, File.Extern) {
        var input = _input;
        input, _ = try input.expect(.Extern);
        input, const ret = try expectType(allocator, input);
        input, const name = try input.expect(.Identifier);
        input, const prms = try params(allocator, input);
        input, _ = try input.expect(.SemiColon);

        return .{
            input,
            .{
                .name = name,
                .ty = .{ .Function = .{
                    .params = prms.types,
                    .ret = try lib.box(allocator, ret),
                }},
            },
        };
    }

    pub fn globalConst(allocator: Allocator, _input: Input) ParseError!?Pair(Input, File.Const) {
        var input = _input;

        input, _ = try input.expect(.Comptime);
        input, const ty = try expr(allocator, input);
        input, const ident = try input.expect(.Identifier);
        input, _ = input.take(.Assign) orelse return null;
        input, const value = try expr(allocator, input);
        input, _ = try input.expect(.SemiColon);

        return .{
            input,
            .{
                .dst = ident,
                .src = try lib.box(allocator, value),
                .ty = try lib.box(allocator, ty),
            }
        };
    }

    fn params(allocator: Allocator, _input: Input) ParseError!Pair(Input, Function.Params) {
        var names = ArrayList([]const u8).init(allocator);
        var types = ArrayList(Type).init(allocator);
        var input, _ = try _input.expect(.LParen);

        while (true) {
            input, const ty = try takeType(allocator, input) orelse break;
            input, const name = try input.expect(.Identifier);

            try names.append(name);
            try types.append(ty);

            input, _ = input.take(.Comma) orelse break;
        }
        
        input, _ = try input.expect(.RParen);
        return .{
            input,
            .{
                .names = try names.toOwnedSlice(),
                .types = try types.toOwnedSlice(),
            },
        };
    }

    fn takeType(allocator: Allocator, _input: Input) ParseError!?Pair(Cursor, Type) {
        var input, const first = _input.next() orelse return null;

        switch (first.node) {
            .Type => |v| return .{ input, v },
            .Ref => {
                input, const inner = try expectType(allocator, input);
                return .{
                    input,
                    .{ .Ptr = try lib.box(allocator, inner) },
                };
            },
            .Fn => {
                var types = ArrayList(Type).init(allocator);
                input, _ = try input.expect(.LParen);

                while (true) {
                    if (input.take(.RParen)) |_|
                        break;

                    input, const ty = try expectType(allocator, input);
                    try types.append(ty);
                    input, _ = input.take(.Comma) orelse break;
                }

                input, _ = try input.expect(.RParen);
                input, const ret = try expectType(allocator, input);

                return .{
                    input,
                    .{ .Function = .{
                        .params = try types.toOwnedSlice(),
                        .ret = try lib.box(allocator, ret),
                    }},
                };
            },
            else => return null,
        }
    }

    fn expectType(allocator: Allocator, input: Input) ParseError!Pair(Cursor, Type) {
        return try takeType(allocator, input) orelse ParseError.Unexpected;
    }

    fn stmt(allocator: Allocator, _input: Input) ParseError!Pair(Input, Stmt) {
        var input = _input;

        // TODO: Clean this up, when const/mut are added
        // If i decide to do that
        if (input.take(.Comptime)) |t| {
            input, _ = t;

            input, const ty = try expr(allocator, input);
            input, const ident = try input.expect(.Identifier);
            input, _ = try input.expect(.Assign);
            input, const value = try expr(allocator, input);
            input, _ = try input.expect(.SemiColon);

            return .{
                input,
                .{ .Declare = .{
                    .dst = ident,
                    .src = try lib.box(allocator, value),
                    .ty = try lib.box(allocator, ty),
                    .is_comptime = true,
                }}
            };
        }

        input, const first = try expr(allocator, input);
        var s = Stmt{ .Ast = first };

        if (input.take(.Identifier)) |t| {
            input, const ident = t;
            input, _ = try input.expect(.Assign);
            input, const value = try expr(allocator, input);

            s = .{ .Declare = .{
                .dst = ident,
                .src = try lib.box(allocator, value),
                .ty = try lib.box(allocator, first),
                .is_comptime = false,
            }};
        }

        input, _ = try input.expect(.SemiColon);
        return .{ input, s };
    }

    fn block(allocator: Allocator, _input: Input) ParseError!Pair(Input, Ast) {
        var body = ArrayList(Stmt).init(allocator);
        var input = _input;

        while (true) {
            input, const s = stmt(allocator, input) catch break;
            try body.append(s);
        }

        input, _ = try input.expect(.RBracket);
        return .{
            input,
            .{ .Block = .{
                .ctx = undefined,
                .body = try body.toOwnedSlice(),
            }},
        };
    }

    fn tuple(allocator: Allocator, _input: Input) ParseError!Pair(Input, []Ast) {
        var body = ArrayList(Ast).init(allocator);
        var input = _input;

        while (true) {
            if (input.take(.RParen)) |_|
                break;

            input, const e = expr(allocator, input) catch break;
            try body.append(e);
            input, _ = input.take(.Comma) orelse break;
        }

        input, _ = try input.expect(.RParen);
        return .{
            input,
            try body.toOwnedSlice(),
        };
    }

    fn @"if"(allocator: Allocator, _input: Input) ParseError!Pair(Input, Ast) {
        var input = _input;
        input, _ = try input.expect(.LParen);
        input, const cond = try expr(allocator, input);
        input, _ = try input.expect(.RParen);
        input, const succ = try expr(allocator, input);

        var fail: Ast = .{ .Block = .{
            .ctx = undefined,
            .body = try allocator.alloc(Stmt, 0)
        }};

        if (input.take(.Else)) |t| {
            input, _ = t;
            input, const f = try expr(allocator, input);
            fail = f;
        }

        return .{
            input,
            .{ .If = .{
                .cond = try lib.box(allocator, cond),
                .succ = try lib.box(allocator, succ),
                .fail = try lib.box(allocator, fail),
            }}
        };
    }

    fn @"while"(allocator: Allocator, _input: Input) ParseError!Pair(Input, Ast) {
        var input = _input;
        input, _ = try input.expect(.LParen);
        input, const cond = try expr(allocator, input);
        input, _ = try input.expect(.RParen);
        input, const body = try expr(allocator, input);

        return .{
            input,
            .{ .While = .{
                .cond = try lib.box(allocator, cond),
                .body = try lib.box(allocator, body),
            }}
        };
    }

    pub fn expr(allocator: Allocator, input: Input) ParseError!Pair(Input, Ast) {
        return exprExt(allocator, input, 0);
    }

    fn exprExt(allocator: Allocator, _input: Input, min_power: u8) ParseError!Pair(Input, Ast) {
        var input, const first = _input.next() orelse return ParseError.Eof;
        lib.global_error = input.loc;

        var lhs: Ast = switch (first.node) {
            .Integer => |v| .{ .Integer = v },
            .Identifier => |v| .{ .Identifier = v },
            .Mul => b: {
                const power = first.powerPrefix().?;
                input, const lhs = try exprExt(allocator, input, power);
                break :b .{ .Deref = try lib.box(allocator, lhs) };
            },
            .LBracket => b: {
                input, const lhs = try block(allocator, input);
                break :b lhs;
            },
            //.Const, .Var => b: {
            //    input, const lhs = try 
            //    break :b lhs;
            //},
            .If => b: {
                input, const lhs = try @"if"(allocator, input);
                break :b lhs;
            },
            .While => b: {
                input, const lhs = try @"while"(allocator, input);
                break :b lhs;
            },
            .Return => b: {
                input, const lhs = try expr(allocator, input);
                break :b .{ .Return = try lib.box(allocator, lhs) };
            },
            else => b: {
                if (try takeType(allocator, _input)) |t| {
                    input, const ty = t;
                    break :b .{ .Type = ty };
                }

                return ParseError.Unexpected;
            },
        };

        while (true) {
            const token = input.peek() orelse break;
            const power = token.power() orelse break;
            lib.global_error = input.loc;

            // Break Check
            switch (power) {
                .post => |p| if (p < min_power) break,
                .infix => |p| if (p.l < min_power) break,
            }

            input, _ = input.next().?;

            lhs = switch (power) {
                .post => switch (token.node) {
                    .LParen => .{
                        .Call = b: {
                            input, const args = try tuple(allocator, input);

                            break :b .{
                                .name = try lib.box(allocator, lhs),
                                .args = args,
                            };
                        },
                    },
                    else => unreachable,
                },
                .infix => |p| b: {
                    input, const rhs = try exprExt(allocator, input, p.r);
                    break :b if (token.node == .Assign) .{ .Assign = .{
                        .dst = try lib.box(allocator, lhs),
                        .src = try lib.box(allocator, rhs),
                    } } else .{ .BinOp = .{
                        .kind = token.kind().?,
                        .lhs = try lib.box(allocator, lhs),
                        .rhs = try lib.box(allocator, rhs),
                    } };
                },
            };
        }

        return .{ input, lhs };
    }
};
