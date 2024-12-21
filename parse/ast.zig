const std = @import("std");
const lib = @import("lib.zig");
const Allocator = std.mem.Allocator;
const StringHashMap = std.StringHashMap;
const ArrayList = std.ArrayList;
const reverseIterator = std.mem.reverseIterator;
const ParseError = lib.ParseError;
const Token = lib.Token;
const Cursor = lib.Cursor;
const Context = lib.Context;
const Pair = lib.Pair;
const Type = lib.Type;

// TODO: Temp
const Function = struct {
    ctx: *Context,
    name: []const u8,
    params: Params,
    ret: Type,
    attr: Attr,
    tree: *Ast,

    const Params = struct {
        names: []const []const u8,
        types: []const Type,
    };

    const Attr = struct {
        exported: bool = false,
    };
};

const File = struct {
    externs: []const Extern,
    functions: []Function,

    const Extern = struct {
        name: []const u8,
        ty: Type,
    };

    pub fn scan(file: File, allocator: Allocator) ParseError!void {
        var context = try Context.init(allocator, null);

        for (file.externs) |ext| {
            try context.put(ext.name, .{
                .scope = .global,
                .tag = .{ .named = ext.name },
                .ty = ext.ty,
            });
        }

        for (file.functions) |*function| {
            function.ctx = try context.child(function.ret);

            for (function.params.names, function.params.types) |name, ty| {
                try function.ctx.put(name, .{
                    .scope = .local,
                    .tag = .{ .named = name },
                    .ty = ty,
                });
            }

            const ret = try function.ctx.check(function.tree);
            if (!ret.coercible(function.ret)) return ParseError.TypeIncompatible;
        }
    }
};

pub const Ast = union(enum) {
    Integer: usize,
    Identifier: []const u8,
    Deref: *Ast,
    Declare: struct {
        dst: []const u8,
        src: *Ast,
        ty: Type,
    },
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
        body: []Ast,
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

    const Op = enum {
        Write,
        Read,
    };

    pub fn parse(allocator: Allocator, source: []const Token) ParseError!File {
        var externs = ArrayList(File.Extern).init(allocator);
        var functions = ArrayList(Function).init(allocator);
        var input = Cursor.new(source);

        while (input.items.len != 0) {
            while (input.isNext(.Extern)) {
                input, const ext = try functionExtern(allocator, input);
                try externs.append(ext);
            }

            input, const func = try function(allocator, input);
            try functions.append(func);
        }

        return .{
            .externs = try externs.toOwnedSlice(),
            .functions = try functions.toOwnedSlice(),
        };
    }

    pub fn function(allocator: Allocator, _input: Input) ParseError!Pair(Input, Function) {
        var input = _input;
        var attr: Function.Attr = .{};

        if (input.take(.Export)) |res| {
            input, _ = res;
            attr.exported = true;
        }

        input, const ret = try input.expect(.Type);
        input, const name = try input.expect(.Identifier);
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
            }
        };
    }

    pub fn functionExtern(allocator: Allocator, _input: Input) ParseError!Pair(Input, File.Extern) {
        var input = _input;
        input, _ = try input.expect(.Extern);
        input, const ret = try input.expect(.Type);
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

    fn params(allocator: Allocator, _input: Input) ParseError!Pair(Input, Function.Params) {
        var names = ArrayList([]const u8).init(allocator);
        var types = ArrayList(Type).init(allocator);
        var input, _ = try _input.expect(.LParen);

        while (true) {
            input, const ty = input.take(.Type) orelse break;
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

    fn block(allocator: Allocator, _input: Input) ParseError!Pair(Input, Ast) {
        var body = ArrayList(Ast).init(allocator);
        var input = _input;

        while (true) {
            const in, const e = expr(allocator, input) catch break;
            input, _ = try in.expect(.SemiColon);
            try body.append(e);
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
            const in, const e = expr(allocator, input) catch break;
            input, _ = in.take(.Comma) orelse break;
            try body.append(e);
        }

        if (expr(allocator, input) catch null) |result| {
            input, const e = result;
            try body.append(e);
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
            .body = try allocator.alloc(Ast, 0)
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

    fn decl(allocator: Allocator, _input: Input, ty: Type) ParseError!Pair(Input, Ast) {
        var input = _input;
        input, const name = input.take(.Identifier) orelse return ParseError.Eof;
        input, _ = input.take(.Assign) orelse return ParseError.Eof;
        input, const e = try expr(allocator, input);

        return .{
            input,
            .{ .Declare = .{
                .dst = name,
                .src = try lib.box(allocator, e),
                .ty = ty,
            }},
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
            .Type => |v| b: {
                input, const lhs = try decl(allocator, input, v);
                break :b lhs;
            },
            else => return ParseError.Unexpected,
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
