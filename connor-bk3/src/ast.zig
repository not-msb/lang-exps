const std = @import("std");
const Allocator = std.mem.Allocator;
const tools = @import("tools.zig");
const Cursor = tools.Cursor;
const box = tools.box;
const types = @import("types.zig");
const Type = types.Type;
const Token = types.Token;
const Ast = types.Ast;

fn token(comptime T: @typeInfo(Token).Union.tag_type.?, input: *Cursor(Token)) ?Token {
    const tok = if (input.done()) return null else input.slice()[0];
    if (tok == T) {
        input.next();
        return tok;
    } else return null;
}

fn integer(input: *Cursor(Token)) ?usize {
    return if (token(.integer, input)) |tok|
        tok.integer
    else
        null;
}

fn identifier(input: *Cursor(Token)) ?[]const u8 {
    return if (token(.identifier, input)) |tok|
        tok.identifier
    else
        null;
}

fn block(input: *Cursor(Token), allocator: Allocator) Allocator.Error!?Ast {
    const pos = input.save();
    blk: {
        _ = token(.l_bracket, input) orelse break :blk;

        var asts = try allocator.alloc(Ast, 0);
        while (try statement(input, allocator)) |stmt| {
            asts = try allocator.realloc(asts, asts.len + 1);
            asts[asts.len - 1] = stmt;
        }

        _ = token(.r_bracket, input) orelse break :blk;

        return .{ .block = asts };
    }
    input.restore(pos);
    return null;
}

fn expr(input: *Cursor(Token), allocator: Allocator) Allocator.Error!?Ast {
    if (try fCall(input, allocator)) |v|
        return v;
    if (integer(input)) |v|
        return .{ .integer = v };
    if (identifier(input)) |v|
        return .{ .identifier = v };
    if (try add(input, allocator)) |v|
        return v;
    if (try sub(input, allocator)) |v|
        return v;
    if (try mul(input, allocator)) |v|
        return v;
    if (try div(input, allocator)) |v|
        return v;
    if (try block(input, allocator)) |v|
        return v;

    return null;
}

fn statement(input: *Cursor(Token), allocator: Allocator) Allocator.Error!?Ast {
    if (try ret(input, allocator)) |v|
        return v;
    if (try assignment(input, allocator)) |v|
        return v;

    return null;
}

fn ret(input: *Cursor(Token), allocator: Allocator) Allocator.Error!?Ast {
    const pos = input.save();
    blk: {
        _ = token(.ret, input) orelse break :blk;
        const e = try expr(input, allocator) orelse break :blk;
        _ = token(.semicolon, input) orelse break :blk;

        return .{ .ret = try box(allocator, e) };
    }
    input.restore(pos);
    return null;
}

fn assignment(input: *Cursor(Token), allocator: Allocator) Allocator.Error!?Ast {
    const pos = input.save();
    blk: {
        _ = token(.constant, input) orelse break :blk;
        const name = identifier(input) orelse break :blk;
        _ = token(.colon, input) orelse break :blk;
        const t = ty(input) orelse break :blk;
        _ = token(.equal, input) orelse break :blk;
        const e = try expr(input, allocator) orelse break :blk;
        _ = token(.semicolon, input) orelse break :blk;

        return .{ .assign = .{ .assignee = name, .assigner = try box(allocator, e), .ty = t } };
    }
    input.restore(pos);
    return null;
}

fn ty(input: *Cursor(Token)) ?Type {
    return if (token(.ty, input)) |tok|
        tok.ty
    else
        null;
}

fn args(input: *Cursor(Token), allocator: Allocator) Allocator.Error![]const Ast {
    var arguments = try allocator.alloc(Ast, 0);

    while (try expr(input, allocator)) |arg| {
        arguments = try allocator.realloc(arguments, arguments.len + 1);
        arguments[arguments.len - 1] = arg;

        if (token(.comma, input) == null) break;
    }

    return arguments;
}

fn prm(input: *Cursor(Token)) ?Ast.Param {
    const pos = input.save();
    blk: {
        const name = identifier(input) orelse break :blk;
        _ = token(.colon, input) orelse break :blk;
        const t = ty(input) orelse break :blk;

        return .{ .name = name, .ty = t };
    }
    input.restore(pos);
    return null;
}

fn prms(input: *Cursor(Token), allocator: Allocator) Allocator.Error![]const Ast.Param {
    var params = try allocator.alloc(Ast.Param, 0);

    while (prm(input)) |param| {
        params = try allocator.realloc(params, params.len + 1);
        params[params.len - 1] = param;

        if (token(.comma, input) == null) break;
    }

    return params;
}

fn add(input: *Cursor(Token), allocator: Allocator) Allocator.Error!?Ast {
    const pos = input.save();
    blk: {
        _ = token(.plus, input) orelse break :blk;
        const l = try expr(input, allocator) orelse break :blk;
        const r = try expr(input, allocator) orelse break :blk;

        return .{ .add = .{ .l = try box(allocator, l), .r = try box(allocator, r) } };
    }
    input.restore(pos);
    return null;
}

fn sub(input: *Cursor(Token), allocator: Allocator) Allocator.Error!?Ast {
    const pos = input.save();
    blk: {
        _ = token(.minus, input) orelse break :blk;
        const l = try expr(input, allocator) orelse break :blk;
        const r = try expr(input, allocator) orelse break :blk;

        return .{ .sub = .{ .l = try box(allocator, l), .r = try box(allocator, r) } };
    }
    input.restore(pos);
    return null;
}

fn mul(input: *Cursor(Token), allocator: Allocator) Allocator.Error!?Ast {
    const pos = input.save();
    blk: {
        _ = token(.star, input) orelse break :blk;
        const l = try expr(input, allocator) orelse break :blk;
        const r = try expr(input, allocator) orelse break :blk;

        return .{ .mul = .{ .l = try box(allocator, l), .r = try box(allocator, r) } };
    }
    input.restore(pos);
    return null;
}

fn div(input: *Cursor(Token), allocator: Allocator) Allocator.Error!?Ast {
    const pos = input.save();
    blk: {
        _ = token(.slash, input) orelse break :blk;
        const l = try expr(input, allocator) orelse break :blk;
        const r = try expr(input, allocator) orelse break :blk;

        return .{ .div = .{ .l = try box(allocator, l), .r = try box(allocator, r) } };
    }
    input.restore(pos);
    return null;
}

fn fDecl(input: *Cursor(Token), allocator: Allocator) Allocator.Error!?Ast {
    const pos = input.save();
    blk: {
        _ = token(.function, input) orelse break :blk;
        const name = identifier(input) orelse break :blk;
        _ = token(.l_paren, input) orelse break :blk;
        const params = try prms(input, allocator);
        _ = token(.r_paren, input) orelse break :blk;
        const t = ty(input) orelse break :blk;
        const ex = try expr(input, allocator) orelse break :blk;

        return .{ .f_decl = .{ .name = name, .prms = params, .ty = t, .expr = try box(allocator, ex) } };
    }
    input.restore(pos);
    return null;
}

fn fCall(input: *Cursor(Token), allocator: Allocator) Allocator.Error!?Ast {
    const pos = input.save();
    blk: {
        const name = identifier(input) orelse break :blk;
        _ = token(.l_paren, input) orelse break :blk;
        const arguments = try args(input, allocator);
        _ = token(.r_paren, input) orelse break :blk;

        return .{ .f_call = .{ .name = name, .args = arguments } };
    }
    input.restore(pos);
    return null;
}

pub fn parser(input: []const Token, allocator: Allocator) Allocator.Error![]const Ast {
    var asts = try allocator.alloc(Ast, 0);
    var cursor = Cursor(Token){ .ptr = input };
    while (try fDecl(&cursor, allocator)) |ast| {
        asts = try allocator.realloc(asts, asts.len + 1);
        asts[asts.len - 1] = ast;

        if (cursor.done()) break;
    }

    return asts;
}
