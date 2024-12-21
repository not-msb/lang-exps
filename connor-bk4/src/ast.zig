const std = @import("std");
const Alloc = std.mem.Allocator;
const box = @import("tools").box;
const librarian = @import("librarian");
const Error = librarian.Error;
const Result = librarian.Result;
const MapO = librarian.MapO;
const many0 = librarian.many0;
const delimitedMany0 = librarian.delimitedMany0;
const sequence = librarian.sequence;
const types = @import("types.zig");
const Type = types.Type;
const Token = types.Token;
const Ast = types.Ast;

fn takeToken(comptime T: @typeInfo(Token).Union.tag_type.?) type {
    return struct {
        pub const I = []const Token;
        pub const O = Token;

        pub fn parse(input: I, alloc: Alloc) Result(I, O) {
            _ = alloc;
            if (input.len == 0) return Error.Eof;
            return switch (input[0]) {
                T => .{ .i = input[1..], .o = input[0] },
                else => Error.Backtrack,
            };
        }
    };
}

// Can't use alt combinator, because of the recursive nature of expr()
fn expr() type {
    return struct {
        pub const I = []const Token;
        pub const O = Ast;

        pub fn parse(input: I, alloc: Alloc) Result(I, O) {
            if (fCall().parse(input, alloc)) |res| return res else |_| {}
            if (identifier().parse(input, alloc)) |res| return res else |_| {}
            if (integer().parse(input, alloc)) |res| return res else |_| {}
            if (add().parse(input, alloc)) |res| return res else |_| {}
            if (sub().parse(input, alloc)) |res| return res else |_| {}

            return Error.Backtrack;
        }
    };
}

// Can't use alt combinator, because of the recursive nature of stmt()
fn stmt() type {
    return struct {
        pub const I = []const Token;
        pub const O = Ast;

        pub fn parse(input: I, alloc: Alloc) Result(I, O) {
            if (ret().parse(input, alloc)) |result| {
                return result;
            } else |_| {}

            return Error.Backtrack;
        }
    };
}

fn prms() type {
    return delimitedMany0(
        MapO(sequence(Token, &[_]type{
            takeToken(Token.identifier),
            takeToken(Token.colon),
            ty(),
        }), struct {
            fn f(alloc: Alloc, result: struct { Token, Token, Type }) Error!Ast.PARAM {
                _ = alloc;
                return .{ .name = result[0].identifier, .ty = result[2] };
            }
        }.f),
        takeToken(Token.comma),
    );
}

fn args() type {
    return delimitedMany0(
        expr(),
        takeToken(Token.comma),
    );
}

fn ty_args() type {
    return delimitedMany0(
        ty(),
        takeToken(Token.comma),
    );
}

fn ty_flat() type {
    return MapO(takeToken(Token.ty), struct {
        fn f(alloc: Alloc, result: Token) Error!Type {
            _ = alloc;
            return result.ty;
        }
    }.f);
}

fn ty_fn() type {
    return MapO(sequence(Token, &[_]type{
        takeToken(.function),
        takeToken(.l_paren),
        ty_args(),
        takeToken(.r_paren),
        ty(),
    }), struct {
        fn f(alloc: Alloc, result: struct { Token, Token, []const Type, Token, Type }) Error!Type {
            return Type{ .Function = .{ .prms = result[2], .ty = try box(alloc, result[4]) } };
        }
    }.f);
}

fn ty_tuple() type {
    return MapO(sequence(Token, &[_]type{
        takeToken(.l_paren),
        ty_args(),
        takeToken(.r_paren),
    }), struct {
        fn f(alloc: Alloc, result: struct { Token, []const Type, Token }) Error!Type {
            _ = alloc;
            return Type{ .Tuple = result[1] };
        }
    }.f);
}

// Can't use alt combinator, because of the recursive nature of ty()
fn ty() type {
    return struct {
        pub const I = []const Token;
        pub const O = Type;

        pub fn parse(input: I, alloc: Alloc) Result(I, O) {
            if (ty_fn().parse(input, alloc)) |res| return res else |_| {}
            if (ty_tuple().parse(input, alloc)) |res| return res else |_| {}
            if (ty_flat().parse(input, alloc)) |res| return res else |_| {}

            return Error.Backtrack;
        }
    };
}

fn identifier() type {
    return MapO(takeToken(Token.identifier), struct {
        fn f(alloc: Alloc, result: Token) Error!Ast {
            _ = alloc;
            return Ast{ .identifier = result.identifier };
        }
    }.f);
}

fn integer() type {
    return MapO(takeToken(Token.integer), struct {
        fn f(alloc: Alloc, result: Token) Error!Ast {
            _ = alloc;
            return Ast{ .integer = result.integer };
        }
    }.f);
}

fn add() type {
    return MapO(sequence(Token, &[_]type{ takeToken(.plus), expr(), expr() }), struct {
        fn f(alloc: Alloc, result: struct { Token, Ast, Ast }) Error!Ast {
            return Ast{ .add = .{ .l = try box(alloc, result[1]), .r = try box(alloc, result[2]) } };
        }
    }.f);
}

fn sub() type {
    return MapO(sequence(Token, &[_]type{ takeToken(.minus), expr(), expr() }), struct {
        fn f(alloc: Alloc, result: struct { Token, Ast, Ast }) Error!Ast {
            return Ast{ .add = .{ .l = try box(alloc, result[1]), .r = try box(alloc, Ast{ .neg = try box(alloc, result[2]) }) } };
        }
    }.f);
}

fn ret() type {
    return MapO(sequence(Token, &[_]type{ takeToken(Token.ret), expr(), takeToken(Token.semicolon) }), struct {
        fn f(alloc: Alloc, result: struct { Token, Ast, Token }) Error!Ast {
            return Ast{ .ret = try box(alloc, result[1]) };
        }
    }.f);
}

fn fDecl() type {
    return MapO(sequence(Token, &[_]type{ takeToken(Token.function), identifier(), takeToken(Token.l_paren), prms(), takeToken(Token.r_paren), ty(), expr() }), struct {
        fn f(alloc: Alloc, result: struct { Token, Ast, Token, []const Ast.PARAM, Token, Type, Ast }) Error!Ast {
            return Ast{ .f_decl = .{
                .name = result[1].identifier,
                .prms = result[3],
                .ty = result[5],
                .expr = try box(alloc, result[6]),
            } };
        }
    }.f);
}

fn fCall() type {
    return MapO(sequence(Token, &[_]type{ identifier(), takeToken(Token.l_paren), args(), takeToken(Token.r_paren) }), struct {
        fn f(alloc: Alloc, result: struct { Ast, Token, []const Ast, Token }) Error!Ast {
            _ = alloc;
            return Ast{ .f_call = .{
                .name = result[0].identifier,
                .args = result[2],
            } };
        }
    }.f);
}

pub fn parser() type {
    return struct {
        pub const I = []const Token;
        pub const O = []const Ast;

        pub fn parse(input: I, alloc: Alloc) Result(I, O) {
            return many0(
                fDecl(),
            ).parse(input, alloc);
        }
    };
}
