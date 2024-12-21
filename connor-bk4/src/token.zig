const std = @import("std");
const Alloc = std.mem.Allocator;
const librarian = @import("librarian");
const Error = librarian.Error;
const Result = librarian.Result;
const MapO = librarian.MapO;
const byte = librarian.byte;
const tag = librarian.tag;
const takeWhile1 = librarian.takeWhile1;
const many0 = librarian.many0;
const alt = librarian.alt;
const types = @import("types.zig");
const Token = types.Token;

fn whitespace() type {
    return MapO(
        takeWhile1(u8, std.ascii.isWhitespace),
        Token{ .whitespace = undefined },
    );
}

fn function() type {
    return MapO(
        tag(u8, "fn"),
        Token{ .function = undefined },
    );
}

fn ret() type {
    return MapO(
        tag(u8, "return"),
        Token{ .ret = undefined },
    );
}

fn ty() type {
    return alt(&[_]type{MapO(
        tag(u8, "i64"),
        Token{ .ty = .I64 },
    )});
}

fn colon() type {
    return MapO(
        byte(u8, ':'),
        Token{ .colon = undefined },
    );
}

fn semicolon() type {
    return MapO(
        byte(u8, ';'),
        Token{ .semicolon = undefined },
    );
}

fn comma() type {
    return MapO(
        byte(u8, ','),
        Token{ .comma = undefined },
    );
}

fn l_paren() type {
    return MapO(
        byte(u8, '('),
        Token{ .l_paren = undefined },
    );
}

fn r_paren() type {
    return MapO(
        byte(u8, ')'),
        Token{ .r_paren = undefined },
    );
}

fn plus() type {
    return MapO(
        byte(u8, '+'),
        Token{ .plus = undefined },
    );
}

fn minus() type {
    return MapO(
        byte(u8, '-'),
        Token{ .minus = undefined },
    );
}

fn identifier() type {
    return MapO(takeWhile1(u8, std.ascii.isAlphabetic), struct {
        fn f(alloc: Alloc, res: []const u8) Error!Token {
            _ = alloc;
            return Token{ .identifier = res };
        }
    }.f);
}

fn integer() type {
    return MapO(takeWhile1(u8, std.ascii.isDigit), struct {
        fn f(alloc: Alloc, res: []const u8) Error!Token {
            _ = alloc;
            return Token{ .integer = std.fmt.parseInt(isize, res, 10) catch unreachable };
        }
    }.f);
}

pub fn tokenizer() type {
    return struct {
        pub const I = []const u8;
        pub const O = []const Token;

        pub fn parse(input: I, alloc: Alloc) Result(I, O) {
            return many0(
                alt(
                    &[_]type{
                        whitespace(),
                        function(),
                        ret(),
                        ty(),
                        colon(),
                        semicolon(),
                        comma(),
                        l_paren(),
                        r_paren(),
                        plus(),
                        minus(),
                        identifier(),
                        integer(),
                    },
                ),
            ).parse(input, alloc);
        }
    };
}
