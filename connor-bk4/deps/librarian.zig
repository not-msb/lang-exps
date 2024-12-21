const std = @import("std");
const Alloc = std.mem.Allocator;
const FixedBufferAlloc = std.heap.FixedBufferAllocator;
const testing = std.testing;
const mEql = std.meta.eql;
const sEql = std.mem.eql;
const Vec = @import("vec.zig").Vec;

pub const Error = error {
    Eof,
    Backtrack,
} || Alloc.Error;

fn MapSlice(comptime T: type, comptime G: type, comptime slice: []const T, comptime f: fn(T) G) [slice.len]G {
    var out: [slice.len]G = undefined;
    for (slice, 0..) |item, i| {
        out[i] = f(item);
    }
    return out;
}

pub fn MapO(comptime P: type, comptime F: anytype) type {
    return struct {
        pub const I = P.I;
        pub const O = switch (@typeInfo(@TypeOf(F))) {
            .Fn => |info| @typeInfo(info.return_type.?).ErrorUnion.payload,
            else => @TypeOf(F),
        };

        pub fn parse(input: I, alloc: Alloc) Result(I, O) {
            const res = try P.parse(input, alloc);
            return .{ .i = res.i, .o = switch (@typeInfo(@TypeOf(F))) {
                .Fn => try F(alloc, res.o),
                else => F,
            }};
        }
    };
}

pub fn Result(comptime I: type, comptime O: type) type {
    return Error!struct { i: I, o: O };
}

pub fn byte(comptime T: type, comptime P: T) type {
    return struct {
        pub const I = []const T;
        pub const O = T;

        pub fn parse(input: I, alloc: Alloc) Result(I, O) {
            _ = alloc;
            if (input.len == 0) return Error.Eof;
            if (mEql(input[0], P)) return .{ .i = input[1..], .o = input[0] };
            return Error.Backtrack;
        }
    };
}

pub fn tag(comptime T: type, comptime P: []const T) type {
    return struct {
        pub const I = []const T;
        pub const O = []const T;

        pub fn parse(input: I, alloc: Alloc) Result(I, O) {
            _ = alloc;
            if (input.len < P.len) return Error.Backtrack;
            if (sEql(u8, input[0..P.len], P)) return .{ .i = input[P.len..], .o = P };
            return Error.Backtrack;
        }
    };
}


pub fn many0(comptime P: type) type {
    return struct {
        pub const I = P.I;
        pub const O = []const P.O;

        pub fn parse(input: I, alloc: Alloc) Result(I, O) {
            var in = input;
            var out = try Vec(P.O).init(alloc);

            while (P.parse(in, alloc)) |res| {
                in = res.i;
                _ = try out.push(res.o);
            } else |_| {}

            return .{ .i = in, .o = try out.shrinkWrap() };
        }
    };
}

pub fn many1(comptime P: type) type {
    return struct {
        pub const I = P.I;
        pub const O = []const P.O;

        pub fn parse(input: I, alloc: Alloc) Result(I, O) {
            const res = try many0(P).parse(input, alloc);
            return if (res.o.len != 0) res else Error.Backtrack;
        }
    };
}

pub fn delimitedMany0(comptime P: type, comptime D: type) type {
    return struct {
        pub const I = P.I;
        pub const O = []const P.O;

        pub fn parse(input: I, alloc: Alloc) Result(I, O) {
            var in = input;
            var out = try Vec(P.O).init(alloc);

            while (P.parse(in, alloc)) |res| {
                in = res.i;
                _ = try out.push(res.o);

                if (D.parse(in, alloc)) |r|
                    in = r.i
                else |_|
                    break;
            } else |_| {}

            return .{ .i = in, .o = try out.shrinkWrap() };
        }
    };
}

pub fn delimitedMany1(comptime P: type) type {
    return struct {
        pub const I = P.I;
        pub const O = []const P.O;

        pub fn parse(input: I, alloc: Alloc) Result(I, O) {
            const res = try delimitedMany0(P).parse(input, alloc);
            return if (res.o.len != 0) res else Error.Backtrack;
        }
    };
}

pub fn takeWhile0(comptime T: type, comptime P: fn(T) bool) type {
    return struct {
        pub const I = []const T;
        pub const O = []const T;

        pub fn parse(input: I, alloc: Alloc) Result(I, O) {
            _ = alloc;
            var i: usize = 0;
            while (i < input.len and P(input[i])) : (i += 1) {}
            return .{ .i = input[i..], .o = input[0..i] };
        }
    };
}

pub fn takeWhile1(comptime T: type, comptime P: fn(T) bool) type {
    return struct {
        pub const I = []const T;
        pub const O = []const T;

        pub fn parse(input: I, alloc: Alloc) Result(I, O) {
            const res = try takeWhile0(T, P).parse(input, alloc);
            return if (res.o.len != 0) res else Error.Backtrack;
        }
    };
}

pub fn alt(comptime P: []const type) type {
    const T = if (P.len != 0) P[0] else @compileError("alt: Not enough parsers");
    inline for (P) |p| {
        if (p.I != T.I) @compileError("alt: Inconsistent I-types");
        if (p.O != T.O) @compileError("alt: Inconsistent O-types");
    }

    return struct {
        pub const I = T.I;
        pub const O = T.O;

        pub fn parse(input: I, alloc: Alloc) Result(I, O) {
            inline for (P) |p| {
                if (p.parse(input, alloc)) |res| {
                    return res;
                } else |_| {}
            }

            return Error.Backtrack;
        }
    };
}

pub fn sequence(comptime T: type, comptime P: []const type) type {
    return struct {
        pub const I = []const T;
        pub const O = std.meta.Tuple(&MapSlice(type, type, P, struct{
            fn f(comptime p: type) type {
                return p.O;
            }
        }.f));

        pub fn parse(input: I, alloc: Alloc) Result(I, O) {
            var in = input;
            var out: O = undefined;

            inline for (P, 0..) |p, i| {
                if (p.parse(in, alloc)) |res| {
                    in = res.i;
                    out[i] = res.o;
                } else |err| {
                    return err;
                }
            }

            return .{ .i = in, .o = out };
        }
    };
}

test "byte: 0" {
    const input = "AAA!";
    const parser = byte(u8, 'A');
    const output = try parser.parse(input, testing.allocator);

    try testing.expect(sEql(
        u8,
        output.i,
        "AA!",
    ));

    try testing.expectEqual(
        output.o,
        'A',
    );
}

test "tag: 0" {
    const input = "AAA!";
    const parser = tag(u8, "AAA");
    const output = try parser.parse(input, testing.allocator);

    try testing.expect(sEql(
        u8,
        output.i,
        "!",
    ));

    try testing.expect(sEql(
        u8,
        output.o,
        "AAA",
    ));
}

test "many0: 0" {
    const input = "AAA!";
    const parser = many0(byte(u8, 'A'));
    const output = try parser.parse(input, testing.allocator);
    defer testing.allocator.free(output.o);

    try testing.expect(sEql(
        u8,
        output.i,
        "!",
    ));

    try testing.expect(sEql(
        u8,
        output.o,
        "AAA",
    ));
}

test "many1: 0" {
    const input = "AAA!";
    const parser = many1(byte(u8, 'A'));
    const output = try parser.parse(input, testing.allocator);
    defer testing.allocator.free(output.o);

    try testing.expect(sEql(
        u8,
        output.i,
        "!",
    ));

    try testing.expect(sEql(
        u8,
        output.o,
        "AAA",
    ));
}

test "takeWhile0: 0" {
    const input = "Hello World!";
    const parser = takeWhile0(u8, std.ascii.isAlphabetic);
    const output = try parser.parse(input, testing.allocator);

    try testing.expect(sEql(
        u8,
        output.i,
        " World!",
    ));

    try testing.expect(sEql(
        u8,
        output.o,
        "Hello",
    ));
}

test "takeWhile1: 0" {
    const input = "Hello World!";
    const parser = takeWhile1(u8, std.ascii.isAlphabetic);
    const output = try parser.parse(input, testing.allocator);

    try testing.expect(sEql(
        u8,
        output.i,
        " World!",
    ));

    try testing.expect(sEql(
        u8,
        output.o,
        "Hello",
    ));
}

test "alt: 0" {
    const input = "++-++*/*";
    const parser = alt(&[_]type{ byte(u8, '+'), byte(u8, '-'), byte(u8, '*'), byte(u8, '/') });
    const output = try parser.parse(input, testing.allocator);

    try testing.expect(std.mem.eql(
        u8,
        output.i,
        "+-++*/*",
    ));

    try testing.expect(output.o == '+');
}

test "sequence: 0" {
    const input = "Hello+-+/*/World!";
    const parser = sequence(u8, &[_]type{
        takeWhile0(u8, std.ascii.isAlphabetic),
        many0(
            alt(
                &[_]type{ byte(u8, '+'), byte(u8, '-'), byte(u8, '*'), byte(u8, '/') }
            ),
        ),
        takeWhile0(u8, std.ascii.isAlphabetic),
        byte(u8, '!'),
    });
    const output = try parser.parse(input, testing.allocator);
    defer testing.allocator.free(output.o[1]);

    try testing.expectEqual(output.i.len, 0);

    try testing.expect(std.mem.eql(
        u8,
        output.o[0],
        "Hello",
    ) and
        sEql(
        u8,
        output.o[1],
        "+-+/*/",
    ) and
        sEql(
        u8,
        output.o[2],
        "World",
    ) and
        output.o[3] == '!');
}
