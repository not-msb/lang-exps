const std = @import("std");
const ModuleDependency = std.Build.ModuleDependency;

pub fn build(b: *std.Build) void {
    const target = b.standardTargetOptions(.{});
    const optimize = b.standardOptimizeOption(.{});

    const exe = b.addExecutable(.{
        .name = "connor",
        .root_source_file = .{ .path = "src/main.zig" },
        .target = target,
        .optimize = optimize,
    });

    // Deps
    const vec = b.addModule("vec", .{
        .source_file = .{ .path = "deps/vec.zig" }
    });

    const tools = b.addModule("tools", .{
        .source_file = .{ .path = "deps/tools.zig" }
    });
    exe.addModule("tools", tools);

    const librarian = b.addModule("librarian", .{
        .source_file = .{ .path = "deps/librarian.zig" },
        .dependencies = &[_]ModuleDependency{
            .{ .name = "vec", .module = vec },
        },
    });
    exe.addModule("librarian", librarian);
    // End Deps

    b.installArtifact(exe);

    const run_cmd = b.addRunArtifact(exe);
    run_cmd.step.dependOn(b.getInstallStep());

    if (b.args) |args| {
        run_cmd.addArgs(args);
    }

    const run_step = b.step("run", "Run the app");
    run_step.dependOn(&run_cmd.step);

    const unit_tests = b.addTest(.{
        .root_source_file = .{ .path = "src/main.zig" },
        .target = target,
        .optimize = optimize,
    });

    const run_unit_tests = b.addRunArtifact(unit_tests);

    const test_step = b.step("test", "Run unit tests");
    test_step.dependOn(&run_unit_tests.step);
}
