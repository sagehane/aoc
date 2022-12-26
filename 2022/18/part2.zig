const std = @import("std");

pub fn main() void {
    var sample = Model(Vec3{ 3 + 1, 3 + 1, 6 + 1 }).fromInput(sample_input);
    std.debug.print("Sample part 1: {d}\n", .{sample.surface_area});

    var real = Model(Vec3{ 20, 20, 20 }).fromInput(input);
    std.debug.print("Real part 1: {d}\n", .{real.surface_area});

    sample.fillEnclosed();
    std.debug.print("Sample part 2: {d}\n", .{sample.surface_area});

    real.fillEnclosed();
    std.debug.print("Real part 2: {d}\n", .{real.surface_area});
}

const sample_input =
    \\2,2,2
    \\1,2,2
    \\3,2,2
    \\2,1,2
    \\2,3,2
    \\2,2,1
    \\2,2,3
    \\2,2,4
    \\2,2,6
    \\1,2,5
    \\3,2,5
    \\2,1,5
    \\2,3,5
;

const State = enum(u2) {
    empty = 0,
    filled = 1,
    checked = 2,
    enclosed = 3,
};

const input = @embedFile("input");

const Vec3 = [3]u8;

fn lineToVec3(line: []const u8) Vec3 {
    var vec3: Vec3 = undefined;

    var i: u2 = 0;
    var iter = std.mem.tokenize(u8, line, ",");
    while (iter.next()) |n| : (i += 1) {
        vec3[i] = (std.fmt.parseInt(u8, n, 10) catch unreachable);
    }

    std.debug.assert(i == 3);

    return vec3;
}

test {
    try std.testing.expectEqualSlices(u8, &Vec3{ 2, 2, 2 }, &lineToVec3("2,2,2"));
}

fn Model(comptime dimensions: Vec3) type {
    const x = dimensions[0];
    const y = dimensions[1];
    const z = dimensions[2];
    const size = @as(u24, x) * y * z;

    return struct {
        const Self = @This();

        surface_area: u32 = 0,
        cubes: [size]State = [1]State{.empty} ** size,

        fn fromInput(input_str: []const u8) Self {
            var model = Self{};

            var iter = std.mem.tokenize(u8, input_str, "\n");
            while (iter.next()) |line| {
                const vec3 = lineToVec3(line);
                model.addCube(vec3);
            }

            return model;
        }

        fn fillEnclosed(self: *Self) void {
            for (self.cubes) |*cube, i| {
                const vec3 = idxToVec(@intCast(u24, i));

                if (cube.* == .empty and self.floodFillCheck(vec3))
                    self.floodFillEnclose(vec3);

                if (cube.* == .empty) {
                    const b = self.floodFillCheck(vec3);
                    std.debug.print("{}@{}@{any}\n", .{ cube.*, b, vec3 });

                    if (b) {
                        self.floodFillEnclose(vec3);
                    }
                }
            }
        }

        fn floodFillCheck(self: *Self, vec3: Vec3) bool {
            var frontier = std.BoundedArray(Vec3, size).init(0) catch unreachable;
            frontier.appendAssumeCapacity(vec3);

            var is_enclosed = true;

            var buffer: [6]Vec3 = undefined;
            while (frontier.popOrNull()) |f_vec3| {
                const adjacents = getAdjacents(f_vec3, &buffer);
                is_enclosed = is_enclosed and adjacents.len == 6;

                for (adjacents) |adj| {
                    if (self.cubes[vecToIdx(adj)] == .empty) {
                        frontier.appendAssumeCapacity(adj);
                        self.cubes[vecToIdx(adj)] = .checked;
                    }
                }
            }

            return is_enclosed;
        }

        fn floodFillEnclose(self: *Self, vec3: Vec3) void {
            self.cubes[vecToIdx(vec3)] = .enclosed;

            var buffer: [6]Vec3 = undefined;
            for (getAdjacents(vec3, &buffer)) |adj| {
                const adj_cube = self.cubes[vecToIdx(adj)];

                self.surface_area -= @boolToInt(adj_cube == .filled);
                if (adj_cube == .checked)
                    self.floodFillEnclose(adj);
            }
        }

        fn vecToIdx(vec3: Vec3) u24 {
            const w = vec3[0];
            const h = vec3[1];
            const d = vec3[2];

            return w + x * (h + @as(u24, y) * d);
        }

        fn idxToVec(idx: u24) Vec3 {
            const w = @intCast(u8, idx % x);
            const h = @intCast(u8, idx / x % y);
            const d = @intCast(u8, idx / x / y);

            return .{ w, h, d };
        }

        fn addCube(self: *Self, vec3: Vec3) void {
            self.cubes[vecToIdx(vec3)] = .filled;
            self.surface_area += 6;

            var buffer: [6]Vec3 = undefined;
            for (getAdjacents(vec3, &buffer)) |adj| {
                const idx = vecToIdx(adj);
                self.surface_area -= 2 * @enumToInt(self.cubes[idx]);
            }
        }

        fn getAdjacents(vec3: Vec3, buffer: *[6]Vec3) []Vec3 {
            var w = vec3[0];
            var h = vec3[1];
            var d = vec3[2];
            var len: u4 = 0;

            buffer[len] = Vec3{ w -% 1, h, d };
            len += @boolToInt(w != 0);
            buffer[len] = Vec3{ w +% 1, h, d };
            len += @boolToInt(w < x - 1);

            buffer[len] = Vec3{ w, h -% 1, d };
            len += @boolToInt(h != 0);
            buffer[len] = Vec3{ w, h +% 1, d };
            len += @boolToInt(h < y - 1);

            buffer[len] = Vec3{ w, h, d -% 1 };
            len += @boolToInt(d != 0);
            buffer[len] = Vec3{ w, h, d +% 1 };
            len += @boolToInt(d < z - 1);

            return buffer[0..len];
        }
    };
}
