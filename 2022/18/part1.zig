const std = @import("std");

pub fn main() void {
    std.debug.print("{d}\n", .{Model(Vec3{ 3 + 1, 3 + 1, 6 + 1 }).getSurfaceArea(sample_input)});
    std.debug.print("{d}\n", .{Model(Vec3{ 20, 20, 20 }).getSurfaceArea(input)});
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
        cubes: [size]u1 = [1]u1{0} ** size,

        fn getSurfaceArea(input_str: []const u8) u32 {
            var model = Self{};

            var iter = std.mem.tokenize(u8, input_str, "\n");
            while (iter.next()) |line| {
                const vec3 = lineToVec3(line);
                model.addCube(vec3);
            }

            return model.surface_area;
        }

        fn vecToIdx(vec3: Vec3) u24 {
            const w = vec3[0];
            const h = vec3[1];
            const d = vec3[2];

            return w + x * (h + @as(u24, y) * d);
        }

        fn addCube(self: *Self, vec3: Vec3) void {
            self.cubes[vecToIdx(vec3)] = 1;
            self.surface_area += 6;

            var buffer: [6]Vec3 = undefined;
            for (getAdjacents(vec3, &buffer)) |adj| {
                const idx = vecToIdx(adj);
                self.surface_area -= 2 * @as(u2, self.cubes[idx]);
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
