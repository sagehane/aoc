const std = @import("std");

pub fn main() void {
    //var model = Model(102, 37);
}

const sample_input =
    \\#.######
    \\#>>.<^<#
    \\#.<..<<#
    \\#>v.><>#
    \\#<^v^^>#
    \\######.#
;

const Blizzard = packed struct(u4) {
    up: bool = false,
    down: bool = false,
    left: bool = false,
    right: bool = false,
};

const Tile = union(enum) {
    wall: void,
    ground: Blizzard,
};

const Vec2 = [2]u8;

fn Model(comptime width: u8, comptime height: u8) type {
    return struct {
        const Self = @This();
        const Grid = [height][width]Tile;

        grid: Grid,
        position: Vec2 = .{ 1, 0 },

        fn fromInput(input: []const u8) Self {
            var grid: Grid = undefined;

            var y: u8 = 0;
            var iter = std.mem.tokenize(u8, input, "\n");
            while (iter.next()) |line| {
                var x: u8 = 0;
                while (x < line.len) : (x += 1) {
                    grid[y][x] = switch (line[x]) {
                        '#' => .wall,
                        '.' => .{ .ground = .{} },
                        '^' => .{ .ground = .{ .up = true } },
                        'v' => .{ .ground = .{ .down = true } },
                        '<' => .{ .ground = .{ .left = true } },
                        '>' => .{ .ground = .{ .right = true } },
                        else => unreachable,
                    };
                }
            }

            return .{ .grid = grid };
        }

        fn preview(self: Self) Self {
            _ = self;
        }
    };
}
