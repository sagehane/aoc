const std = @import("std");

pub fn main() void {
    std.debug.print("{any}\n", .{getDimensions(sample_input)});
    std.debug.print("{any}\n", .{getDimensions(input)});

    //std.debug.print("{d}\n", .{part1(.{ 494, 503 }, .{ 0, 9 }, sample_input)});
    std.debug.print("{d}\n", .{part1(.{ 490, 573 }, .{ 0, 182 }, input)});
}

fn getDimensions(input_str: []const u8) [2]Coord {
    var x_range = Coord{ 500, 500 };
    var y_range = Coord{ 0, 0 };

    var iter = std.mem.tokenize(u8, input_str, "\n");
    while (iter.next()) |line| {
        var line_iter = std.mem.tokenize(u8, line, " -> ");
        while (line_iter.next()) |coord_str| {
            const coord = parseCoord(coord_str);

            x_range = Coord{
                std.math.min(x_range[0], coord[0]),
                std.math.max(x_range[1], coord[0]),
            };

            y_range = Coord{
                std.math.min(y_range[0], coord[1]),
                std.math.max(y_range[1], coord[1]),
            };
        }
    }

    return .{ x_range, y_range };
}

const input = @embedFile("input");

const sample_input =
    \\498,4 -> 498,6 -> 496,6
    \\503,4 -> 502,4 -> 502,9 -> 494,9
;

fn part1(
    comptime x_range: [2]u16,
    comptime y_range: [2]u16,
    input_str: []const u8,
) u16 {
    var model = Model(x_range, y_range).parseInput(input_str);

    var count: u16 = 0;
    while (model.fillSand()) : (count += 1) {}

    return count;
}

const Tile = enum { air, rock, sand };

const Coord = [2]u16;

fn parseCoord(str: []const u8) Coord {
    const idx = std.mem.indexOf(u8, str, ",").?;
    return .{
        std.fmt.parseInt(u16, str[0..idx], 10) catch unreachable,
        std.fmt.parseInt(u16, str[idx + 1 ..], 10) catch unreachable,
    };
}

fn Model(comptime x_range: [2]u16, comptime y_range: [2]u16) type {
    return struct {
        const Self = @This();
        const height = y_range[1] - y_range[0] + 1;
        const width = x_range[1] - x_range[0] + 1;
        const Grid = [height][width]Tile;

        grid: Grid = [1][width]Tile{[1]Tile{.air} ** width} ** height,

        fn parseInput(input_str: []const u8) Self {
            var self = Self{};

            var iter = std.mem.tokenize(u8, input_str, "\n");
            while (iter.next()) |line|
                self.parseLine(line);

            return self;
        }

        fn offsetCoord(coord: Coord) Coord {
            return Coord{
                coord[0] - x_range[0],
                coord[1] - y_range[0],
            };
        }

        fn parseLine(self: *Self, line: []const u8) void {
            var iter = std.mem.tokenize(u8, line, " -> ");

            var start = offsetCoord(parseCoord(iter.next().?));

            while (iter.next()) |coord_str| {
                const tmp_coord = offsetCoord(parseCoord(coord_str));
                self.addRocks(start, tmp_coord);
                start = tmp_coord;
            }
        }

        fn addRocks(self: *Self, start: Coord, end: Coord) void {
            const cond = start[0] == end[0];
            const idx = @boolToInt(cond);
            const min = std.math.min(start[idx], end[idx]);
            const max = std.math.max(start[idx], end[idx]);

            // Change in height
            if (cond) {
                var i: u8 = 0;
                while (i <= max - min) : (i += 1) {
                    self.grid[min + i][start[~idx]] = .rock;
                }
            }

            // Change in width
            else {
                var i: u8 = 0;
                while (i <= max - min) : (i += 1) {
                    self.grid[start[~idx]][min + i] = .rock;
                }
            }
        }

        // Returns false if filling does nothing
        fn fillSand(self: *Self) bool {
            return self.fillCoord(offsetCoord(.{ 500, 0 }));
        }

        fn fillCoord(self: *Self, coord: Coord) bool {
            const x = coord[0];
            const y = coord[1];

            // Fails if the sand fell to the bottom
            if (y == height - 1) return false;
            if (self.getCoord(.{ x, y + 1 }) == .air)
                return self.fillCoord(.{ x, y + 1 });

            // Fails if the sand fell to left edge
            if (x == 0) return false;
            if (self.getCoord(.{ x - 1, y + 1 }) == .air)
                return self.fillCoord(.{ x - 1, y + 1 });

            // Fails if the sand fell to right edge
            if (x == width - 1) return false;
            if (self.getCoord(.{ x + 1, y + 1 }) == .air)
                return self.fillCoord(.{ x + 1, y + 1 });

            self.grid[y][x] = .sand;

            return true;
        }

        fn getCoord(self: Self, coord: Coord) Tile {
            return self.grid[coord[1]][coord[0]];
        }
    };
}
