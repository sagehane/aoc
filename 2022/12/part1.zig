const std = @import("std");

pub fn main() void {
    Maze(8, 5).fromInput(sample_input).breadthFirstSearch();

    //const width = 162;
    //const height = 41;
    //breadWidthSearch(width, height, input);
}

const input = @embedFile("input");

const sample_input =
    \\Sabqponm
    \\abcryxxl
    \\accszExk
    \\acctuvwj
    \\abdefghi
;

const Coord = [2]u8;

const DirMask = packed struct(u4) {
    up: bool = false,
    left: bool = false,
    right: bool = false,
    down: bool = false,
};

fn Maze(comptime width: u8, comptime height: u8) type {
    return struct {
        const Self = @This();
        const Grid = [height][width]u8;

        start: Coord,
        end: Coord,
        grid: Grid,

        fn fromInput(input_str: []const u8) Self {
            var grid: Grid = undefined;

            var iter = std.mem.tokenize(u8, input_str, "\n");
            var i: u8 = 0;
            while (iter.next()) |line| {
                defer i += 1;
                std.mem.copy(u8, &grid[i], line);
            }

            const start = getCoord(grid, 'S').?;
            const end = getCoord(grid, 'E').?;

            grid[start[1]][start[0]] = 'a';
            grid[end[1]][end[0]] = 'z';

            return .{ .start = start, .end = end, .grid = grid };
        }

        fn breadthFirstSearch(self: Self) void {
            var frontier = std.BoundedArray(Coord, @as(u16, width) * height).init(0) catch unreachable;
            frontier.appendAssumeCapacity(self.start);

            var came_from: [height][width]?Coord = [1][width]?Coord{[1]?Coord{null} ** width} ** height;
            came_from[self.start[1]][self.start[0]] = null;

            // Faster but doesn't traverse the map evenly
            //while (frontier.popOrNull()) |current| {
            while (frontier.len != 0) {
                const current = frontier.orderedRemove(0);

                if (std.mem.eql(u8, &current, &self.end)) {
                    break;
                }

                var buffer: [4]Coord = undefined;
                for (self.getAdjacents(current, &buffer)) |next| {
                    if (came_from[next[1]][next[0]] == null and
                        !std.mem.eql(u8, &next, &self.start))
                    {
                        frontier.appendAssumeCapacity(next);
                        came_from[next[1]][next[0]] = current;
                    }
                }
            }

            var next = came_from[self.end[1]][self.end[0]];
            if (next == null) {
                std.debug.print("Goal wasn't reached\n", .{});
                return;
            }

            var i: u16 = 0;
            while (next) |coord| {
                i += 1;
                next = came_from[coord[1]][coord[0]];
            }
            std.debug.print("{d}\n", .{i});
        }

        //fn breadthFirstSearch(self: Self) void {
        //    var frontier = std.BoundedArray(Coord, @as(u16, width) * height).init(0) catch unreachable;
        //    frontier.appendAssumeCapacity(self.start);

        //    var reached: [height][width]bool = [1][width]bool{[1]bool{false} ** width} ** height;
        //    reached[self.start[1]][self.start[0]] = true;

        //    // Faster but doesn't traverse the map in all directons
        //    //while (frontier.popOrNull()) |current| {
        //    while (frontier.len != 0) {
        //        const current = frontier.orderedRemove(0);
        //        if (std.mem.eql(u8, &current, &self.end)) {
        //            break;
        //        }

        //        var buffer: [4]Coord = undefined;
        //        for (self.getAdjacents(current, &buffer)) |next| {
        //            if (!reached[next[1]][next[0]]) {
        //                frontier.appendAssumeCapacity(next);
        //                reached[next[1]][next[0]] = true;

        //                std.debug.print("{c}@{any} -> {c}@{any}\n", .{
        //                    self.coordValue(current),
        //                    current,
        //                    self.coordValue(next),
        //                    next,
        //                });
        //            }
        //        }
        //    }
        //}

        fn getCoord(grid: Grid, needle: u8) ?Coord {
            for (grid) |line, y|
                if (std.mem.indexOf(u8, &line, &.{needle})) |x|
                    return .{ @intCast(u8, x), @intCast(u8, y) };

            return null;
        }

        fn getAdjacents(self: Self, coord: Coord, buffer: *[4]Coord) []Coord {
            const x = coord[0];
            const y = coord[1];
            const val = self.coordValue(coord);

            var len: u8 = 0;
            var tmp_coord: Coord = undefined;

            tmp_coord = .{ x -% 1, y };
            buffer[len] = tmp_coord;
            len += @boolToInt(x > 0 and compareValue(val, self.coordValue(tmp_coord)));

            tmp_coord = .{ x, y -% 1 };
            buffer[len] = tmp_coord;
            len += @boolToInt(y > 0 and compareValue(val, self.coordValue(tmp_coord)));

            tmp_coord = .{ x +% 1, y };
            buffer[len] = tmp_coord;
            len += @boolToInt(x < width - 1 and compareValue(val, self.coordValue(tmp_coord)));

            tmp_coord = .{ x, y +% 1 };
            buffer[len] = tmp_coord;
            len += @boolToInt(y < height - 1 and compareValue(val, self.coordValue(tmp_coord)));

            return buffer[0..len];
        }

        fn coordValue(self: Self, coord: Coord) u8 {
            return self.grid[coord[1]][coord[0]];
        }

        fn compareValue(x: u8, y: u8) bool {
            return x + 1 >= y;
        }
    };
}
