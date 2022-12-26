const std = @import("std");

//const height = 5;
//const width = 8;
//const Size = u8;
const height = 41;
const width = 162;
const Size = u16;

pub fn main() void {
    //const maze = Maze.inputToMaze(sample_input);
    const maze = Maze.inputToMaze(input);

    std.debug.print("{d}\n", .{maze.solveMaze()});
}

const input = @embedFile("input");

const sample_input =
    \\Sabqponm
    \\abcryxxl
    \\accszExk
    \\acctuvwj
    \\abdefghi
;

const Route = [height][width]Size;
const Grid = [height][width]u8;
const Coord = [2]u8;

fn getIndex(grid: Grid, needle: u8) Coord {
    for (grid) |line, y|
        for (line) |haystack, x|
            if (needle == haystack)
                return .{ @intCast(u8, x), @intCast(u8, y) };
    unreachable;
}

const Maze = struct {
    start: Coord,
    grid: Grid,

    fn inputToMaze(input_text: []const u8) Maze {
        var grid: Grid = undefined;

        var iter = std.mem.tokenize(u8, input_text, "\n");
        var i: u8 = 0;
        while (iter.next()) |line| {
            defer i += 1;
            std.mem.copy(u8, &grid[i], line);
        }

        const start = getIndex(grid, 'E');
        const special = getIndex(grid, 'S');

        grid[start[1]][start[0]] = 'z';
        grid[special[1]][special[0]] = 'a';

        return .{ .start = start, .grid = grid };
    }

    fn solveMaze(maze: Maze) Size {
        //var route: Route = [1][width]Size{[1]Size{31} ** width} ** height;
        var route: Route = [1][width]Size{[1]Size{437} ** width} ** height;

        //var min_step: Size = 31;
        var min_step: Size = 437;

        maze.updateRoute(&route, &min_step, maze.start, 0);

        return min_step;
    }

    fn coordValue(maze: Maze, coord: Coord) u8 {
        return maze.grid[coord[1]][coord[0]];
    }
    fn compareValue(val1: u8, val2: u8) bool {
        return val2 + 1 >= val1;
    }

    fn getAdjacents(maze: Maze, coord: Coord, buffer: *[4]Coord) []Coord {
        const x = coord[0];
        const y = coord[1];
        const val = maze.coordValue(coord);

        var len: u8 = 0;
        var tmp_coord: Coord = undefined;

        tmp_coord = .{ x -% 1, y };
        buffer[len] = tmp_coord;
        len += @boolToInt(x > 0 and compareValue(val, maze.coordValue(tmp_coord)));

        tmp_coord = .{ x, y -% 1 };
        buffer[len] = tmp_coord;
        len += @boolToInt(y > 0 and compareValue(val, maze.coordValue(tmp_coord)));

        tmp_coord = .{ x +% 1, y };
        buffer[len] = tmp_coord;
        len += @boolToInt(x < width - 1 and compareValue(val, maze.coordValue(tmp_coord)));

        tmp_coord = .{ x, y +% 1 };
        buffer[len] = tmp_coord;
        len += @boolToInt(y < height - 1 and compareValue(val, maze.coordValue(tmp_coord)));

        return buffer[0..len];
    }

    fn updateRoute(
        maze: Maze,
        route: *Route,
        min_step: *Size,
        coord: Coord,
        count: Size,
    ) void {
        const x = coord[0];
        const y = coord[1];

        if (maze.grid[coord[1]][coord[0]] == 'a') {
            min_step.* = count;

            std.debug.print("Count: {d}, Min Step: {d} Coord: {any}\n", .{ count, min_step.*, coord });
        }

        if (count < min_step.* and count < route[y][x]) {
            route[y][x] = count;

            var buffer: [4]Coord = undefined;
            for (maze.getAdjacents(coord, &buffer)) |adjacent| {
                maze.updateRoute(route, min_step, adjacent, count + 1);
            }
        }
    }
};
