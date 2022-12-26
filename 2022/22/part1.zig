const std = @import("std");

pub fn main() !void {
    var sample_model = try Model(16, 14, 15).fromInput(sample_input);
    sample_model.traverseMaze();

    std.debug.print("{d}\n", .{sample_model.getPassword()});

    var model = try Model(150, 200, 5634).fromInput(input_str);
    model.traverseMaze();

    std.debug.print("{d}\n", .{model.getPassword()});
}

const input_str = @embedFile("input");

const sample_input =
    \\        ...#
    \\        .#..
    \\        #...
    \\        ....
    \\...#.......#
    \\........#...
    \\..#....#....
    \\..........#.
    \\        ...#....
    \\        .....#..
    \\        .#......
    \\        ......#.
    \\
    \\10R5L5R10L4R5L5
;

const Size = u8;
const Vec2 = [2]Size;

const Tile = enum(u2) {
    empty,
    open,
    wall,
};

const Facing = enum(u2) {
    right = 0,
    down = 1,
    left = 2,
    up = 3,
};

const Instruction = union(enum) {
    number: Size,
    letter: enum(u1) { l = 0, r = 1 },
};

fn Model(comptime width: Size, comptime height: Size, comptime length: usize) type {
    return struct {
        const Self = @This();
        const Board = [height][width]Tile;
        const Instructions = std.BoundedArray(Instruction, length);

        board: Board,
        position: Vec2,
        facing: Facing = .right,
        instructions: Instructions,

        fn getPassword(self: Self) u32 {
            const row: u32 = self.position[1] + 1;
            const column: u32 = self.position[0] + 1;

            return 1000 * row + 4 * column + @enumToInt(self.facing);
        }

        fn traverseMaze(self: *Self) void {
            for (self.instructions.constSlice()) |instruction|
                switch (instruction) {
                    .number => |number| {
                        var step = number;
                        var forward = self.toForward(self.position);

                        while (step > 0) {
                            const next = self.board[forward[1]][forward[0]];

                            switch (next) {
                                .empty => forward = self.toForward(forward),
                                .open => {
                                    self.position = forward;
                                    step -= 1;
                                    forward = self.toForward(forward);
                                },
                                .wall => {
                                    break;
                                },
                            }
                        }
                    },
                    .letter => |letter| {
                        self.facing = @intToEnum(Facing, @enumToInt(self.facing) +% 2 * @as(u32, @enumToInt(letter)) -% 1);
                    },
                };
        }

        fn toForward(self: Self, vec2: Vec2) Vec2 {
            const horizontal = -@as(i2, @boolToInt(self.facing == .left)) + @boolToInt(self.facing == .right);
            const vertical = -@as(i2, @boolToInt(self.facing == .up)) + @boolToInt(self.facing == .down);
            return .{
                @intCast(Size, @mod(@as(i16, vec2[0]) + horizontal, width)),
                @intCast(Size, @mod(@as(i16, vec2[1]) + vertical, height)),
            };
        }

        fn fromInput(input: []const u8) !Self {
            var board: Board = [1][width]Tile{[1]Tile{.empty} ** width} ** height;
            var w: Size = width;
            var h: Size = height;

            const separator = std.mem.indexOf(u8, input, "\n\n").?;

            var y: Size = 0;
            var iter = std.mem.tokenize(u8, input[0..separator], "\n");
            while (iter.next()) |line| : (y += 1) {
                for (line) |c, x|
                    switch (c) {
                        ' ' => {},
                        '.' => {
                            w -= @boolToInt(w == width) * (width - @intCast(Size, x));
                            h -= @boolToInt(h == height) * (height - y);
                            board[y][x] = .open;
                        },
                        '#' => {
                            w -= @boolToInt(w == width) * (width - @intCast(Size, x));
                            h -= @boolToInt(h == height) * (height - y);
                            board[y][x] = .wall;
                        },
                        else => unreachable,
                    };
            }

            var position: Vec2 = .{ w, h };
            const instructions = try parseInstructions(input[separator + 2 ..]);

            return .{ .board = board, .position = position, .instructions = instructions };
        }

        fn parseInstructions(ins_str: []const u8) !Instructions {
            var instructions = try Instructions.init(0);

            var start: usize = 0;
            var i: usize = 0;
            while (i < ins_str.len) : (i += 1)
                switch (ins_str[i]) {
                    '0'...'9' => {
                        start = i;
                        while (i + 1 < ins_str.len and std.ascii.isDigit(ins_str[i + 1])) i += 1;
                        instructions.appendAssumeCapacity(.{ .number = try std.fmt.parseInt(Size, ins_str[start .. i + 1], 10) });
                    },
                    'L' => {
                        instructions.appendAssumeCapacity(.{ .letter = .l });
                    },
                    'R' => {
                        instructions.appendAssumeCapacity(.{ .letter = .r });
                    },
                    '\n' => break,
                    else => unreachable,
                };

            return instructions;
        }
    };
}
