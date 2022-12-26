const std = @import("std");

pub fn main() !void {
    // The cube is represented as:
    //  0
    //  1
    // 42
    //  35

    const SM = Model(4, 15);
    var sample_model = try SM.fromInput(sample_input, [6]Vec2{
        // 0, 1 needs to be flipped upside down
        .{ 0, 1 },
        // This cube is the starting point
        .{ 2, 0 },
        .{ 2, 1 },
        .{ 2, 2 },
        .{ 1, 1 },
        .{ 3, 2 },
    });

    sample_model.cube[0] = SM.transposeSquare(.down, .up, sample_model.cube[0]);
    sample_model.state = .{ .facing = .right, .cube_num = 1, .vec2 = .{ 0, 0 } };

    sample_model.traverseMaze();
    const sample_state = sample_model.state;

    std.debug.print("{}:{d}:{any} -> {d}\n", .{
        sample_state.facing,
        sample_state.cube_num,
        sample_state.vec2,
        1000 * @as(u32, sample_state.vec2[1] + 4 + 1) + 4 * (sample_state.vec2[0] + 4 + 1) + @enumToInt(sample_state.facing),
    });

    const M = Model(50, 5634);
    var model = try M.fromInput(input_str, [6]Vec2{
        // This cube is the starting point
        .{ 1, 0 },
        .{ 1, 1 },
        .{ 1, 2 },
        // Flip right -> up
        .{ 0, 3 },
        .{ 0, 2 },
        // up -> left
        .{ 2, 0 },
    });

    model.cube[3] = M.transposeSquare(.right, .up, model.cube[3]);
    model.cube[5] = M.transposeSquare(.up, .left, model.cube[5]);

    model.traverseMaze();
    const state = model.state;

    std.debug.print("{}:{d}:{any} -> {d}\n", .{
        state.facing,
        state.cube_num,
        state.vec2,
        1000 * @as(u32, state.vec2[1] + 100 + 1) + 4 * (state.vec2[0] + 0 + 1) + @enumToInt(state.facing),
    });
}

test {
    _ = Model(4, 0);
    std.testing.refAllDecls(@This());
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

    fn axisModified(self: Facing) u1 {
        return @truncate(u1, @enumToInt(self));
    }

    test {
        try std.testing.expectEqual(@as(u1, 0), Facing.right.axisModified());
        try std.testing.expectEqual(@as(u1, 1), Facing.down.axisModified());
        try std.testing.expectEqual(@as(u1, 0), Facing.left.axisModified());
        try std.testing.expectEqual(@as(u1, 1), Facing.up.axisModified());
    }

    /// 0 for moving backwards
    /// 1 for moving forwards
    fn axisOffset(self: Facing) u1 {
        return @truncate(u1, ~@enumToInt(self) >> 1);
    }

    test {
        try std.testing.expectEqual(@as(u1, 1), Facing.right.axisOffset());
        try std.testing.expectEqual(@as(u1, 1), Facing.down.axisOffset());
        try std.testing.expectEqual(@as(u1, 0), Facing.left.axisOffset());
        try std.testing.expectEqual(@as(u1, 0), Facing.up.axisOffset());
    }
};

const Instruction = union(enum) {
    number: Size,
    letter: enum(u1) { l = 0, r = 1 },
};

fn Model(comptime side: Size, comptime length: usize) type {
    return struct {
        const Self = @This();
        const Square = [side][side]Tile;
        const Cube = [6]Square;
        const State = struct { facing: Facing, vec2: Vec2, cube_num: u4 };
        const Instructions = std.BoundedArray(Instruction, length);

        cube: Cube,
        state: State,
        instructions: Instructions,

        //fn getPassword(self: Self) u32 {
        //    const row: u32 = self.position[1] + 1;
        //    const column: u32 = self.position[0] + 1;

        //    return 1000 * row + 4 * column + @enumToInt(self.facing);
        //}

        fn traverseMaze(self: *Self) void {
            for (self.instructions.constSlice()) |instruction|
                switch (instruction) {
                    .number => |number| {
                        var step = number;
                        var forward = toForward(self.state);

                        while (step > 0) {
                            const next = self.cube[forward.cube_num][forward.vec2[1]][forward.vec2[0]];

                            switch (next) {
                                .empty => unreachable,
                                .open => {
                                    self.state = forward;
                                    step -= 1;
                                    forward = toForward(forward);
                                },
                                .wall => {
                                    break;
                                },
                            }
                        }
                    },
                    .letter => |letter| {
                        self.state.facing = @intToEnum(Facing, @enumToInt(self.state.facing) +% 2 * @as(u32, @enumToInt(letter)) -% 1);
                    },
                };
        }

        fn toForward(state: State) State {
            const modified_axis = state.facing.axisModified();
            const new_coord = state.vec2[modified_axis] + (@as(u2, state.facing.axisOffset()) << 1) -% 1;
            const no_warp = (new_coord < side);

            var tmp_state = state;

            if (no_warp) {
                tmp_state.vec2[modified_axis] = new_coord;
                return tmp_state;
            }

            // Went left or up
            const cond = state.vec2[modified_axis] == 0;
            tmp_state.vec2[modified_axis] = @boolToInt(cond) * (side - 1);

            if (state.cube_num < 4) {
                if (modified_axis == 0) {
                    tmp_state.facing = @intToEnum(Facing, @boolToInt(cond) * state.cube_num + @boolToInt(!cond) * (4 - state.cube_num - 1));
                    tmp_state.cube_num = 4 + @as(u4, @boolToInt(!cond));

                    tmp_state.vec2 = transpose(state.facing, tmp_state.facing, tmp_state.vec2);

                    return tmp_state;
                }

                if (modified_axis == 1) {
                    tmp_state.cube_num = @mod(state.cube_num + 4 - @boolToInt(cond) + @boolToInt(!cond), 4);

                    return tmp_state;
                }
            }

            // right of 4 = right of 2
            // down of 4 = right of 3
            // left of 4 = right of 0
            // up of 4 = right of 1
            // right of 5 = left of 1
            // down of 5 = left of 0
            // left of 5 = left of 3
            // up of 5 = left of 2

            if (state.cube_num == 4) {
                tmp_state.facing = .right;
                tmp_state.vec2 = transpose(state.facing, tmp_state.facing, tmp_state.vec2);
                tmp_state.cube_num = @enumToInt(state.facing) +% 2;

                return tmp_state;
            }

            tmp_state.facing = .left;
            tmp_state.vec2 = transpose(state.facing, tmp_state.facing, tmp_state.vec2);
            tmp_state.cube_num = @as(u2, 1) -% @enumToInt(state.facing);

            return tmp_state;
        }

        test {
            const M = Model(4, 0);

            try std.testing.expectEqual(M.toForward(
                .{ .facing = .down, .vec2 = .{ 1, 3 }, .cube_num = 2 },
            ), .{ .facing = .down, .vec2 = .{ 1, 0 }, .cube_num = 3 });

            try std.testing.expectEqual(M.toForward(
                .{ .facing = .up, .vec2 = .{ 2, 0 }, .cube_num = 0 },
            ), .{ .facing = .up, .vec2 = .{ 2, 3 }, .cube_num = 3 });

            try std.testing.expectEqual(M.toForward(
                .{ .facing = .left, .vec2 = .{ 0, 2 }, .cube_num = 2 },
            ), .{ .facing = .left, .vec2 = .{ 3, 2 }, .cube_num = 4 });

            try std.testing.expectEqual(M.toForward(
                .{ .facing = .right, .vec2 = .{ 3, 1 }, .cube_num = 3 },
            ), .{ .facing = .right, .vec2 = .{ 0, 1 }, .cube_num = 5 });
        }

        fn transpose(from: Facing, to: Facing, vec2: Vec2) Vec2 {
            // The axis modified
            //const modified = from.axisModified();
            //const cond = vec2[modified] == 0;
            //var tmp_vec2 = vec2;
            //tmp_vec2[modified] = @boolToInt(cond) * (side - 1);

            const x = vec2[0];
            const y = vec2[1];

            return switch (@enumToInt(to) -% @enumToInt(from)) {
                0 => .{ x, y },
                1 => .{ side - y - 1, x },
                2 => .{ side - x - 1, side - y - 1 },
                3 => .{ y, side - x - 1 },
            };
        }

        fn transposeSquare(from: Facing, to: Facing, square: Square) Square {
            var new_square: Square = undefined;
            for (square) |row, y|
                for (row) |tile, x| {
                    const coord = transpose(from, to, .{ @intCast(u8, x), @intCast(u8, y) });
                    new_square[coord[1]][coord[0]] = tile;
                };

            return new_square;
        }

        test {
            const M = Model(4, 0);
            try std.testing.expectEqual(Vec2{ 2, 2 }, M.transpose(.up, .up, .{ 2, 2 }));
            try std.testing.expectEqual(Vec2{ 3, 3 }, M.transpose(.down, .up, .{ 0, 0 }));
            try std.testing.expectEqual(Vec2{ 1, 3 }, M.transpose(.right, .down, .{ 3, 2 }));
            try std.testing.expectEqual(Vec2{ 2, 0 }, M.transpose(.left, .down, .{ 3, 2 }));
            try std.testing.expectEqual(Vec2{ 2, 0 }, M.transpose(.up, .left, .{ 3, 2 }));
        }

        //fn toForward(self: Self, vec2: Vec2) Vec2 {
        //    const horizontal = -@as(i2, @boolToInt(self.facing == .left)) + @boolToInt(self.facing == .right);
        //    const vertical = -@as(i2, @boolToInt(self.facing == .up)) + @boolToInt(self.facing == .down);
        //    return .{
        //        @intCast(Size, @mod(@as(i16, vec2[0]) + horizontal, width)),
        //        @intCast(Size, @mod(@as(i16, vec2[1]) + vertical, height)),
        //    };
        //}

        fn fromInput(input: []const u8, order: [6]Vec2) !Self {
            var board = [1][4]Square{[1]Square{[1][side]Tile{[1]Tile{.empty} ** side} ** side} ** 4} ** 4;
            const separator = std.mem.indexOf(u8, input, "\n\n").?;

            var y: Size = 0;
            var iter = std.mem.tokenize(u8, input[0..separator], "\n");
            while (iter.next()) |line| : (y += 1) {
                for (line) |c, x| {
                    const board_x = x / side;
                    const board_y = y / side;
                    const square_x = @mod(x, side);
                    const square_y = @mod(y, side);

                    switch (c) {
                        ' ' => {},
                        '.' => {
                            board[board_y][board_x][square_y][square_x] = .open;
                        },
                        '#' => {
                            board[board_y][board_x][square_y][square_x] = .wall;
                        },
                        else => unreachable,
                    }
                }
            }

            var cube: Cube = undefined;
            for (order) |vec2, i| {
                cube[i] = board[vec2[1]][vec2[0]];
            }

            const state = State{
                .facing = .right,
                .vec2 = .{ 0, 0 },
                .cube_num = 0,
            };

            const instructions = try parseInstructions(input[separator + 2 ..]);

            return .{ .cube = cube, .state = state, .instructions = instructions };
        }

        fn parseInstructions(ins_str: []const u8) !Instructions {
            var instructions = try Instructions.init(0);

            var start: usize = 0;
            var i: usize = 0;
            while (i < ins_str.len) : (i += 1)
                switch (ins_str[i]) {
                    '\n' => break,
                    'L' => {
                        instructions.appendAssumeCapacity(.{ .letter = .l });
                    },
                    'R' => {
                        instructions.appendAssumeCapacity(.{ .letter = .r });
                    },
                    else => {
                        start = i;
                        while (i + 1 < ins_str.len and std.ascii.isDigit(ins_str[i + 1])) i += 1;
                        instructions.appendAssumeCapacity(.{ .number = try std.fmt.parseInt(Size, ins_str[start .. i + 1], 10) });
                    },
                };

            return instructions;
        }
    };
}
