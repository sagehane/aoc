const std = @import("std");
const math = std.math;

pub fn main() !void {
    var gpa = std.heap.GeneralPurposeAllocator(.{}){};
    defer {
        const leaked = gpa.deinit();
        if (leaked) @panic("Leak detected");
    }

    var allocator = gpa.allocator();

    const SM = Model(8, 6);
    var sample_model = SM.fromInput(sample_input);

    std.debug.print("{?}\n", .{try sample_model.getMinTurnGeneric(
        0,
        SM.start,
        SM.goal,
        allocator,
    )});

    std.debug.print("{?}\n", .{try sample_model.getMinTurnGeneric(
        18,
        SM.goal,
        SM.start,
        allocator,
    )});

    std.debug.print("{?}\n", .{try sample_model.getMinTurnGeneric(
        41,
        SM.start,
        SM.goal,
        allocator,
    )});

    const M = Model(102, 37);
    var model = M.fromInput(input_str);

    std.debug.print("{?}\n", .{try model.getMinTurnGeneric(
        0,
        M.start,
        M.goal,
        allocator,
    )});

    std.debug.print("{?}\n", .{try model.getMinTurnGeneric(
        251,
        M.goal,
        M.start,
        allocator,
    )});

    std.debug.print("{?}\n", .{try model.getMinTurnGeneric(
        512,
        M.start,
        M.goal,
        allocator,
    )});
}

const input_str = @embedFile("input");

const sample_input =
    \\#.######
    \\#>>.<^<#
    \\#.<..<<#
    \\#>v.><>#
    \\#<^v^^>#
    \\######.#
;

test {
    std.testing.refAllDecls(@This());
}

const Vec2 = packed struct(u16) { x: u8, y: u8 };

fn Model(comptime width: u8, comptime height: u8) type {
    return struct {
        const Self = @This();
        const w = width - 2;
        const h = height - 2;

        const Horizontal = std.StaticBitSet(w);
        const Vertical = std.StaticBitSet(h);

        const start = Vec2{ .x = 0, .y = 0 };
        const goal = Vec2{ .x = w - 1, .y = h + 1 };

        const Item = packed struct(u32) {
            position: Vec2,
            turn_count: u16,

            fn toCost(self: Item) u16 {
                return self.turn_count + w - self.position.x + h - self.position.y;
            }

            fn lessThan(context: void, a: Item, b: Item) std.math.Order {
                _ = context;
                return switch (std.math.order(a.toCost(), b.toCost())) {
                    .eq => std.math.order(a.turn_count, b.turn_count),
                    else => |other| other,
                };
            }

            test {
                const M = Model(3, 3);

                try std.testing.expectEqual(std.math.Order.lt, M.Item.lessThan(
                    {},
                    .{ .position = .{ .x = 2, .y = 1 }, .turn_count = 3 },
                    .{ .position = .{ .x = 1, .y = 2 }, .turn_count = 4 },
                ));
            }
        };

        up: [w]Vertical,
        down: [w]Vertical,
        left: [h]Horizontal,
        right: [h]Horizontal,

        fn getMinTurnGeneric(
            self: Self,
            turn: u16,
            start_v: Vec2,
            goal_v: Vec2,
            allocator: std.mem.Allocator,
        ) !?u16 {
            const init = Item{ .position = start_v, .turn_count = turn };

            var frontier = std.PriorityQueue(Item, void, Item.lessThan).init(allocator, {});
            defer frontier.deinit();

            try frontier.add(init);

            var reached = std.AutoHashMap(Item, void).init(allocator);
            defer reached.deinit();

            try reached.put(init, {});

            var min_turn: ?u16 = null;
            var buffer: [5]Vec2 = undefined;

            while (frontier.removeOrNull()) |item| {
                const position = item.position;
                const turn_count = item.turn_count;

                if (min_turn != null and min_turn.? <= turn_count)
                    continue;

                if (@bitCast(u16, goal_v) == @bitCast(u16, position)) {
                    std.debug.print("Min turn: {d}\n", .{turn_count});
                    min_turn = turn_count;
                }

                for (self.getOptions(position, turn_count + 1, &buffer)) |next_position| {
                    const next = Item{
                        .position = next_position,
                        .turn_count = turn_count + 1,
                    };

                    if (!reached.contains(next)) {
                        try frontier.add(next);
                        try reached.put(next, {});
                    }
                }
            }

            return min_turn;
        }

        fn getMinTurn(self: Self, allocator: std.mem.Allocator) !?u16 {
            return self.getMinTurnGeneric(0, start, goal, allocator);
        }

        fn fromInput(input: []const u8) Self {
            var up = [1]Vertical{Vertical.initEmpty()} ** (w);
            var down = [1]Vertical{Vertical.initEmpty()} ** (w);
            var left = [1]Horizontal{Horizontal.initEmpty()} ** (h);
            var right = [1]Horizontal{Horizontal.initEmpty()} ** (h);

            var y: u8 = 0;
            var iter = std.mem.tokenize(u8, input, "\n");
            _ = iter.next();
            while (iter.next()) |line| : (y += 1) {
                if (y + 1 == height - 1)
                    break;

                var x: u8 = 0;
                while (x + 1 < line.len - 1) : (x += 1) {
                    switch (line[x + 1]) {
                        '.' => {},
                        '^' => up[x].set(y),
                        'v' => down[x].set(y),
                        '<' => left[y].set(x),
                        '>' => right[y].set(x),
                        else => unreachable,
                    }
                }
            }

            return .{
                .up = up,
                .down = down,
                .left = left,
                .right = right,
            };
        }

        /// Takes a buffer and returns a slice of all the possible destinations for this turn
        fn getOptions(self: Self, vec2: Vec2, turn_count: u16, buffer: *[5]Vec2) []Vec2 {
            const x = vec2.x;
            const y = vec2.y;

            var len: u8 = 0;

            buffer[len] = vec2;
            len += @boolToInt(self.isOpen(vec2, turn_count));

            var tmp_vec: Vec2 = undefined;

            tmp_vec = .{ .x = x -% 1, .y = y };
            buffer[len] = tmp_vec;
            len += @boolToInt(x != 0 and self.isOpen(tmp_vec, turn_count));

            tmp_vec = .{ .x = x + 1, .y = y };
            buffer[len] = tmp_vec;
            len += @boolToInt(x < w - 1 and self.isOpen(tmp_vec, turn_count));

            tmp_vec = .{ .x = x, .y = y -% 1 };
            buffer[len] = tmp_vec;
            len += @boolToInt(y != 0 and self.isOpen(tmp_vec, turn_count));

            tmp_vec = .{ .x = x, .y = y + 1 };
            buffer[len] = tmp_vec;
            len += @boolToInt(y < h + 1 and self.isOpen(tmp_vec, turn_count));

            return buffer[0..len];
        }

        fn isOpen(self: Self, vec2: Vec2, turn_count: u16) bool {
            const x = vec2.x;
            var y = vec2.y;

            // Everything at the first row is blocked bar x == 0
            if (y == 0)
                return (x == 0);

            if (y == h + 1)
                return (x == w - 1);

            y -= 1;

            const t_y = @mod(turn_count, h);
            const t_x = @mod(turn_count, w);

            //std.debug.print("t:{d},{any} - u:{},d:{},l:{},r:{}\n", .{
            //    turn_count,
            //    vec2,
            //    !self.up[x].isSet(@mod(y + t_y, h)),
            //    !self.down[x].isSet(@mod(h + y - t_y, h)),
            //    !self.left[y].isSet(@mod(x + t_x, w)),
            //    !self.right[y].isSet(@mod(w + x - t_x, w)),
            //});

            return !self.up[x].isSet(@mod(y + t_y, h)) and
                !self.down[x].isSet(@mod(h + y - t_y, h)) and
                !self.left[y].isSet(@mod(x + t_x, w)) and
                !self.right[y].isSet(@mod(w + x - t_x, w));
        }

        test {
            var model = Model(8, 6).fromInput(sample_input);
            var t: u16 = 0;

            try std.testing.expect(model.isOpen(.{ .x = 0, .y = 0 }, t));
            try std.testing.expect(!model.isOpen(.{ .x = 1, .y = 0 }, t));

            t += 1;
            try std.testing.expect(model.isOpen(.{ .x = 0, .y = 1 }, t));

            t += 1;
            try std.testing.expect(model.isOpen(.{ .x = 0, .y = 2 }, t));

            t += 1;
            try std.testing.expect(model.isOpen(.{ .x = 0, .y = 2 }, t));

            t += 1;
            try std.testing.expect(model.isOpen(.{ .x = 0, .y = 1 }, t));

            t += 1;
            try std.testing.expect(model.isOpen(.{ .x = 1, .y = 1 }, t));

            t += 1;
            try std.testing.expect(model.isOpen(.{ .x = 2, .y = 1 }, t));

            t += 1;
            try std.testing.expect(model.isOpen(.{ .x = 2, .y = 2 }, t));

            t += 1;
            try std.testing.expect(model.isOpen(.{ .x = 1, .y = 2 }, t));

            t += 1;
            try std.testing.expect(model.isOpen(.{ .x = 1, .y = 1 }, t));

            t += 1;
            try std.testing.expect(model.isOpen(.{ .x = 2, .y = 1 }, t));

            t += 1;
            try std.testing.expect(model.isOpen(.{ .x = 2, .y = 1 }, t));

            t += 1;
            try std.testing.expect(model.isOpen(.{ .x = 2, .y = 2 }, t));

            t += 1;
            try std.testing.expect(model.isOpen(.{ .x = 2, .y = 3 }, t));

            t += 1;
            try std.testing.expect(model.isOpen(.{ .x = 3, .y = 3 }, t));

            t += 1;
            try std.testing.expect(model.isOpen(.{ .x = 4, .y = 3 }, t));

            t += 1;
            try std.testing.expect(model.isOpen(.{ .x = 5, .y = 3 }, t));

            t += 1;
            try std.testing.expect(model.isOpen(.{ .x = 5, .y = 4 }, t));

            t += 1;
            try std.testing.expect(model.isOpen(.{ .x = 5, .y = 5 }, t));
        }
    };
}
