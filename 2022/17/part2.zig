// Note on how I solved it:
// I checked the state of the board after every loop of the input to look out for a patttern
// I deduced that the first cycle starts at 1733 rocks and every 1740 rocks from that point
// Every succeeding cycle takes 1740 rocks and adds 2759 height
// Thus, `f(n) = (n - 1733) / 1740 * 2759 + f((n - 1733) % 1740 + 1733)

const std = @import("std");

const first_cycle_rock = 1733;
const cycle_rock = 1740;
const cycle_height = 2759;
const n = 1_000_000_000_000;

//const num = std.math.maxInt(u13);
const num = (n - first_cycle_rock) % cycle_rock + first_cycle_rock;

pub fn main() void {
    //std.debug.print("{d}\n", .{Chamber.readInput(sample_input)});
    std.debug.print("{d}\n", .{(n - first_cycle_rock) / cycle_rock * cycle_height + @as(u64, Chamber.readInput(input))});
}

const sample_input = ">>><<><>><<<>><>>><<<>>><<<><<<>><>><<>>";
const input = @embedFile("input");

const Rock = [4]u4;

const rocks = [5]Rock{ .{
    0b0000,
    0b0000,
    0b0000,
    0b1111,
}, .{
    0b0000,
    0b0100,
    0b1110,
    0b0100,
}, .{
    0b0000,
    0b0010,
    0b0010,
    0b1110,
}, .{
    0b1000,
    0b1000,
    0b1000,
    0b1000,
}, .{
    0b0000,
    0b0000,
    0b1100,
    0b1100,
} };

fn minCtz(rock: Rock) u3 {
    var x: u3 = 7;

    for (rock) |mask| {
        x = std.math.min(x, @ctz(@as(u7, mask) << 3));
    }

    return x;
}

fn getMask(mask: u4, x: u3) u7 {
    return @as(u7, mask) << 3 >> x;
}

test {
    try std.testing.expectEqual(@as(u7, 0b1100000), getMask(0b1100, 0));
    try std.testing.expectEqual(@as(u7, 0b0000011), getMask(0b1100, 5));
    try std.testing.expectEqual(@as(u7, 0b0001100), getMask(0b1100, 3));
}

test {
    try std.testing.expectEqual(@as(u3, 3), minCtz(rocks[0]));
    try std.testing.expectEqual(@as(u3, 4), minCtz(rocks[1]));
    try std.testing.expectEqual(@as(u3, 4), minCtz(rocks[2]));
    try std.testing.expectEqual(@as(u3, 6), minCtz(rocks[3]));
    try std.testing.expectEqual(@as(u3, 5), minCtz(rocks[4]));
}

const Coord = struct { x: u3, y: u16 };

const Chamber = struct {
    const size = std.math.maxInt(u16);
    const Grid = [size]u7;

    max_height: u16 = 0,
    grid: Grid = [1]u7{0} ** size,

    fn readInput(input_str: []const u8) u16 {
        var chamber = Chamber{};

        var rock_count: u16 = 0;
        var rock = rocks[0];
        var min_ctz: u3 = 3;
        var coord_x: u3 = 2;
        var coord_y: u16 = 3;

        blk: while (true) {
            for (input_str) |c| {
                //std.debug.print("Rock {d}: {c}, {any}\n", .{
                //    rock_count,
                //    c,
                //    .{ coord_x + 1, coord_y + 1 },
                //});

                switch (c) {
                    '>' => {
                        coord_x = coord_x + @boolToInt(coord_x != min_ctz and !chamber.detectCollision(rock, .{ .x = coord_x + 1, .y = coord_y }));
                    },
                    '<' => {
                        coord_x = coord_x - @boolToInt(coord_x != 0 and !chamber.detectCollision(rock, .{ .x = coord_x - 1, .y = coord_y }));
                    },
                    '\n' => continue,
                    else => unreachable,
                }

                if (coord_y == 0 or chamber.detectCollision(rock, .{ .x = coord_x, .y = coord_y - 1 })) {
                    chamber.placeRock(rock, .{ .x = coord_x, .y = coord_y });
                    rock_count += 1;
                    rock = rocks[rock_count % rocks.len];
                    min_ctz = minCtz(rock);
                    coord_x = 2;
                    coord_y = chamber.max_height + 3;

                    //std.debug.print("\n", .{});
                    //var i = chamber.max_height;
                    //while (i != 0) : (i -= 1) {
                    //    std.debug.print("{b:0>7}\n", .{chamber.grid[i - 1]});
                    //}
                    //std.debug.print("\n", .{});

                    //if (rock_count == 4)
                    if (rock_count == num)
                        break :blk;
                } else {
                    coord_y -= 1;
                }
            }

            std.debug.print("Stone: {d}, height: {d}:{d}\n", .{ rock_count, @as(i17, coord_y) - chamber.max_height, chamber.max_height });
            var i = chamber.max_height;
            while (i != chamber.max_height - 5) : (i -= 1) {
                std.debug.print("{b:0>7}\n", .{chamber.grid[i - 1]});
            }
            std.debug.print("\n", .{});
        }

        return chamber.max_height;
    }

    fn detectCollision(self: Chamber, rock: Rock, coord: Coord) bool {
        var x = false;

        for (self.grid[coord.y .. coord.y + 4]) |mask, i| {
            x = x or (mask & getMask(rock[4 - i - 1], coord.x) != 0);
        }

        return x;
    }

    fn placeRock(self: *Chamber, rock: Rock, coord: Coord) void {
        for (self.grid[coord.y .. coord.y + 4]) |*mask, i| {
            const rock_mask = getMask(rock[4 - i - 1], coord.x);

            self.max_height = std.math.max(self.max_height, @boolToInt(rock_mask != 0) * (coord.y + @intCast(u3, i) + 1));

            std.debug.assert(mask.* | rock_mask == mask.* ^ rock_mask);
            mask.* ^= rock_mask;
        }
    }
};
