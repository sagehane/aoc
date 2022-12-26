const std = @import("std");
const math = std.math;
const max = math.max;
const min = math.min;

pub fn main() !void {
    var gpa = std.heap.GeneralPurposeAllocator(.{}){};
    var allocator = gpa.allocator();

    var sample_model = Model(7, 7).fromInput(sample_input);
    var sample_count: u8 = 0;
    while (try sample_model.round(allocator)) : (sample_count += 1) {}
    std.debug.print("{d}\n", .{sample_count + 1});

    var model = Model(70, 70).fromInput(input_str);
    var count: u16 = 0;
    while (try model.round(allocator)) : (count += 1) {}
    std.debug.print("{d}\n", .{count + 1});
}

const input_str = @embedFile("input");

const sample_input =
    \\....#..
    \\..###.#
    \\#...#.#
    \\.#...##
    \\#.###..
    \\##.#.##
    \\.#..#..
;

const Tile = enum(u1) { empty, elf };
const Vec2 = [2]u8;
const Direction = enum(u2) {
    north,
    south,
    west,
    east,

    fn toNext(self: Direction) Direction {
        return @intToEnum(Direction, @enumToInt(self) +% 1);
    }
};

fn Model(comptime width: u8, comptime height: u8) type {
    return struct {
        const Self = @This();
        const round_count = 80;
        const w = width + 2 * round_count;
        const h = height + 2 * round_count;
        const Grid = [h][w]Tile;
        const HashMap = std.AutoArrayHashMap(Vec2, ?Vec2);

        elves: u16,
        direction: Direction = .north,
        grid: Grid,

        fn print(self: Self) void {
            for (self.grid) |row| {
                for (row) |tile| {
                    const c: u8 = switch (tile) {
                        .empty => '.',
                        .elf => '#',
                    };
                    std.debug.print("{c}", .{c});
                }
                std.debug.print("\n", .{});
            }
        }

        fn fromInput(input: []const u8) Self {
            var elves: u16 = 0;
            //var dimensions: [2]Vec2 = undefined;
            var grid: Grid = [1][w]Tile{[1]Tile{.empty} ** w} ** h;

            var iter = std.mem.tokenize(u8, input, "\n");
            var x: u8 = round_count;
            var y: u8 = round_count;
            while (iter.next()) |line| : ({
                x = round_count;
                y += 1;
            }) {
                while (x < line.len + round_count) : (x += 1) {
                    switch (line[x - round_count]) {
                        '#' => {
                            elves += 1;
                            grid[y][x] = .elf;
                        },
                        '.' => {},
                        else => unreachable,
                    }
                }
            }

            //dimensions[0] = .{ round_count, x };
            //dimensions[1] = .{ round_count, y };

            return .{ .elves = elves, .grid = grid };
        }

        fn calculateEmpties(self: Self) u16 {
            var x_min: u8 = 255;
            var x_max: u8 = 0;
            var y_min: u8 = 255;
            var y_max: u8 = 0;

            for (self.grid) |row, y| for (row) |tile, x| {
                if (tile == .elf) {
                    x_min = min(x_min, @intCast(u8, x));
                    x_max = max(x_max, @intCast(u8, x));
                    y_min = min(y_min, @intCast(u8, y));
                    y_max = max(y_max, @intCast(u8, y));
                }
            };

            const x_range: u16 = x_max - x_min + 1;
            const y_range: u16 = y_max - y_min + 1;

            return x_range * y_range - self.elves;
        }

        fn round(self: *Self, allocator: std.mem.Allocator) !bool {
            var hash_map = HashMap.init(allocator);
            defer hash_map.deinit();

            for (self.grid) |row, y|
                for (row) |tile, x| {
                    if (tile == .elf) {
                        const from = .{ @intCast(u8, x), @intCast(u8, y) };
                        const mask = self.getAdjacents(from);

                        //std.debug.print("{any}:{b:0>8}:{}\n", .{ from, mask, self.direction });

                        if (mask != 0) {
                            var i: u8 = 0;
                            var direction = self.direction;
                            while (i < 4 and !(try moveDirection(
                                from,
                                mask,
                                direction,
                                &hash_map,
                            ))) : ({
                                i += 1;
                                direction = direction.toNext();
                            }) {}
                        }
                    }
                };

            var changed = false;

            var iter = hash_map.iterator();
            while (iter.next()) |entry| {
                if (entry.value_ptr.*) |from| {
                    changed = true;

                    const to = entry.key_ptr.*;
                    const to_x = to[0];
                    const to_y = to[1];

                    self.grid[to_y][to_x] = self.grid[from[1]][from[0]];
                    self.grid[from[1]][from[0]] = .empty;
                }
            }

            self.direction = self.direction.toNext();
            return changed;
        }

        fn moveDirection(
            from: Vec2,
            mask: u8,
            direction: Direction,
            hash_map: *HashMap,
        ) !bool {
            const x = from[0];
            const y = from[1];

            const n_mask = 0b01101011;
            const s_mask = 0b11010110;
            const w_mask = 0b00011111;
            const e_mask = 0b11111000;

            var cond = false;
            var to: Vec2 = undefined;

            switch (direction) {
                .north => {
                    cond = mask | n_mask == n_mask;
                    to = Vec2{ x, y - 1 };
                },
                .south => {
                    cond = mask | s_mask == s_mask;
                    to = Vec2{ x, y + 1 };
                },
                .west => {
                    cond = mask | w_mask == w_mask;
                    to = Vec2{ x - 1, y };
                },
                .east => {
                    cond = mask | e_mask == e_mask;
                    to = Vec2{ x + 1, y };
                },
            }

            if (cond) {
                //std.debug.print("{}:{any}->{any}\n", .{ direction, from, to });

                if (!hash_map.contains(to)) {
                    try hash_map.put(to, from);
                } else {
                    try hash_map.put(to, null);
                }
            }

            return cond;
        }

        fn getAdjacents(self: Self, vec2: Vec2) u8 {
            const x = vec2[0];
            const y = vec2[1];
            var mask: u8 = 0;

            mask += @boolToInt(x != 0 and y != 0 and self.get(.{ x - 1, y - 1 }) == .elf);
            mask <<= 1;
            mask += @boolToInt(x != 0 and self.get(.{ x - 1, y }) == .elf);
            mask <<= 1;
            mask += @boolToInt(x != 0 and y < h - 1 and self.get(.{ x - 1, y + 1 }) == .elf);

            mask <<= 1;
            mask += @boolToInt(y != 0 and self.get(.{ x, y - 1 }) == .elf);
            mask <<= 1;
            mask += @boolToInt(y < h - 1 and self.get(.{ x, y + 1 }) == .elf);
            mask <<= 1;

            mask += @boolToInt(x < w - 1 and y != 0 and self.get(.{ x + 1, y - 1 }) == .elf);
            mask <<= 1;
            mask += @boolToInt(x < w - 1 and self.get(.{ x + 1, y }) == .elf);
            mask <<= 1;
            mask += @boolToInt(x < w - 1 and y < h - 1 and self.get(.{ x + 1, y + 1 }) == .elf);

            return mask;
        }

        fn get(self: Self, vec2: Vec2) Tile {
            return self.grid[vec2[1]][vec2[0]];
        }
    };
}
