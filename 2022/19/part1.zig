const std = @import("std");
const expectEqual = std.testing.expectEqual;

pub fn main() void {
    std.debug.print("{d}\n", .{Model.fromString(sample_input).getQualityLevel()});
    std.debug.print("{d}\n", .{part1(input)});
}

const input = @embedFile("input");

const sample_models = [2]Model{
    .{
        .ore_cost = 4,
        .clay_cost = 2,
        .obsidian_cost = .{ 3, 14 },
        .geode_cost = .{ 2, 7 },
    },
    .{
        .ore_cost = 2,
        .clay_cost = 3,
        .obsidian_cost = .{ 3, 8 },
        .geode_cost = .{ 3, 12 },
    },
};

const sample_input =
    \\Blueprint 1: Each ore robot costs 4 ore.
    \\  Each clay robot costs 2 ore.
    \\  Each obsidian robot costs 3 ore and 14 clay.
    \\  Each geode robot costs 2 ore and 7 obsidian.
    \\
    \\Blueprint 2:
    \\  Each ore robot costs 2 ore.
    \\  Each clay robot costs 3 ore.
    \\  Each obsidian robot costs 3 ore and 8 clay.
    \\  Each geode robot costs 3 ore and 12 obsidian.
;

fn part1(input_str: []const u8) u16 {
    var sum: u16 = 0;

    var iter = std.mem.tokenize(u8, input_str, "\n");
    var x: u8 = 1;
    while (iter.next()) |line| : (x += 1) {
        sum += x * Model.fromString(line).getQualityLevel();
    }

    return sum;
}

test {
    std.testing.refAllDecls(@This());
}

const Stat = packed struct(u24) {
    amount: u16 = 0,
    production: u8 = 0,

    fn produce(self: *Stat, time: u8) void {
        self.amount += self.production * time;
    }

    // Returns `0` iff `amount < cost and production == 0` or integer under/overflow
    fn timeRequired(self: Stat, cost: u8) u8 {
        const a = self.amount;
        const p = self.production;

        const cond = a < cost and p == 0;
        //std.math.divCeil
        const result = @intCast(u8, (@as(u16, cost) -| a + p -| 1) / std.math.max(1, p)) + 1;

        return @boolToInt(!cond) * result;
    }

    test {
        try expectEqual(@as(u8, 0), timeRequired(.{
            .amount = 0,
            .production = 0,
        }, 1));

        try expectEqual(@as(u8, 0), timeRequired(.{
            .amount = 1,
            .production = 0,
        }, 2));

        try expectEqual(@as(u8, 1), timeRequired(.{
            .amount = 0,
            .production = 0,
        }, 0));

        try expectEqual(@as(u8, 2), timeRequired(.{
            .amount = 0,
            .production = 1,
        }, 1));

        try expectEqual(@as(u8, 2), timeRequired(.{
            .amount = 0,
            .production = 2,
        }, 1));

        try expectEqual(@as(u8, 2), timeRequired(.{
            .amount = 0,
            .production = 3,
        }, 3));

        try expectEqual(@as(u8, 0), timeRequired(.{
            .amount = 0,
            .production = 0,
        }, 7));

        const model = sample_models[0];
        try expectEqual(@as(u8, 0), model.obsidian_stat.timeRequired(model.geode_cost[1]));
    }

    fn timeRequiredTwo(x: Stat, y: Stat, costs: [2]u8) u8 {
        const t_x = x.timeRequired(costs[0]);
        const t_y = y.timeRequired(costs[1]);

        return @boolToInt(t_x * t_y != 0) * std.math.max(t_x, t_y);
    }

    test {
        const model = sample_models[0];
        try expectEqual(@as(u8, 0), timeRequiredTwo(
            model.ore_stat,
            model.obsidian_stat,
            model.geode_cost,
        ));

        try expectEqual(@as(u8, 15), timeRequiredTwo(
            .{ .amount = 85, .production = 16 },
            .{ .amount = 0, .production = 1 },
            model.obsidian_cost,
        ));
    }
};

const Resources = enum(u2) { ore, clay, obsidian, geode };

const Model = struct {
    time: u8 = 24,

    ore_stat: Stat = .{ .amount = 0, .production = 1 },
    clay_stat: Stat = .{},
    obsidian_stat: Stat = .{},
    geode_stat: Stat = .{},

    ore_cost: u8,
    clay_cost: u8,
    obsidian_cost: [2]u8,
    geode_cost: [2]u8,

    fn fromString(input_str: []const u8) Model {
        var c = "costs ";
        var a = "and ";

        var s = std.mem.indexOf(u8, input_str, c).? + c.len;
        var e = std.mem.indexOf(u8, input_str[s..], " ").?;
        //std.debug.print("{s}\n", .{input_str[s .. s + e]});
        const ore_cost = std.fmt.parseInt(u8, input_str[s .. s + e], 10) catch unreachable;

        s += std.mem.indexOf(u8, input_str[s..], c).? + c.len;
        e = std.mem.indexOf(u8, input_str[s..], " ").?;
        //std.debug.print("{s}\n", .{input_str[s .. s + e]});
        const clay_cost = std.fmt.parseInt(u8, input_str[s .. s + e], 10) catch unreachable;

        var obsidian_cost: [2]u8 = undefined;
        s += std.mem.indexOf(u8, input_str[s..], c).? + c.len;
        e = std.mem.indexOf(u8, input_str[s..], " ").?;
        //std.debug.print("{s}\n", .{input_str[s .. s + e]});
        obsidian_cost[0] = std.fmt.parseInt(u8, input_str[s .. s + e], 10) catch unreachable;
        s += std.mem.indexOf(u8, input_str[s..], a).? + a.len;
        e = std.mem.indexOf(u8, input_str[s..], " ").?;
        //std.debug.print("{s}\n", .{input_str[s .. s + e]});
        obsidian_cost[1] = std.fmt.parseInt(u8, input_str[s .. s + e], 10) catch unreachable;

        var geode_cost: [2]u8 = undefined;
        s += std.mem.indexOf(u8, input_str[s..], c).? + c.len;
        e = std.mem.indexOf(u8, input_str[s..], " ").?;
        //std.debug.print("{s}\n", .{input_str[s .. s + e]});
        geode_cost[0] = std.fmt.parseInt(u8, input_str[s .. s + e], 10) catch unreachable;
        s += std.mem.indexOf(u8, input_str[s..], a).? + a.len;
        e = std.mem.indexOf(u8, input_str[s..], " ").?;
        //std.debug.print("{s}\n", .{input_str[s .. s + e]});
        geode_cost[1] = std.fmt.parseInt(u8, input_str[s .. s + e], 10) catch unreachable;

        return .{
            .ore_cost = ore_cost,
            .clay_cost = clay_cost,
            .obsidian_cost = obsidian_cost,
            .geode_cost = geode_cost,
        };
    }

    test {
        //try expectEqual(sample_models[1], fromString(sample_input));
    }

    fn getQualityLevel(self: Model) u16 {
        if (self.time == 0) return self.geode_stat.amount;

        //std.debug.print("Time: {d:0>2}\n", .{self.time});
        //std.debug.print("Ore amount: {d}, production: {d}\n", .{
        //    self.ore_stat.amount,
        //    self.ore_stat.production,
        //});
        //std.debug.print("Clay amount: {d}, production: {d}\n", .{
        //    self.clay_stat.amount,
        //    self.clay_stat.production,
        //});
        //std.debug.print("Obsidian amount: {d}, production: {d}\n", .{
        //    self.obsidian_stat.amount,
        //    self.obsidian_stat.production,
        //});
        //std.debug.print("Geode amount: {d}, production: {d}\n", .{
        //    self.geode_stat.amount,
        //    self.geode_stat.production,
        //});

        var m: u16 = 0;

        var model = self;
        model.buildOreBot();
        m = std.math.max(m, model.getQualityLevel());

        model = self;
        model.buildClayBot();
        m = std.math.max(m, model.getQualityLevel());

        model = self;
        model.buildObsidianBot();
        m = std.math.max(m, model.getQualityLevel());

        model = self;
        model.buildGeodeBot();
        m = std.math.max(m, model.getQualityLevel());

        //std.debug.print("Geode: {d}\n", .{m});

        return m;
    }

    test {
        const model = Model{
            .time = 100,
            .ore_stat = .{ .amount = 0, .production = 0 },
            .geode_stat = .{ .amount = 0, .production = 1 },

            .ore_cost = 1,
            .clay_cost = 1,
            .obsidian_cost = .{ 1, 1 },
            .geode_cost = .{ 1, 1 },
        };

        try expectEqual(@as(u16, 100), model.getQualityLevel());
    }

    // Produces `elapsed` minutes worth of resources and reduces `time` by `elapsed` minutes
    // `elapsed` is treated as `self.time` when `0`
    fn passTime(self: *Model, elapsed: u8) void {
        const t = @boolToInt(elapsed == 0) * self.time + elapsed;

        self.ore_stat.produce(t);
        self.clay_stat.produce(t);
        self.obsidian_stat.produce(t);
        self.geode_stat.produce(t);

        self.time -= t;
    }

    test {
        var model = sample_models[0];
        model.buildClayBot();
        try expectEqual(@as(u8, 24 - 3), model.time);
        model.buildClayBot();
        try expectEqual(@as(u8, 24 - 5), model.time);
        model.buildClayBot();
        try expectEqual(@as(u8, 24 - 7), model.time);
        model.buildObsidianBot();
        try expectEqual(@as(u8, 24 - 11), model.time);
        model.buildClayBot();
        try expectEqual(@as(u8, 24 - 12), model.time);
        model.buildObsidianBot();
        try expectEqual(@as(u8, 24 - 15), model.time);
        model.buildGeodeBot();
        try expectEqual(@as(u8, 24 - 18), model.time);
        model.buildGeodeBot();
        try expectEqual(@as(u8, 24 - 21), model.time);
        try expectEqual(@as(u16, 3), model.geode_stat.amount);
        model.passTime(0);
        try expectEqual(@as(u16, 9), model.geode_stat.amount);

        //model = sample_models[0];
        //model.buildGeodeBot();
        //try expectEqual(@as(u16, 0), model.time);
        //try expectEqual(@as(u16, 0), model.geode_stat.amount);
    }

    fn buildOreBot(self: *Model) void {
        //std.debug.print("Ore: {d}\n", .{self.ore_stat.amount});
        var t = self.ore_stat.timeRequired(self.ore_cost);
        t *= @boolToInt(t < self.time);

        self.passTime(t);

        // If time became 0, don't bother
        self.ore_stat.amount -= @boolToInt(t != 0) * self.ore_cost;
        self.ore_stat.production += @boolToInt(t != 0);
    }

    fn buildClayBot(self: *Model) void {
        //std.debug.print("Clay: {d}\n", .{self.clay_stat.amount});
        var t = self.ore_stat.timeRequired(self.clay_cost);
        t *= @boolToInt(t < self.time);

        self.passTime(t);

        self.ore_stat.amount -= @boolToInt(t != 0) * self.clay_cost;
        self.clay_stat.production += @boolToInt(t != 0);
    }

    fn buildObsidianBot(self: *Model) void {
        var t = Stat.timeRequiredTwo(self.ore_stat, self.clay_stat, self.obsidian_cost);
        t *= @boolToInt(t < self.time);

        self.passTime(t);

        self.ore_stat.amount -= @boolToInt(t != 0) * self.obsidian_cost[0];
        self.clay_stat.amount -= @boolToInt(t != 0) * self.obsidian_cost[1];
        self.obsidian_stat.production += @boolToInt(t != 0);
    }

    fn buildGeodeBot(self: *Model) void {
        var t = Stat.timeRequiredTwo(self.ore_stat, self.obsidian_stat, self.geode_cost);
        t *= @boolToInt(t < self.time);

        self.passTime(t);

        self.ore_stat.amount -= @boolToInt(t != 0) * self.geode_cost[0];
        self.obsidian_stat.amount -= @boolToInt(t != 0) * self.geode_cost[1];
        self.geode_stat.production += @boolToInt(t != 0);
    }
};
