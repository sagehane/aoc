const std = @import("std");

pub fn main() void {
    var gpa = std.heap.GeneralPurposeAllocator(.{}){};
    var allocator = gpa.allocator();
    part1(allocator, sample_input);
    part1(allocator, input_str);
}

fn part1(allocator: std.mem.Allocator, input: []const u8) void {
    var model = Model.fromInput(allocator, input) catch unreachable;
    std.debug.print("{d}\n", .{model.solveMonkey(.{ 'r', 'o', 'o', 't' })});
}

const input_str = @embedFile("input");

const sample_input =
    \\root: pppw + sjmn
    \\dbpl: 5
    \\cczh: sllz + lgvd
    \\zczc: 2
    \\ptdq: humn - dvpt
    \\dvpt: 3
    \\lfqf: 4
    \\humn: 5
    \\ljgn: 2
    \\sjmn: drzm * dbpl
    \\sllz: 4
    \\pppw: cczh / lfqf
    \\lgvd: ljgn * ptdq
    \\drzm: hmdt - zczc
    \\hmdt: 32
;

const Operator = enum(u2) { add, sub, mul, div };

const Operation = struct {
    m1: [4]u8,
    m2: [4]u8,
    operator: Operator,
};

const Model = struct {
    const Value = union(enum) {
        number: u64,
        operation: Operation,
    };

    const HashMap = std.AutoHashMap([4]u8, Value);

    hash_map: HashMap,

    fn solveMonkey(self: Model, key: [4]u8) u64 {
        return switch (self.hash_map.get(key).?) {
            .number => |number| number,
            .operation => |operation| switch (operation.operator) {
                .add => self.solveMonkey(operation.m1) + self.solveMonkey(operation.m2),
                .sub => self.solveMonkey(operation.m1) - self.solveMonkey(operation.m2),
                .mul => self.solveMonkey(operation.m1) * self.solveMonkey(operation.m2),
                .div => @divExact(self.solveMonkey(operation.m1), self.solveMonkey(operation.m2)),
            },
        };
    }

    fn fromInput(allocator: std.mem.Allocator, input: []const u8) !Model {
        var model = Model{ .hash_map = HashMap.init(allocator) };

        var iter = std.mem.tokenize(u8, input, "\n");
        while (iter.next()) |line| {
            try model.parseLine(line);
        }

        return model;
    }

    fn deinit(self: *Model) void {
        self.hash_map.deinit();
    }

    fn parseLine(self: *Model, line: []const u8) !void {
        var needle = ": ";
        var idx: usize = 4;

        var array: [4]u8 = undefined;
        std.mem.copy(u8, &array, line[0..4]);

        const key = array;
        idx += needle.len;

        const value: Value = (if (std.mem.indexOf(u8, line[idx..], " ") == null) .{ .number = std.fmt.parseInt(u32, line[idx..], 10) catch unreachable } else blk: {
            std.mem.copy(u8, &array, line[idx..][0..4]);
            const m1 = array;
            idx += 5;

            const operator: Operator = switch (line[idx]) {
                '+' => .add,
                '-' => .sub,
                '*' => .mul,
                '/' => .div,
                else => unreachable,
            };
            idx += 2;

            std.mem.copy(u8, &array, line[idx..][0..4]);
            const m2 = array;

            break :blk .{ .operation = .{
                .m1 = m1,
                .m2 = m2,
                .operator = operator,
            } };
        });

        try self.hash_map.put(key, value);
    }
};
