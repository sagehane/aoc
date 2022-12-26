const std = @import("std");

pub fn main() void {
    var gpa = std.heap.GeneralPurposeAllocator(.{}){};
    var allocator = gpa.allocator();
    part2(allocator, sample_input) catch unreachable;
    part2(allocator, input_str) catch unreachable;
}

fn part1(allocator: std.mem.Allocator, input: []const u8) void {
    var model = Model.fromInput(allocator, input) catch unreachable;
    std.debug.print("{d}\n", .{model.solveMonkey(.{ 'r', 'o', 'o', 't' })});
}

fn part2(allocator: std.mem.Allocator, input: []const u8) !void {
    const model = Model.fromInput(allocator, input) catch unreachable;
    var array_list = std.ArrayList(History).init(allocator);

    const root = model.hash_map.get(.{ 'r', 'o', 'o', 't' }).?;

    if (try model.findHuman(&array_list, root.operation.m1)) {
        var other = model.solveMonkey(root.operation.m2);
        std.debug.print("{d}\n", .{Model.calculateHuman(&array_list, other)});

        return;
    }

    _ = try model.findHuman(&array_list, root.operation.m2);
    var other = model.solveMonkey(root.operation.m1);
    std.debug.print("{d}\n", .{Model.calculateHuman(&array_list, other)});
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

const Key = [4]u8;

const Operator = enum(u2) { add, sub, mul, div };

const Operation = struct {
    m1: Key,
    m2: Key,
    operator: Operator,
};

const NumType = u64;

const History = struct { arg: NumType, side: enum(u1) { left, right }, operator: Operator };

const Model = struct {
    const Value = union(enum) {
        number: NumType,
        operation: Operation,
    };

    const HashMap = std.AutoHashMap(Key, Value);

    hash_map: HashMap,

    fn calculateHuman(array_list: *std.ArrayList(History), other: NumType) NumType {
        var tmp = other;
        while (array_list.popOrNull()) |history| {
            std.debug.print("{any}:{d}\n", .{ history, tmp });
            switch (history.operator) {
                .add => tmp -= history.arg,
                .sub => switch (history.side) {
                    .left => tmp += history.arg,
                    .right => tmp = history.arg - tmp,
                },
                .mul => tmp = @divExact(tmp, history.arg),
                .div => switch (history.side) {
                    .left => tmp *= history.arg,
                    .right => tmp = @divExact(history.arg, tmp),
                },
            }
        }

        return tmp;
    }

    fn findHuman(self: Model, array_list: *std.ArrayList(History), key: Key) !bool {
        if (std.mem.eql(u8, &key, &[4]u8{ 'h', 'u', 'm', 'n' })) return true;

        switch (self.hash_map.get(key).?) {
            .number => {},
            .operation => |operation| {
                if (try self.findHuman(array_list, operation.m1)) {
                    try array_list.append(.{
                        .arg = self.solveMonkey(operation.m2),
                        .side = .left,
                        .operator = operation.operator,
                    });

                    return true;
                }

                if (try self.findHuman(array_list, operation.m2)) {
                    try array_list.append(.{
                        .arg = self.solveMonkey(operation.m1),
                        .side = .right,
                        .operator = operation.operator,
                    });

                    return true;
                }
            },
        }
        return false;
    }

    fn solveMonkey(self: Model, key: Key) NumType {
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

        var array: Key = undefined;
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
