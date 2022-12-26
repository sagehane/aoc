const std = @import("std");

pub fn main() void {
    var gpa = std.heap.GeneralPurposeAllocator(.{}){};
    const allocator = gpa.allocator();

    var sample_model = Model.fromInput(allocator, sample_input) catch unreachable;
    sample_model.part2();

    var model = Model.fromInput(allocator, input) catch unreachable;
    model.part2();
}

test {
    std.testing.refAllDecls(@This());
    _ = Model;
}

const sample_input =
    \\1
    \\2
    \\-3
    \\3
    \\-2
    \\0
    \\4
;

const input = @embedFile("input");

const Elem = struct {
    ins: i64,
    idx: u16,
};

const Model = struct {
    allocator: std.mem.Allocator,
    array_list: std.MultiArrayList(Elem),
    z_index: u16,

    fn fromInput(allocator: std.mem.Allocator, input_str: []const u8) !Model {
        var array_list = std.MultiArrayList(Elem){};
        try array_list.ensureTotalCapacity(allocator, 5000);

        var z_index: u16 = 0;

        var idx: u16 = 0;
        var iter = std.mem.tokenize(u8, input_str, "\n");
        while (iter.next()) |line| : (idx += 1) {
            const val = 811589153 * (std.fmt.parseInt(i64, line, 10) catch unreachable);

            z_index += @boolToInt(val == 0) * idx;
            array_list.appendAssumeCapacity(.{ .ins = val, .idx = idx });
        }

        return .{ .allocator = allocator, .array_list = array_list, .z_index = z_index };
    }

    fn part2(self: *Model) void {
        var x: u4 = 0;
        while (x < 10) : (x += 1) {
            self.mix();
        }

        var t1: u16 = 0;
        var t2: u16 = 0;
        var t3: u16 = 0;

        const idxs = self.array_list.items(.idx);
        const inss = self.array_list.items(.ins);

        const len = @intCast(u15, self.array_list.len);
        const z_idx = idxs[self.z_index];

        for (idxs) |elem_idx, i| {
            const id = @intCast(u15, i);

            t1 += @boolToInt(elem_idx == (1000 + z_idx) % len) * id;
            t2 += @boolToInt(elem_idx == (2000 + z_idx) % len) * id;
            t3 += @boolToInt(elem_idx == (3000 + z_idx) % len) * id;
        }

        std.debug.print("t1: {d}\n", .{inss[t1]});
        std.debug.print("t2: {d}\n", .{inss[t2]});
        std.debug.print("t3: {d}\n", .{inss[t3]});
        std.debug.print("Sum: {d}\n", .{inss[t1] + inss[t2] + inss[t3]});
    }

    fn deinit(self: *Model) void {
        self.array_list.deinit(self.allocator);
    }

    fn applyIns(self: *Model, id: u15) void {
        const elem = self.array_list.get(id);
        const ins = elem.ins;
        const idx = @intCast(u15, elem.idx);
        const len = @intCast(u15, self.array_list.len);

        const offset = @mod(ins, len - 1);

        var items = self.array_list.items(.idx);
        for (items) |*elem_idx| {
            const e_idx = @intCast(i16, elem_idx.*);
            const span = @boolToInt(@mod(e_idx - idx, len) <= offset);

            elem_idx.* = @intCast(u15, @mod(e_idx - span, len));
        }

        items[id] = @intCast(u15, @mod(idx + offset, len));
    }

    fn mix(self: *Model) void {
        var id: u15 = 0;
        while (id < self.array_list.len) : (id += 1) {
            self.applyIns(id);
        }
    }
};
