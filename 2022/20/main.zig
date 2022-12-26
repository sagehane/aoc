const std = @import("std");

pub fn main() void {
    var gpa = std.heap.GeneralPurposeAllocator(.{}){};
    const allocator = gpa.allocator();

    var sample_model = Model.fromInput(allocator, sample_input) catch unreachable;
    sample_model.part1();

    var model = Model.fromInput(allocator, input) catch unreachable;
    model.part1();
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

const Elem = packed struct(u32) {
    ins: i16,
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
            const val = std.fmt.parseInt(i16, line, 10) catch unreachable;

            z_index += @boolToInt(val == 0) * idx;
            array_list.appendAssumeCapacity(.{ .ins = val, .idx = idx });
        }

        return .{ .allocator = allocator, .array_list = array_list, .z_index = z_index };
    }

    fn part1(self: *Model) void {
        self.mix();

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
        std.debug.print("Product: {d}\n", .{inss[t1] + inss[t2] + inss[t3]});
    }

    fn deinit(self: *Model) void {
        self.array_list.deinit(self.allocator);
    }

    fn applyIns(self: *Model, id: u15) void {
        const elem = self.array_list.get(id);
        const ins = elem.ins;
        const idx = @intCast(u15, elem.idx);
        const len = @intCast(u15, self.array_list.len);

        //const quot = @divTrunc(ins, len);
        //const rem = @rem(ins, len);

        //const div = @intCast(u15, std.math.absCast(@divTrunc(ins, len)));
        // TODO: Fix this
        // if idx + ins < 0 or idx + ins < len: ins - @boolToInt(ins < 0) % len
        // if idx + ins >= len or idx + ins > idx: len - (idx + ins)
        // (idx + ins) % len < idx: ins - @boolToInt(ins < 0) % len
        // (idx + ins) % len > idx:
        // len 7, 0 + 1 -> +1
        // len 7, 0 + 2 -> +2
        // len 7, 1 - 3 -> +3
        // len 7, 2 - 2 -> +4
        // len 7, 5 + 4 -> -2
        // len 7, 6 - 2 -> -2?
        const offset =
            // Move normally to the left or right?
            // idk if the logic for moving to the left is correct here
            @boolToInt(idx + ins > 0 and idx + ins <= len - 1) * @rem(ins, len) +
            // Move to the right with the value subtracted by 1 for some reason
            @boolToInt(idx + ins <= 0) * @mod(ins - 1, len) +
            // Move to the left
            @boolToInt(idx + ins > len - 1) * -@mod(idx + ins, len);

        //const offset = @intCast(i16, @mod(ins - @boolToInt(ins < 0), len));

        var items = self.array_list.items(.idx);
        for (items) |*elem_idx| {
            const e_idx = @intCast(i16, elem_idx.*);

            //const span = @boolToInt(@mod(e_idx - idx, len) <= rem) -
            //    @as(i2, @boolToInt(@mod(idx - e_idx, len) <= -rem));
            //elem_idx.* = @intCast(u15, @mod(e_idx - (quot + span), len));

            const span = @boolToInt(@mod(e_idx - idx, len) <= offset) -
                @as(i2, @boolToInt(@mod(idx - e_idx, len) <= -offset));

            //std.debug.print("ins: {d}, idx: {d}, len: {d}, idx': {d}, mod: {d}, span: {}\n", .{
            //    ins,
            //    idx,
            //    len,
            //    e_idx,
            //    mod,
            //    span,
            //});

            //elem_idx.* = @intCast(u15, @mod(e_idx - (div + span), len));
            elem_idx.* = @intCast(u15, @mod(e_idx - span, len));

            //std.debug.print("offset: {d}, idx': {d}->{d}\n", .{
            //    offset,
            //    e_idx,
            //    elem_idx.*,
            //});
        }

        //items[id] = @intCast(u15, @mod(@as(i16, idx) + ins, len));
        items[id] = @intCast(u15, @mod(idx + offset, len));
    }

    test {
        var allocator = std.testing.allocator;

        var model = try Model.fromInput(allocator, sample_input);
        defer model.deinit();

        var items = model.array_list.items(.idx);

        model.applyIns(0);
        try std.testing.expectEqual(items[0], 1);
        try std.testing.expectEqual(items[1], 0);
        try std.testing.expectEqual(items[2], 2);
        try std.testing.expectEqual(items[3], 3);
        try std.testing.expectEqual(items[4], 4);
        try std.testing.expectEqual(items[5], 5);
        try std.testing.expectEqual(items[6], 6);

        model.applyIns(1);
        try std.testing.expectEqual(items[0], 0);
        try std.testing.expectEqual(items[1], 2);
        try std.testing.expectEqual(items[2], 1);
        try std.testing.expectEqual(items[3], 3);
        try std.testing.expectEqual(items[4], 4);
        try std.testing.expectEqual(items[5], 5);
        try std.testing.expectEqual(items[6], 6);

        model.applyIns(2);
        try std.testing.expectEqual(items[0], 0);
        try std.testing.expectEqual(items[1], 1);
        try std.testing.expectEqual(items[2], 4);
        try std.testing.expectEqual(items[3], 2);
        try std.testing.expectEqual(items[4], 3);
        try std.testing.expectEqual(items[5], 5);
        try std.testing.expectEqual(items[6], 6);

        model.applyIns(3);
        try std.testing.expectEqual(items[0], 0);
        try std.testing.expectEqual(items[1], 1);
        try std.testing.expectEqual(items[2], 3);
        try std.testing.expectEqual(items[3], 5);
        try std.testing.expectEqual(items[4], 2);
        try std.testing.expectEqual(items[5], 4);
        try std.testing.expectEqual(items[6], 6);

        model.applyIns(4);
        try std.testing.expectEqual(items[0], 0);
        try std.testing.expectEqual(items[1], 1);
        try std.testing.expectEqual(items[2], 2);
        try std.testing.expectEqual(items[3], 4);
        try std.testing.expectEqual(items[4], 6);
        try std.testing.expectEqual(items[5], 3);
        try std.testing.expectEqual(items[6], 5);

        model.applyIns(5);
        try std.testing.expectEqual(items[0], 0);
        try std.testing.expectEqual(items[1], 1);
        try std.testing.expectEqual(items[2], 2);
        try std.testing.expectEqual(items[3], 4);
        try std.testing.expectEqual(items[4], 6);
        try std.testing.expectEqual(items[5], 3);
        try std.testing.expectEqual(items[6], 5);

        model.applyIns(6);
        try std.testing.expectEqual(items[0], 0);
        try std.testing.expectEqual(items[1], 1);
        try std.testing.expectEqual(items[2], 2);
        try std.testing.expectEqual(items[3], 5);
        try std.testing.expectEqual(items[4], 6);
        try std.testing.expectEqual(items[5], 4);
        try std.testing.expectEqual(items[6], 3);
    }

    fn mix(self: *Model) void {
        var id: u15 = 0;
        while (id < self.array_list.len) : (id += 1) {
            self.applyIns(id);
        }
    }

    test {
        var allocator = std.testing.allocator;

        var model = try Model.fromInput(allocator, sample_input);
        defer model.deinit();

        var items = model.array_list.items(.idx);

        model.mix();
        try std.testing.expectEqual(items[0], 0);
        try std.testing.expectEqual(items[1], 1);
        try std.testing.expectEqual(items[2], 2);
        try std.testing.expectEqual(items[3], 5);
        try std.testing.expectEqual(items[4], 6);
        try std.testing.expectEqual(items[5], 4);
        try std.testing.expectEqual(items[6], 3);
    }

    fn getGrooveCoords(self: Model) [3]i16 {
        var grooves: [3]u16 = undefined;

        grooves[0] = (1000 + self.array[self.z_index].idx) % self.array.len;
        grooves[1] = (2000 + self.array[self.z_index].idx) % self.array.len;
        grooves[2] = (3000 + self.array[self.z_index].idx) % self.array.len;

        return grooves;
    }
};
