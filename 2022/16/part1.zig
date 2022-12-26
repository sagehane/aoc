const std = @import("std");
const mem = std.mem;
const readIntSlice = std.mem.readIntSlice;

pub fn main() !void {
    var gpa = std.heap.GeneralPurposeAllocator(.{}){};
    var allocator = gpa.allocator();

    var sample_model = try Model.parseInput(sample_input, allocator);
    std.debug.print("{d}\n", .{try sample_model.getMaxFlow(allocator)});

    var model = try Model.parseInput(input_str, allocator);
    std.debug.print("{d}\n", .{try model.getMaxFlow(allocator)});
}

const input_str = @embedFile("input");

const sample_input =
    \\Valve AA has flow rate=0; tunnels lead to valves DD, II, BB
    \\Valve BB has flow rate=13; tunnels lead to valves CC, AA
    \\Valve CC has flow rate=2; tunnels lead to valves DD, BB
    \\Valve DD has flow rate=20; tunnels lead to valves CC, AA, EE
    \\Valve EE has flow rate=3; tunnels lead to valves FF, DD
    \\Valve FF has flow rate=0; tunnels lead to valves EE, GG
    \\Valve GG has flow rate=0; tunnels lead to valves FF, HH
    \\Valve HH has flow rate=22; tunnel leads to valve GG
    \\Valve II has flow rate=0; tunnels lead to valves AA, JJ
    \\Valve JJ has flow rate=21; tunnel leads to valve II
;

const Valve = struct {
    flow_rate: u8,
    len: u8,
    leads_to: [5]u16,
};

const Model = struct {
    const HashMap = std.AutoHashMap(u16, Valve);
    const Distances = std.ArrayList(u8);

    const init_id = std.mem.readIntSliceNative(u16, "AA");

    hash_map: HashMap,
    key_valves: std.ArrayList(u16),

    fn getMaxFlow(self: Model, allocator: std.mem.Allocator) !u16 {
        const len = @intCast(u8, self.key_valves.items.len);

        var distances = Distances.init(allocator);
        defer distances.deinit();

        for (self.key_valves.items) |item| {
            std.debug.print("Calculating route for {s}\n", .{
                std.mem.asBytes(&item),
            });
            try self.breadthFirstSearch(item, allocator, &distances);
        }
        try self.breadthFirstSearch(init_id, allocator, &distances);

        std.debug.print("Length of Distances: {d}\n", .{distances.items.len});

        const State = packed struct(u32) {
            time: u8 = 30,
            id: u8,
            mask: u16 = 0,
        };

        var total_flow: u16 = 0;

        var frontier = std.fifo.LinearFifo(State, .Dynamic).init(allocator);
        defer frontier.deinit();

        try frontier.writeItem(State{ .id = len });

        var reached = std.AutoHashMap(State, u16).init(allocator);
        defer reached.deinit();

        try reached.put(State{ .id = len }, 0);

        while (frontier.readItem()) |state| {

            // Enumerate over unreached valves
            var id: u4 = 0;
            while (id < len) : (id += 1) {
                const mask = @as(u16, 1) << id;

                if (state.mask & mask == 0) {
                    var distance = distances.items[@as(u16, len) * state.id + id];
                    if (distance < state.time) {
                        var next_state = state;
                        next_state.id = id;
                        next_state.mask |= mask;
                        next_state.time -= distance + 1;

                        const flow_rate = self.hash_map.get(self.key_valves.items[id]).?.flow_rate;
                        var flow: u16 = reached.get(state).?;
                        flow += @as(u16, next_state.time) * flow_rate;

                        //std.debug.print("{b:0>6}&{b:0>6}\n", .{
                        //    state.mask,
                        //    @as(u16, 1) << id,
                        //});

                        //if (state.id != len) {
                        //    std.debug.print("{d}:{s}->{s}:{d}\n", .{
                        //        distance,
                        //        std.mem.asBytes(&self.key_valves.items[state.id]),
                        //        std.mem.asBytes(&self.key_valves.items[id]),
                        //        flow,
                        //    });
                        //}

                        const opt_flow = reached.get(next_state);
                        if (opt_flow == null or opt_flow.? < flow) {
                            total_flow = std.math.max(total_flow, flow);
                            try frontier.writeItem(next_state);
                            try reached.put(next_state, flow);
                        }
                    }
                }
            }
        }

        return total_flow;
    }

    /// Gets the distance between the start and all points with a flow rate
    fn breadthFirstSearch(
        self: Model,
        id: u16,
        allocator: std.mem.Allocator,
        distances: *Distances,
    ) !void {
        var frontier = std.fifo.LinearFifo(u16, .Dynamic).init(allocator);
        defer frontier.deinit();

        try frontier.writeItem(id);

        // Consider storing the steps taken
        var reached = std.AutoHashMap(u16, u8).init(allocator);
        defer reached.deinit();

        try reached.put(id, 0);

        while (frontier.readItem()) |item_id| {
            const item = self.hash_map.get(item_id).?;
            const distance = reached.get(item_id).?;

            for (item.leads_to[0..item.len]) |next| {
                if (!reached.contains(next)) {
                    try frontier.writeItem(next);
                    try reached.put(next, distance + 1);
                }
            }
        }

        for (self.key_valves.items) |item| {
            try distances.append(reached.get(item).?);

            //std.debug.print("{s} -> {s} takes {} step(s)\n", .{
            //    std.mem.asBytes(&id),
            //    std.mem.asBytes(&item),
            //    reached.get(item).?,
            //});
        }
    }

    fn parseInput(input: []const u8, allocator: std.mem.Allocator) !Model {
        var hash_map = HashMap.init(allocator);
        var key_valves = std.ArrayList(u16).init(allocator);

        var iter = mem.tokenize(u8, input, "\n");
        while (iter.next()) |line| {
            var idx = "Valve  ".len - 1;
            var end = idx + 2;
            const id = std.mem.readIntSliceNative(u16, line[idx..]);
            //std.debug.print("{s}\n", .{line[idx..end]});

            var valve: Valve = undefined;

            idx = end + " has flow rate =".len - 1;
            end = idx + std.mem.indexOf(u8, line[idx..], ";").?;
            valve.flow_rate = std.fmt.parseInt(u8, line[idx..end], 10) catch unreachable;

            if (valve.flow_rate != 0)
                try key_valves.append(id);

            idx = end + "; tunnels lead to valve".len - 1;
            idx += std.mem.indexOf(u8, line[idx..], " ").? + 1;
            //std.debug.print("{s}\n", .{line[idx..]});

            valve.len = 0;
            var lead_iter = std.mem.tokenize(u8, line[idx..], ", ");
            while (lead_iter.next()) |lead| : (valve.len += 1) {
                const lead_id = std.mem.readIntSliceNative(u16, lead);
                valve.leads_to[valve.len] = lead_id;
            }

            try hash_map.put(id, valve);
        }

        return .{ .hash_map = hash_map, .key_valves = key_valves };
    }
};
