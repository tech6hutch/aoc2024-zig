const std = @import("std");

pub fn main() !void {
    var general_purpose_allocator = std.heap.GeneralPurposeAllocator(.{}){};
    const ally = general_purpose_allocator.allocator();

    var input_file = try std.fs.cwd().openFile("./inputs/day1", .{});
    defer input_file.close();

    var buf_reader = std.io.bufferedReader(input_file.reader());
    var input = buf_reader.reader();
    var buf: [256]u8 = undefined;

    var first_list = try std.ArrayListUnmanaged(i32).initCapacity(ally, 0);
    var second_list = try std.ArrayListUnmanaged(i32).initCapacity(ally, 0);
    while (try input.readUntilDelimiterOrEof(&buf, '\n')) |line| {
        var first_end: usize = 0;
        while (first_end < line.len and std.ascii.isDigit(line[first_end])) first_end += 1;
        var second_start: usize = first_end;
        while (second_start < line.len and std.ascii.isWhitespace(line[second_start])) second_start += 1;
        const first = try std.fmt.parseInt(i32, line[0..first_end], 10);
        const second = try std.fmt.parseInt(i32, line[second_start..], 10);
        try first_list.append(ally, first);
        try second_list.append(ally, second);
    }
    
    std.mem.sort(i32, first_list.items, {}, std.sort.asc(i32));
    std.mem.sort(i32, second_list.items, {}, std.sort.asc(i32));

    std.debug.assert(first_list.items.len == second_list.items.len);
    var total_dist: u64 = 0;
    for (first_list.items, second_list.items) |first, second| {
        total_dist += @abs(first - second);
    }

    var similarity: u64 = 0;
    for (first_list.items) |first| {
        for (second_list.items) |second| {
            if (first == second) {
                similarity += @intCast(first);
            }
        }
    }

    var stdout = std.io.getStdOut().writer();
    try stdout.print("Day 1\n", .{});
    try stdout.print("part 1: {d} ({s}correct)\n", .{total_dist, if (total_dist == 1530215) "" else "in"});
    try stdout.print("part 2: {d} ({s}correct)\n", .{similarity, if (similarity == 26800609) "" else "in"});
}
