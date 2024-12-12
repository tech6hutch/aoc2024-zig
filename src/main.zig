const std = @import("std");

var general_purpose_allocator = std.heap.GeneralPurposeAllocator(.{}){};
const ally = general_purpose_allocator.allocator();

const DayInfo = struct {
    f: union(enum) {
        one: fn (std.fs.File) anyerror!struct { i64, i64 },
        two: fn (std.fs.File, std.fs.File) anyerror!struct { i64, i64 },
    },
    answers: struct { i64, i64 = 0 } = .{ -1, -1 },
    test_answers: struct { i64, i64 } = .{ -1, -1 },
};

const days = .{
    DayInfo{
        .f = .{ .one = day1 },
        .answers = .{ 1530215, 26800609 },
    },
    DayInfo{
        .f = .{ .one = day2 },
        .answers = .{ 314, 373 },
        .test_answers = .{ 2, 4 },
    },
    DayInfo{
        // .f = .{.two = day3},
        .f = .{ .one = day3 },
        .answers = .{ 173529487, 99532691 },
        .test_answers = .{ 161, 48 },
    },
    DayInfo{
        .f = .{ .one = day4 },
        .answers = .{ 2567, 2029 },
        .test_answers = .{ 18, 9 },
    },
    DayInfo{
        .f = .{ .one = day5 },
        .answers = .{7307, 4713},
        .test_answers = .{143, 123},
    },
    DayInfo{
        .f = .{ .one = day6 },
        .answers = .{5212, 1767},
        .test_answers = .{41, 6},
    },
    DayInfo{
        .f = .{ .one = day7 },
        .answers = .{3119088655389, 264184041398847},
        .test_answers = .{3749, 11387},
    },
    DayInfo{
        .f = .{ .one = day8 },
        .answers = .{371, 1229},
        .test_answers = .{14, 34},
    },
    DayInfo{
        .f = .{ .one = day9 },
        .answers = .{6331212425418, 6363268339304},
        .test_answers = .{1928, 2858},
    },
    DayInfo{
        .f = .{ .one = day10 },
        .answers = .{811, 1794},
        .test_answers = .{36, 81},
    },
    DayInfo{
        .f = .{ .one = day11 },
        .answers = .{216996, 257335372288947},
        .test_answers = .{55312, -1},
    },
    DayInfo{
        .f = .{ .one = Day12.day12 },
        .answers = .{1433460, -1},
        .test_answers = .{1930, 1206},
    },
};

pub fn main() !void {
    try do_solutions(false);
}

test "solutions against test inputs" {
    try do_solutions(true);
}

const SolutionInputs = enum {
    Normal,
    Example,
};
fn do_solutions(comptime use_example_inputs: bool) !void {
    var stdout =
        if (use_example_inputs) std.io.getStdErr().writer() else std.io.getStdOut().writer();

    // inline for (1..days.len + 1) |day| {
    {
        const day = days.len;
        // You can't use continue in an inline loop, for some reason.
        inline_continue: {
            const input_file = std.fs.cwd().openFile(std.fmt.comptimePrint("./{s}/day{d}", .{ if (use_example_inputs) "examples" else "inputs", day }), .{}) catch null;
            const input_file_part1 = std.fs.cwd().openFile(std.fmt.comptimePrint("./{s}/day{d}pt1", .{ if (use_example_inputs) "examples" else "inputs", day }), .{}) catch null;
            const input_file_part2 = std.fs.cwd().openFile(std.fmt.comptimePrint("./{s}/day{d}pt2", .{ if (use_example_inputs) "examples" else "inputs", day }), .{}) catch null;
            defer {
                if (input_file) |file| file.close();
                if (input_file_part1) |file| file.close();
                if (input_file_part2) |file| file.close();
            }

            const info = days[day - 1];
            if (
                (input_file == null and info.f == .one) or
                ((input_file_part1 == null or input_file_part2 == null) and info.f == .two)
            ) {
                try stdout.print("Skipping day {d}\n", .{day});
                break :inline_continue;
            }

            const answers = switch (info.f) {
                .one => |f| try f(input_file.?),
                .two => |f| try f(input_file_part1.?, input_file_part2.?),
            };

            try stdout.print("Day {d}\n", .{day});
            inline for (0..info.answers.len) |i| {
                const given = answers[i];
                const correct = if (use_example_inputs) info.test_answers[i] else info.answers[i];
                try stdout.print("part {d}: {d}", .{ i + 1, given });
                if (correct != -1) {
                    try stdout.print(" ({s})", .{if (given == correct) "correct" else "incorrect"});
                }
                try stdout.print("\n", .{});
            }
        }
    }
}

const Answers = struct {i64, i64};

fn day1(input_file: std.fs.File) !Answers {
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
    var total_dist: i64 = 0;
    for (first_list.items, second_list.items) |first, second| {
        total_dist += @abs(first - second);
    }

    var similarity: i64 = 0;
    for (first_list.items) |first| {
        for (second_list.items) |second| {
            if (first == second) {
                similarity += @intCast(first);
            }
        }
    }

    return .{ total_dist, similarity };
}

fn day2(input_file: std.fs.File) !Answers {
    var buf_reader = std.io.bufferedReader(input_file.reader());
    var input = buf_reader.reader();
    var buf: [256]u8 = undefined;
    var part1_safe: i64 = 0;
    var part2_safe: i64 = 0;
    var nums = try std.ArrayList(i32).initCapacity(ally, 8);
    while (try input.readUntilDelimiterOrEof(&buf, '\n')) |line| {
        nums.clearRetainingCapacity();
        var it = std.mem.splitScalar(u8, line, ' ');
        while (it.next()) |str| {
            const s = std.mem.trim(u8, str, "\r\n");
            if (s.len > 0) {
                try nums.append(try std.fmt.parseInt(i32, s, 10));
            }
        }
        if (nums.items.len == 0) break;

        if (report_is_safe(nums.items)) {
            part1_safe += 1;
            part2_safe += 1;
            continue;
        }

        for (0..nums.items.len) |i| {
            var nums2 = try nums.clone();
            _ = nums2.orderedRemove(i);
            if (report_is_safe(nums2.items)) {
                part2_safe += 1;
                break;
            }
        }
    }
    return .{ part1_safe, part2_safe };
}

fn report_is_safe(reports: []i32) bool {
    if (reports.len < 2) {
        return reports.len == 1;
    }
    const first = reports[0];
    const second = reports[1];
    const going_up: bool =
        if (first < second) true else if (first > second) false else return false;
    var prev = first;
    for (1..reports.len) |i| {
        const n = reports[i];
        if (prev == n or going_up != (prev < n) or @abs(prev - n) > 3) {
            return false;
        }
        prev = n;
    }
    return true;
}

fn day3(input_file: std.fs.File) !Answers {
    // var buf_reader = std.io.bufferedReader(input_file.reader());
    // var input = buf_reader.reader();
    // var buf: [256]u8 = undefined;
    // var mul_start: usize = 0;
    // input.skipUntilDelimiterOrEof("mul(");
    const input = try input_file.readToEndAlloc(ally, 10_000_000);
    const MULSTR = "mul(";
    const DOSTR = "do()";
    const DONTSTR = "don't()";

    var all_sum: i64 = 0;
    var enabled_sum: i64 = 0;
    var enabled = true;
    var mul_start: usize = 0;
    each_mul_loop: while (mul_start < input.len) {
        while (true) {
            if (mul_start >= input.len) {
                break :each_mul_loop;
            }

            if (std.mem.startsWith(u8, input[mul_start..], MULSTR)) {
                mul_start += MULSTR.len;
                break;
            }
            if (std.mem.startsWith(u8, input[mul_start..], DOSTR)) {
                mul_start += DOSTR.len;
                enabled = true;
                continue :each_mul_loop;
            }
            if (std.mem.startsWith(u8, input[mul_start..], DONTSTR)) {
                mul_start += DONTSTR.len;
                enabled = false;
                continue :each_mul_loop;
            }

            mul_start += 1;
        }

        var int_end = mul_start;
        while (std.ascii.isDigit(input[int_end])) int_end += 1;
        const int1 = parseI32(input[mul_start..int_end]) catch continue :each_mul_loop;
        mul_start = int_end;
        if (input[mul_start] != ',') continue :each_mul_loop;
        mul_start += 1;
        int_end = mul_start;
        while (std.ascii.isDigit(input[int_end])) int_end += 1;
        const int2 = parseI32(input[mul_start..int_end]) catch continue :each_mul_loop;
        mul_start = int_end;
        if (input[mul_start] != ')') continue :each_mul_loop;

        all_sum += int1 * int2;
        if (enabled) {
            enabled_sum += int1 * int2;
        }
    }

    return .{ all_sum, enabled_sum };
}

fn day4(input_file: std.fs.File) !Answers {
    var lines = std.ArrayList([]const u8).init(ally);
    var lines_iter = std.mem.splitScalar(u8, try input_file.readToEndAlloc(ally, 10_000_000), '\n');
    while (lines_iter.next()) |line| {
        try lines.append(line);
    }

    // Part 1
    var pt1_count: i64 = 0;
    {
        const XMAS = "XMAS";
        const directions = [_]struct{isize, isize}{
            .{-1, -1}, .{0, -1}, .{1, -1},
            .{-1,  0},           .{1,  0},
            .{-1,  1}, .{0,  1}, .{1,  1},
        };

        var i: isize = -1;
        for (lines.items) |line| {
            i += 1;
            var j: isize = -1;
            for (line) |char| {
                j += 1;
                if (char != XMAS[0]) continue;
                for (directions) |dir| {
                    var ii = i;
                    var jj = j;
                    for (XMAS) |xmas_char| {
                        if (
                            ii < 0 or ii >= lines.items.len or
                            jj < 0 or jj >= lines.items[@intCast(ii)].len
                        ) break;
                        if (lines.items[@intCast(ii)][@intCast(jj)] != xmas_char) break;
                        ii += dir[0];
                        jj += dir[1];
                    } else {
                        pt1_count += 1;
                    }
                }
            }
        }
    }

    // Part 2
    var pt2_count: i64 = 0;
    {
        var i: isize = -1;
        for (lines.items) |line| {
            i += 1;
            var j: isize = -1;
            for (line) |char| {
                j += 1;
                if (char != 'A') continue;
                switch (get2d(u8, lines.items, i - 1, j - 1, ' ')) {
                    'M' => switch (get2d(u8, lines.items, i + 1, j + 1, ' ')) {
                        'S' => {},
                        else => continue
                    },
                    'S' => switch (get2d(u8, lines.items, i + 1, j + 1, ' ')) {
                        'M' => {},
                        else => continue
                    },
                    else => continue
                }
                switch (get2d(u8, lines.items, i + 1, j - 1, ' ')) {
                    'M' => switch (get2d(u8, lines.items, i - 1, j + 1, ' ')) {
                        'S' => {},
                        else => continue
                    },
                    'S' => switch (get2d(u8, lines.items, i - 1, j + 1, ' ')) {
                        'M' => {},
                        else => continue
                    },
                    else => continue
                }
                pt2_count += 1;
            }
        }
    }

    return .{pt1_count, pt2_count};
}

const Day5Rule = struct {
    before: []const u8,
    after: []const u8,
};
fn day5(input_file: std.fs.File) !Answers {
    const input = try input_file.readToEndAlloc(ally, 10_000_000);
    var lines_iter = std.mem.tokenizeAny(u8, input, "\r\n");

    const Rule = Day5Rule;
    var rules = std.ArrayList(Rule).init(ally);
    while (lines_iter.peek()) |line| {
        if (std.mem.indexOfScalar(u8, line, '|') == null) break;
        _ = lines_iter.next().?;
        var it = std.mem.splitScalar(u8, line, '|');
        try rules.append(Rule {
            .before = it.next() orelse return error.InvalidRule,
            .after = it.next() orelse return error.InvalidRule,
        });
    }

    var sum_of_valid: i64 = 0;
    var sum_of_reordered: i64 = 0;
    while (lines_iter.next()) |line| {
        var update = std.ArrayList([]const u8).init(ally);
        var it = std.mem.splitScalar(u8, line, ',');
        while (it.next()) |item| {
            try update.append(item);
        }

        var is_valid = true;
        rule_loop: for (rules.items) |rule| {
            for (0.., update.items) |i, n| {
                if (!std.mem.eql(u8, n, rule.after)) continue;
                for (i..update.items.len) |j| {
                    if (std.mem.eql(u8, update.items[j], rule.before)) {
                        is_valid = false;
                        break :rule_loop;
                    }
                }
            }
        }
        if (is_valid) {
            sum_of_valid += try std.fmt.parseInt(i64, update.items[update.items.len / 2], 10);
        } else {
            std.mem.sortUnstable([]const u8, update.items, rules.items, applyRules);
            sum_of_reordered += try std.fmt.parseInt(i64, update.items[update.items.len / 2], 10);
        }
    }

    return .{sum_of_valid, sum_of_reordered};
}

fn applyRules(rules: []const Day5Rule, lhs: []const u8, rhs: []const u8) bool {
    for (rules) |rule| {
        if (std.mem.eql(u8, lhs, rule.before) and std.mem.eql(u8, rhs, rule.after)) return true;
        if (std.mem.eql(u8, rhs, rule.before) and std.mem.eql(u8, lhs, rule.after)) return false;
    }
    return false;
}

fn day6(input_file: std.fs.File) !Answers {
    const input_str = try input_file.readToEndAlloc(ally, 10_000_000);
    const map = Str2d.new(input_str);

    var initial_guard_pos = Vec2i.ZERO;
    var initial_guard_dir: Dir4 = undefined;
    looking_for_guard_loop: for (0..map.height()) |row| {
        for (0..map.width_sans_end) |col| {
            const dir = switch (map.at(row, col)) {
                '^' => Dir4.UP,
                'v' => Dir4.DOWN,
                '<' => Dir4.LEFT,
                '>' => Dir4.RIGHT,
                else => continue
            };
            initial_guard_pos = Vec2i.new(@intCast(col), @intCast(row));
            initial_guard_dir = dir;
            break :looking_for_guard_loop;
        }
    } else {
        return error.NoGuard;
    }

    const part1_count = try guard_map_count_positions(
        map,
        initial_guard_pos, initial_guard_dir,
        null,
    ) orelse unreachable;

    var part2_count: i64 = 0;
    for (0..map.height()) |row| {
        for (0..map.width_sans_end) |col| {
            const maybe_count = try guard_map_count_positions(
                map,
                initial_guard_pos, initial_guard_dir,
                Vec2i {.x = @intCast(col), .y = @intCast(row)},
            );
            if (maybe_count == null) {
                part2_count += 1;
            }
        }
    }

    return .{part1_count, part2_count};
}

fn guard_map_count_positions(
    map: Str2d,
    initial_guard_pos: Vec2i, initial_guard_dir: Dir4,
    obstruction: ?Vec2i,
) !?i64 {
    var positions = std.AutoHashMap(Vec2i, u4).init(ally);
    var guard_pos = initial_guard_pos;
    var guard_dir: Dir4 = initial_guard_dir;
    try positions.put(guard_pos, 0);
    while (true) {
        // std.debug.print("\n", .{});
        // for (0..map.height()) |row| {
        //     for (0..map.width_sans_end) |col| {
        //         var c = map.index(row, col);
        //         if (c == '^') {
        //             c = '.';
        //         }
        //         if (row == guard_pos.y and col == guard_pos.x) {
        //             c = '^';
        //         }
        //         std.debug.print("{c}", .{c});
        //     }
        //     std.debug.print("\n", .{});
        // }
        guard_pos.add(guard_dir.unit_vec());
        const char =
            if (obstruction != null and guard_pos.eql(obstruction.?)) '#'
            else map.get(guard_pos.y, guard_pos.x);
        switch (char) {
            '#' => {
                guard_pos.sub(guard_dir.unit_vec());
                guard_dir = guard_dir.clockwise();
            },
            0 => break,
            else => {
                const entry = try positions.getOrPutValue(guard_pos, 0);
                const bitmask = @as(u4, 1) << @intFromEnum(guard_dir);
                if (entry.value_ptr.* & bitmask != 0) {
                    // The guard is in a loop.
                    return null;
                }
                entry.value_ptr.* |= bitmask;
            }
        }
    }

    return positions.count();
}

fn day7(input_file: std.fs.File) !Answers {
    const input = try input_file.readToEndAlloc(ally, 10_000_000);
    var line_iter = std.mem.tokenizeAny(u8, input, "\r\n");
    const Equation = struct {
        testval: u64,
        numbers: []u16,
    };
    var equations = std.ArrayList(Equation).init(ally);
    while (line_iter.next()) |line| {
        const testval, const end = try parseIntUntilItsEnd(u64, line[0..]);
        if (line[end] != ':') return error.NoColonInEquation;
        var num_iter = std.mem.tokenizeScalar(u8, line[end+1..], ' ');
        var num_list = std.ArrayList(u16).init(ally);
        while (num_iter.next()) |num_str| {
            try num_list.append(try std.fmt.parseInt(u16, num_str, 10));
        }
        try equations.append(Equation {
            .testval = testval,
            .numbers = try num_list.toOwnedSlice(),
        });
    }

    const Op = Day7Op;

    var part1_calibration: i64 = 0;
    for (equations.items) |equation| {
        if (equation_can_be_true(&[_]Op{.@"+", .@"*"}, equation.testval, equation.numbers)) {
            part1_calibration += @intCast(equation.testval);
        }
    }

    var part2_calibration: i64 = 0;
    for (equations.items) |equation| {
        if (equation_can_be_true(&[_]Op{.@"+", .@"*", .@"||"}, equation.testval, equation.numbers)) {
            part2_calibration += @intCast(equation.testval);
        }
    }

    return .{part1_calibration, part2_calibration};
}

const Day7Op = enum {
    @"+",
    @"*",
    @"||",
};

fn equation_can_be_true(comptime operations: []const Day7Op, testval: u64, numbers: []const u16) bool {
    std.debug.assert(numbers.len > 0);
    return _equation_can_be_true(operations, testval, numbers[0], numbers[1..]);
}

fn _equation_can_be_true(comptime operations: []const Day7Op, testval: u64, used: u64, remaining: []const u16) bool {
    if (remaining.len == 0) return testval == used;
    inline for (operations) |op| {
        const new_used = switch (op) {
            .@"+" => used + remaining[0],
            .@"*" => used * remaining[0],
            .@"||" => concatenateInts(u64, used, remaining[0]),
        };
        if (_equation_can_be_true(operations, testval, new_used, remaining[1..])) return true;
    }
    return false;
}

fn day8(input_file: std.fs.File) !Answers {
    const map = Str2d.new(try input_file.readToEndAlloc(ally, 10_000_000));

    var antinodes = std.AutoHashMap(Vec2i, void).init(ally);
    for (0..map.height()) |row1| {
        for (0..map.width_sans_end) |col1| {
            const freq1 = map.at(row1, col1);
            if (!std.ascii.isAlphanumeric(freq1)) continue;

            const pos1 = Vec2i.colrowCast(col1, row1);
            for (0..map.height()) |row2| {
                for (0..map.width_sans_end) |col2| {
                    const freq2 = map.at(row2, col2);
                    if (!std.ascii.isAlphanumeric(freq2)) continue;
                    if (freq1 != freq2) continue;

                    const pos2 = Vec2i.colrowCast(col2, row2);
                    if (pos1.eql(pos2)) continue;

                    const dist = pos2.minus(pos1);
                    const dist_times_2 = dist.times(Vec2i.new(2, 2));
                    var pos = pos1.minus(dist_times_2);
                    const end = pos2.plus(dist_times_2);
                    while (true) : (pos.add(dist)) {
                        if (pos.eql(pos1) or pos.eql(pos2)) continue;

                        if (map.inBounds(pos.y, pos.x) and (
                            pos.minus(pos1).eql(dist) or
                            pos.minus(pos2).eql(dist)
                        ) and (
                            pos.minus(pos1).eql(dist_times_2) or
                            pos.minus(pos2).eql(dist_times_2)
                        )) {
                            try antinodes.put(pos, {});
                        }

                        if (pos.eql(end)) break;
                    }
                }
            }
        }
    }
    const part1_antinode_count = antinodes.count();

    for (0..map.height()) |row1| {
        for (0..map.width_sans_end) |col1| {
            const freq1 = map.at(row1, col1);
            if (!std.ascii.isAlphanumeric(freq1)) continue;

            const pos1 = Vec2i.colrowCast(col1, row1);
            for (0..map.height()) |row2| {
                for (0..map.width_sans_end) |col2| {
                    const freq2 = map.at(row2, col2);
                    if (!std.ascii.isAlphanumeric(freq2)) continue;
                    if (freq1 != freq2) continue;

                    const pos2 = Vec2i.colrowCast(col2, row2);
                    if (pos1.eql(pos2)) continue;

                    const dist = pos2.minus(pos1);
                    var pos = pos1;
                    while (map.inBounds(pos.y, pos.x)) pos.sub(dist);
                    var end = pos2;
                    while (map.inBounds(end.y, end.x)) end.add(dist);

                    while (true) : (pos.add(dist)) {
                        if (map.inBounds(pos.y, pos.x)) {
                            try antinodes.put(pos, {});
                        }

                        if (pos.eql(end)) break;
                    }
                }
            }
        }
    }
    const part2_antinode_count = antinodes.count();

    // for (0..map.height()) |row| {
    //     for (0..map.width_sans_end) |col| {
    //         std.debug.print("{c}", .{
    //             if (antinodes.contains(Vec2i.colrowCast(col, row))) '#'
    //             else map.index(row, col)
    //         });
    //     }
    //     std.debug.print("\n", .{});
    // }

    return .{part1_antinode_count, part2_antinode_count};
}

fn day9(input_file: std.fs.File) !Answers {
    var disk_original: []?u32 = undefined;
    var last_file_id: u32 = undefined;
    {
        var disk_dyn = std.ArrayList(?u32).init(ally);
        var buf_reader = std.io.bufferedReader(input_file.reader());
        var reader = buf_reader.reader();

        var file_id: u32 = 0;
        while (reader.readByte()) |byte| {
            if (!std.ascii.isDigit(byte)) break;
            const n = byte - '0';
            // We already advanced by 1, so it's backwards.
            if (buf_reader.start % 2 != 0) {
                try disk_dyn.appendNTimes(file_id, n);
                file_id += 1;
            } else {
                try disk_dyn.appendNTimes(null, n);
            }
        } else |err| {
            if (err != error.EndOfStream) return err;
        }

        disk_original = try disk_dyn.toOwnedSlice();
        last_file_id = file_id - 1;
    }
    defer ally.free(disk_original);

    const part1_checksum = part1: {
        const disk = try copySlice(?u32, disk_original, ally);
        defer ally.free(disk);

        var free_block: usize = 0;
        var last_block: usize = disk.len - 1;
        compact_loop: while (true) {
            while (disk[free_block] != null) {
                free_block += 1;
                if (free_block >= disk.len) break :compact_loop;
            }
            while (disk[last_block] == null) {
                if (last_block == 0) break :compact_loop;
                last_block -= 1;
            }
            if (last_block <= free_block) break :compact_loop;
            disk[free_block] = disk[last_block];
            disk[last_block] = null;
        }

        // for (disk) |block| {
        //     if (block) |n| std.debug.print("{d}", .{n})
        //     else std.debug.print(".", .{});
        // }
        // std.debug.print("\n", .{});

        std.debug.assert(blk: {
            var i: usize = 0;
            while (disk[i] != null) {
                i += 1;
                if (i >= disk.len) break :blk true;
            }
            while (i < disk.len) : (i += 1) {
                if (disk[i] != null) break :blk false;
            }
            break :blk true;
        });

        break :part1 day9FileChecksum(disk);
    };

    const part2_checksum = part2: {
        const disk = try copySlice(?u32, disk_original, ally);
        defer ally.free(disk);

        var file_id = last_file_id;
        while (true) {
            const file_last = std.mem.lastIndexOfScalar(?u32, disk, file_id)
                orelse unreachable; // end is inclusive
            const file_first = std.mem.indexOfScalar(?u32, disk, file_id)
                orelse unreachable;
            const file_len = file_last - file_first + 1;

            var free_start: usize = 0;
            var free_end = free_start; // end is exclusive
            var found_enough_space = false;
            while (true) {
                if (free_start >= disk.len) break;

                while (disk[free_start] != null) {
                    free_start += 1;
                    if (free_start >= disk.len) break;
                }
                free_end = free_start;
                while (disk[free_end] == null) {
                    free_end += 1;
                    if (free_end >= disk.len) break;
                }

                if (free_end > file_first) {
                    break;
                }
                if (free_end - free_start >= file_len) {
                    found_enough_space = true;
                    break;
                }
                free_start = free_end;
            }

            if (found_enough_space) {
                @memset(disk[free_start..free_start+file_len], file_id);
                @memset(disk[file_first..file_last+1], null);
            }

            if (file_id == 0) break;
            file_id -= 1;
        }

        // for (disk) |block| {
        //     if (block) |n| std.debug.print("{d}", .{n})
        //     else std.debug.print(".", .{});
        // }
        // std.debug.print("\n", .{});

        break :part2 day9FileChecksum(disk);
    };

    return .{part1_checksum, part2_checksum};
}

fn day9FileChecksum(disk: []const ?u32) i64 {
    var checksum: i64 = 0;
    for (0.., disk) |i, block| {
        const file_id = block orelse continue;
        checksum += @intCast(i * file_id);
    }
    return checksum;
}

fn day10(input_file: std.fs.File) !Answers {
    const map: Array2d(u8) = blk: {
        const size = try file_get_2d_size(input_file);
        var map = try Array2d(u8).init(ally, size.line_count, size.line_len);

        try input_file.seekTo(0);
        var buf_reader = std.io.bufferedReader(input_file.reader());
        var reader = buf_reader.reader();
        for (0..map.data.len) |i| {
            // We should not reach EoF since we already know the length.
            var c = try reader.readByte();
            while (c == '\r' or c == '\n') c = try reader.readByte();
            std.debug.assert(std.ascii.isDigit(c));
            map.data[i] = c - '0';
        }
        break :blk map;
    };

    var total_score: i64 = 0;
    var total_rating: i64 = 0;
    for (0..map.h) |row| {
        for (0..map.w) |col| {
            if (map.at(row, col) == 0) {
                const pos = Vec2i.colrowCast(col, row);
                total_score += score_trailhead(map, pos);
                total_rating += rate_trailhead(map, pos);
            }
        }
    }

    return .{total_score, total_rating};
}

fn score_trailhead(map: Array2d(u8), pos: Vec2i) i64 {
    var nines_reached = std.ArrayList(Vec2i).init(ally);
    defer nines_reached.deinit();
    return _score_trailhead(map, pos, &nines_reached);
}

fn _score_trailhead(map: Array2d(u8), pos: Vec2i, nines_reached: *std.ArrayList(Vec2i)) i64 {
    var score: i64 = 0;
    const height = map.atv(pos);
    if (height == 9) {
        for (nines_reached.items) |nine_pos| {
            if (nine_pos.eql(pos)) return 0;
        }
        nines_reached.append(pos)
            catch |err| std.debug.panic("{any}", .{err});
        return 1;
    }
    for (Dir4.ALL) |dir| {
        const next_pos = pos.plus(dir.unit_vec());
        const next_h = map.getv(next_pos) orelse continue;
        if (next_h == height + 1) {
            score += _score_trailhead(map, next_pos, nines_reached);
        }
    }
    return score;
}

fn rate_trailhead(map: Array2d(u8), pos: Vec2i) i64 {
    var score: i64 = 0;
    const height = map.atv(pos);
    if (height == 9) return 1;
    for (Dir4.ALL) |dir| {
        const next_pos = pos.plus(dir.unit_vec());
        const next_h = map.getv(next_pos) orelse continue;
        if (next_h == height + 1) {
            score += rate_trailhead(map, next_pos);
        }
    }
    return score;
}

fn day11(input_file: std.fs.File) !Answers {
    var stones = std.ArrayList(u64).init(ally);
    {
        const input = try input_file.readToEndAlloc(ally, 10_000_000);
        var iter = std.mem.tokenizeScalar(u8, input, ' ');
        while (iter.next()) |raw_str| {
            const str = std.mem.trim(u8, raw_str, "\r\n");
            if (str.len == 0) continue;
            const n = try std.fmt.parseInt(u64, str, 10);
            try stones.append(n);
        }
    }

    // Part 1 solution. Not efficient enough for part 2.
    // for (0..75) |_| {
    //     var i = stones.items.len;
    //     while (i > 0) {
    //         i -= 1;
    //         const stone = stones.items[i];
    //         if (stone == 0) {
    //             stones.items[i] = 1;
    //         } else if (countDigits(u64, stone) % 2 == 0) {
    //             const new1, const new2 = splitDigitsInHalf(u64, stone);
    //             stones.items[i] = new1;
    //             try stones.insert(i + 1, new2);
    //         } else {
    //             stones.items[i] *= 2024;
    //         }
    //     }
    // }

    // for (stones.items) |stone| {
    //     std.debug.print("{d} ", .{stone});
    // }
    // std.debug.print("\n", .{});

    const CountStonesInput = struct {
        stone: u64,
        generations: u64,

        const Self = @This();

        fn nextGen(self: *const Self, new_stone: u64) Self {
            return Self {
                .stone = new_stone,
                .generations = self.generations - 1,
            };
        }
    };
    const StoneCounter = struct {
        cache: Cache,

        const Self = @This();
        const Cache = std.AutoHashMap(CountStonesInput, u64);

        fn countStones(self: *Self, in: CountStonesInput) u64 {
            return switch (in.generations) {
                0 => 1,
                1 => self.applyRules(in),
                else => {
                    if (!self.cache.contains(in)) {
                        self.cache.put(in, self.applyRules(in))
                            catch |err| std.debug.panic("{any}", .{err});
                    }
                    return self.cache.get(in).?;
                }
            };
        }

        fn applyRules(self: *Self, in: CountStonesInput) u64 {
            if (in.stone == 0) return self.countStones(in.nextGen(1));
            if (countDigits(u64, in.stone) % 2 == 0) {
                const new1, const new2 = splitDigitsInHalf(u64, in.stone);
                return self.countStones(in.nextGen(new1))
                    + self.countStones(in.nextGen(new2));
            }
            return self.countStones(in.nextGen(in.stone * 2024));
        }
    };

    var stone_counter = StoneCounter {
        .cache = StoneCounter.Cache.init(ally),
    };
    var part1_count: u64 = 0;
    for (stones.items) |stone| {
        part1_count += stone_counter.countStones(CountStonesInput {
            .stone = stone,
            .generations = 25,
        });
    }
    var part2_count: u64 = 0;
    for (stones.items) |stone| {
        part2_count += stone_counter.countStones(CountStonesInput {
            .stone = stone,
            .generations = 75,
        });
    }

    return .{@intCast(part1_count), @intCast(part2_count)};
}

const Day12 = struct {
    map: Str2d,
    regions: std.ArrayList(Region),
    in_a_region: Array2d(bool),

    const Self = @This();

    const Region = struct {
        plant: u8,
        plots: std.ArrayList(Vec2u16),
    };

    fn day12(input_file: std.fs.File) !Answers {
        var self = Self {
            .map = Str2d.new(try input_file.readToEndAlloc(ally, 10_000_000)),
            .regions = std.ArrayList(Region).init(ally),
            .in_a_region = undefined,
        };
        self.in_a_region = try Array2d(bool).init(ally, self.map.height(), self.map.width_sans_end);

        // Keep track of plots in each region
        for (0..self.map.height()) |row| {
            for (0..self.map.width_sans_end) |col| {
                // If the plot is in a group, it's already processed.
                if (self.in_a_region.at(row, col)) continue;

                try self.regions.append(Region {
                    .plant = self.map.at(row, col),
                    .plots = std.ArrayList(Vec2u16).init(ally),
                });
                try self.regionMarkAll(Vec2i.colrowCast(col, row));
            }
        }
        std.debug.assert(std.mem.allEqual(bool, self.in_a_region.data, true));

        // Calculate perimeter and area for each region
        var total_price: i64 = 0;
        for (self.regions.items) |region| {
            var region_perimeter: u32 = 0;
            for (region.plots.items) |pos_u16| {
                const pos: Vec2i = pos_u16.intCast(i32);
                for (Dir4.ALL) |dir| {
                    if (self.map.getv(pos.plus(dir.unit_vec())) != region.plant) {
                        region_perimeter += 1;
                    }
                }
            }
            const region_area = region.plots.items.len;
            total_price += region_perimeter * @as(i64, @intCast(region_area));
        }

        // Part 2 ideas:
        // Movement Dir4 and affix Dir4
        // around the outside of the edge of a region
        // when reaching plots not of the region, affix = movement and movement = affix.opposite()

        return .{total_price, -1};
    }

    fn regionMarkAll(self: *Self, start_pos: Vec2i) !void {
        if (self.in_a_region.atv(start_pos)) return;
        self.in_a_region.mutv(start_pos).* = true;

        try self.regions.items[self.regions.items.len-1].plots.append(start_pos.intCast(u16));
        const plot = self.map.atv(start_pos);
        for (Dir4.ALL) |dir| {
            const pos = start_pos.plus(dir.unit_vec());
            if (self.map.getv(pos) == plot) {
                try self.regionMarkAll(pos);
            }
        }
    }
};

// Helper functions and types

const Dir4 = enum {
    UP, DOWN, LEFT, RIGHT,

    const ALL = [4]Dir4{
        Dir4.UP, Dir4.DOWN, Dir4.LEFT, Dir4.RIGHT,
    };

    fn clockwise(dir: Dir4) Dir4 {
        return switch (dir) {
            .UP => .RIGHT,
            .RIGHT => .DOWN,
            .DOWN => .LEFT,
            .LEFT => .UP,
        };
    }

    fn unit_vec(self: Dir4) Vec2i {
        return switch (self) {
            .UP => Vec2i.UP,
            .RIGHT => Vec2i.RIGHT,
            .DOWN => Vec2i.DOWN,
            .LEFT => Vec2i.LEFT,
        };
    }
};

const Vec2i = GenericVec2(i32);
const Vec2u16 = GenericVec2(u16);

fn GenericVec2(comptime T: type) type {
    return struct {
        x: T,
        y: T,

        const Self = @This();

        const ZERO = Self.new(0, 0);
        const UP = Self.new(0, -1);
        const DOWN = Self.new(0, 1);
        const LEFT = Self.new(-1, 0);
        const RIGHT = Self.new(1, 0);

        fn new(x: T, y: T) Self {
            return Self { .x = x, .y = y };
        }
        fn colrowCast(col: usize, row: usize) Self {
            return Self.new(@intCast(col), @intCast(row));
        }

        fn intCast(self: Self, comptime NewT: type) GenericVec2(NewT) {
            return GenericVec2(NewT) {
                .x = @intCast(self.x),
                .y = @intCast(self.y),
            };
        }

        fn eql(self: Self, other: Self) bool {
            return self.x == other.x and self.y == other.y;
        }

        fn abs(self: Self) Self {
            // @abs() returns an unsigned type, so cast to re-"sign" it.
            return Self {
                .x = @intCast(@abs(self.x)),
                .y = @intCast(@abs(self.y)),
            };
        }

        fn plus(self: Self, other: Self) Self {
            return self._mutate_copy(add, other);
        }
        fn minus(self: Self, other: Self) Self {
            return self._mutate_copy(sub, other);
        }
        fn times(self: Self, other: Self) Self {
            return self._mutate_copy(mul, other);
        }
        inline fn _mutate_copy(self: Self, op: fn(*Self, Self) void, other: Self) Self {
            var copy = self;
            op(&copy, other);
            return copy;
        }

        fn add(self: *Self, other: Self) void {
            self.x += other.x;
            self.y += other.y;
        }
        fn sub(self: *Self, other: Self) void {
            self.x -= other.x;
            self.y -= other.y;
        }
        fn mul(self: *Self, other: Self) void {
            self.x *= other.x;
            self.y *= other.y;
        }
    };
}

test "mutating vec" {
    var multest = Vec2i.new(2, 3);
    multest.mul(Vec2i.new(4, 5));
    try std.testing.expectEqual(Vec2i.new(8, 15), multest);
}
test "vec arithmetic" {
    const result = Vec2i.new(1, 2).plus(Vec2i.new(3, 4));
    try std.testing.expectEqual(Vec2i.new(4, 6), result);
}
test "vec abs" {
    const both_neg = Vec2i.new(-2, -3);
    try std.testing.expectEqual(Vec2i.new(2, 3), both_neg.abs());
    const one_neg = Vec2i.new(-4, 5);
    try std.testing.expectEqual(Vec2i.new(4, 5), one_neg.abs());
    const no_neg = Vec2i.new(6, 7);
    try std.testing.expectEqual(Vec2i.new(6, 7), no_neg.abs());
}

/// A wrapper to treat a 1D string as being 2D, with the lines as rows. The
/// lines must be the same length.
const Str2d = struct {
    str1d: []const u8,
    /// Line width, including newline char(s).
    width: usize,
    /// Printable line width (excluding newline char(s)).
    width_sans_end: usize,

    fn new(str: []const u8) Str2d {
        var self = Str2d {
            .str1d = str,
            .width = 0,
            .width_sans_end = 0,
        };
        if (std.mem.indexOf(u8, str, "\r\n")) |line_end_idx| {
            self.width_sans_end = line_end_idx;
            self.width = line_end_idx + 2;
        } else if (std.mem.indexOfScalar(u8, str, '\n')) |line_end_idx| {
            self.width_sans_end = line_end_idx;
            self.width = line_end_idx + 1;
        } else {
            self.width_sans_end = str.len;
            self.width = str.len;
        }
        return self;
    }

    fn height(self: *const Str2d) usize {
        return self.str1d.len / self.width + @as(usize,
            if (self.str1d.len % self.width == 0) 0
            else 1
        );
    }

    /// Directly index the string. Like usual, this will panic or cause illegal behavior if out of bounds.
    fn at(self: *const Str2d, row: usize, col: usize) u8 {
        return self.str1d[row * self.width + col];
    }
    fn atv(self: *const Str2d, v: Vec2i) u8 {
        std.debug.assert(v.x >= 0 and v.y >= 0);
        return self.at(@intCast(v.y), @intCast(v.x));
    }

    /// Get the char at the row and column if it exists, else the null char.
    fn get(self: *const Str2d, row: isize, col: isize) u8 {
        return
            if (self.inBounds(row, col)) self.at(@intCast(row), @intCast(col))
            else 0;
    }
    fn getv(self: *const Str2d, v: Vec2i) u8 {
        return self.get(@intCast(v.y), @intCast(v.x));
    }

    fn inBounds(self: *const Str2d, row: isize, col: isize) bool {
        return !(row < 0 or row >= self.height() or col < 0 or col >= self.width_sans_end);
    }
    fn inBoundsv(self: *const Str2d, v: Vec2i) bool {
        const row = v.y;
        const col = v.x;
        return !(row < 0 or row >= self.height() or col < 0 or col >= self.width_sans_end);
    }
};

test "that Str2d works" {
    const s = Str2d.new("abc\ndef");
    try std.testing.expectEqual(4, s.width);
    try std.testing.expectEqual('b', s.at(0, 1));
    try std.testing.expectEqual('f', s.at(1, 2));
}

fn Array2d(comptime T: type) type {
    return struct {
        data: []T,
        h: usize,
        w: usize,

        const Self = @This();

        fn init(allocator: std.mem.Allocator, h: usize, w: usize) std.mem.Allocator.Error!Self {
            return Self {
                .data = try allocator.alloc(T, h * w),
                .h = h,
                .w = w,
            };
        }

        fn at(self: *const Self, row: usize, col: usize) T {
            return self.data[row * self.w + col];
        }
        fn atv(self: *const Self, v: Vec2i) T {
            std.debug.assert(v.x >= 0 and v.y >= 0);
            return self.at(@intCast(v.y), @intCast(v.x));
        }
        fn mut(self: *const Self, row: usize, col: usize) *T {
            return &self.data[row * self.w + col];
        }
        fn mutv(self: *const Self, v: Vec2i) *T {
            std.debug.assert(v.x >= 0 and v.y >= 0);
            return self.mut(@intCast(v.y), @intCast(v.x));
        }

        fn set(self: *Self, row: usize, col: usize, value: T) void {
            self.data[row * self.w + col] = value;
        }
        fn setv(self: *Self, v: Vec2i, value: T) void {
            std.debug.assert(v.x >= 0 and v.y >= 0);
            return self.set(@intCast(v.y), @intCast(v.x), value);
        }

        fn get(self: *const Self, row: usize, col: usize) ?T {
            return
                if (self.inBounds(row, col)) self.at(row, col)
                else null;
        }
        fn getv(self: *const Self, v: Vec2i) ?T {
            if (v.x < 0 or v.y < 0) return null;
            return self.get(@intCast(v.y), @intCast(v.x));
        }

        fn getOrDefault(self: *const Self, row: usize, col: usize, default: T) T {
            return self.get(row, col) orelse default;
        }

        fn inBounds(self: *const Self, row: usize, col: usize) bool {
            return !(row < 0 or row >= self.h or col < 0 or col >= self.w);
        }
    };
}

test "that Array2d works" {
    var arr = try Array2d(u8).init(ally, 2, 3);
    @memcpy(arr.data, &[_]u8{
        'a', 'b', 'c',
        'd', 'e', 'f',
    });
    try std.testing.expectEqual(3, arr.w);
    try std.testing.expectEqual('b', arr.at(0, 1));
    try std.testing.expectEqual('f', arr.at(1, 2));
    try std.testing.expectEqual('n', arr.getOrDefault(9, 9, 'n'));
}

/// Gets a value from a 2D slice if within bounds, else the given default.
fn get2d(comptime T: type, slice: []const []const T, i: isize, j: isize, default: T) T {
    return
        if (i < 0 or i >= slice.len or j < 0 or j >= slice[@intCast(i)].len) default
        else slice[@intCast(i)][@intCast(j)];
}

fn parseI32(buf: []const u8) std.fmt.ParseIntError!i32 {
    return std.fmt.parseInt(i32, buf, 10);
}

/// Parses out an int of the given type, until it encounters a non-digit char.
fn parseIntUntilItsEnd(comptime T: type, buf: []const u8) std.fmt.ParseIntError!struct {T, usize} {
    var end: usize = 0;
    while (end < buf.len and std.ascii.isDigit(buf[end])) end += 1;
    const n = try std.fmt.parseInt(T, buf[0..end], 10);
    return .{n, end};
}

fn splitDigitsInHalf(comptime T: type, n: T) struct {T, T} {
    const t_info = @typeInfo(T);
    if (t_info != .Int or t_info.Int.signedness != .unsigned) {
        @compileError("unsigned int only");
    }
    std.debug.assert(countDigits(T, n) % 2 == 0);
    const half = std.math.powi(T, 10, countDigits(T, n) / 2) catch unreachable;
    return .{n / half, n % half};
}

test "splitting digits" {
    try std.testing.expectEqualDeep(.{1, 2}, splitDigitsInHalf(u32, 12));
    try std.testing.expectEqualDeep(.{9, 9}, splitDigitsInHalf(u32, 99));
    try std.testing.expectEqualDeep(.{12, 34}, splitDigitsInHalf(u32, 1234));
}

fn countDigits(comptime T: type, n: T) T {
    var digits: T = 1;
    while (std.math.powi(T, 10, digits) catch unreachable <= n) digits += 1;
    return digits;
}

test "counting digits" {
    for (0..10) |n| {
        try std.testing.expectEqual(1, countDigits(usize, n));
    }

    try std.testing.expectEqual(2, countDigits(u32, 10));
    try std.testing.expectEqual(2, countDigits(u32, 11));
    try std.testing.expectEqual(2, countDigits(u32, 19));
    try std.testing.expectEqual(2, countDigits(u32, 20));
    try std.testing.expectEqual(2, countDigits(u32, 50));
    try std.testing.expectEqual(2, countDigits(u32, 99));

    try std.testing.expectEqual(3, countDigits(u32, 100));
}

inline fn concatenateInts(comptime T: type, a: T, b: T) T {
    var pow: T = 10;
    while (b >= pow) pow *= 10;
    return a * pow + b;
}

test "concatenating integers" {
    try std.testing.expectEqual(123456, concatenateInts(u32, 123, 456));
}

/// Caller owns the returned slice.
fn copySlice(comptime T: type, src: []const T, allocator: std.mem.Allocator) ![]T {
    const dst = try allocator.alloc(T, src.len);
    std.mem.copyForwards(T, dst, src);
    return dst;
}

const FileSize2d = struct {
    line_len: usize,
    line_count: usize,
};
fn file_get_2d_size(file: std.fs.File) !FileSize2d {
    var buf_reader = std.io.bufferedReader(file.reader());
    var reader = buf_reader.reader();

    var line_len: usize = 0;
    while (reader.readByte()) |byte| {
        switch (byte) {
            '\r' => {
                const byte2 = reader.readByte()
                    catch |err|
                        if (err == error.EndOfStream) 0
                        else return err;
                if (byte2 != '\n') {
                    return error.MalformedNewline;
                }
                break;
            },
            '\n' => {
                break;
            },
            else => line_len += 1,
        }
    } else |err| {
        if (err != error.EndOfStream) return err;
    }

    if (line_len == 0) return FileSize2d {
        .line_len = 0,
        .line_count = 0,
    };

    var line_count: usize = 1;
    while (true) {
        reader.skipBytes(line_len, .{})
            catch |err| if (err == error.EndOfStream) break else return err;
        line_count += 1;
        var c = reader.readByte() catch |err| if (err == error.EndOfStream) break else return err;
        if (c == '\r') {
            c = reader.readByte() catch |err| if (err == error.EndOfStream) break else return err;
        }
        if (c != '\n') {
            return error.MalformedNewline;
        }
    }

    std.debug.assert(reader.readByte() == error.EndOfStream);
    return FileSize2d {
        .line_len = line_len,
        .line_count = line_count,
    };
}
