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

    inline for (1..days.len + 1) |day| {
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
            const dir = switch (map.index(row, col)) {
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
            const freq1 = map.index(row1, col1);
            if (!std.ascii.isAlphanumeric(freq1)) continue;

            const pos1 = Vec2i.colrowCast(col1, row1);
            for (0..map.height()) |row2| {
                for (0..map.width_sans_end) |col2| {
                    const freq2 = map.index(row2, col2);
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
            const freq1 = map.index(row1, col1);
            if (!std.ascii.isAlphanumeric(freq1)) continue;

            const pos1 = Vec2i.colrowCast(col1, row1);
            for (0..map.height()) |row2| {
                for (0..map.width_sans_end) |col2| {
                    const freq2 = map.index(row2, col2);
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

// Helper functions and types

const Dir4 = enum {
    UP, DOWN, LEFT, RIGHT,

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

const Vec2i = struct {
    x: i32,
    y: i32,

    const ZERO = Vec2i.new(0, 0);
    const UP = Vec2i.new(0, -1);
    const DOWN = Vec2i.new(0, 1);
    const LEFT = Vec2i.new(-1, 0);
    const RIGHT = Vec2i.new(1, 0);

    fn new(x: i32, y: i32) Vec2i {
        return Vec2i { .x = x, .y = y };
    }
    fn colrowCast(col: usize, row: usize) Vec2i {
        return Vec2i.new(@intCast(col), @intCast(row));
    }

    fn eql(self: Vec2i, other: Vec2i) bool {
        return self.x == other.x and self.y == other.y;
    }

    fn abs(self: Vec2i) Vec2i {
        // @abs() returns an unsigned type, so cast to re-"sign" it.
        return Vec2i {
            .x = @intCast(@abs(self.x)),
            .y = @intCast(@abs(self.y)),
        };
    }

    fn plus(self: Vec2i, other: Vec2i) Vec2i {
        return self._mutate_copy(add, other);
    }
    fn minus(self: Vec2i, other: Vec2i) Vec2i {
        return self._mutate_copy(sub, other);
    }
    fn times(self: Vec2i, other: Vec2i) Vec2i {
        return self._mutate_copy(mul, other);
    }
    inline fn _mutate_copy(self: Vec2i, op: fn(*Vec2i, Vec2i) void, other: Vec2i) Vec2i {
        var copy = self;
        op(&copy, other);
        return copy;
    }

    fn add(self: *Vec2i, other: Vec2i) void {
        self.x += other.x;
        self.y += other.y;
    }
    fn sub(self: *Vec2i, other: Vec2i) void {
        self.x -= other.x;
        self.y -= other.y;
    }
    fn mul(self: *Vec2i, other: Vec2i) void {
        self.x *= other.x;
        self.y *= other.y;
    }
};

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

inline fn concatenateInts(comptime T: type, a: T, b: T) T {
    var pow: T = 10;
    while (b >= pow) pow *= 10;
    return a * pow + b;
}

test "concatenating integers" {
    try std.testing.expectEqual(123456, concatenateInts(u32, 123, 456));
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

/// Gets a value from a 2D slice if within bounds, else the given default.
fn get2d(comptime T: type, slice: []const []const T, i: isize, j: isize, default: T) T {
    return
        if (i < 0 or i >= slice.len or j < 0 or j >= slice[@intCast(i)].len) default
        else slice[@intCast(i)][@intCast(j)];
}

/// A wrapper to treat a 1D string as being 2D, with the lines as rows. The
/// lines must be the same length.
const Str2d = struct {
    str1d: []const u8,
    /// Line width, including newline char(s).
    width: usize,
    /// Printable line width (excluding newline char(s)).
    width_sans_end: usize,

    fn new(str: []const u8) @This() {
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

    fn height(self: @This()) usize {
        return self.str1d.len / self.width + @as(usize,
            if (self.str1d.len % self.width == 0) 0
            else 1
        );
    }

    /// Directly index the string. Like usual, this will panic or cause illegal behavior if out of bounds.
    fn index(self: @This(), row: usize, col: usize) u8 {
        return self.str1d[row * self.width + col];
    }

    /// Get the char at the row and column if it exists, else the null char.
    fn get(self: @This(), row: isize, col: isize) u8 {
        return
            if (self.inBounds(row, col)) self.index(@intCast(row), @intCast(col))
            else 0;
    }

    fn inBounds(self: @This(), row: isize, col: isize) bool {
        return !(row < 0 or row >= self.height() or col < 0 or col >= self.width_sans_end);
    }
};

test "that Str2d works" {
    const s = Str2d.new("abc\ndef");
    try std.testing.expectEqual(4, s.width);
    try std.testing.expectEqual('b', s.index(0, 1));
    try std.testing.expectEqual('f', s.index(1, 2));
}
