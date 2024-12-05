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

fn day1(input_file: std.fs.File) !struct { i64, i64 } {
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

fn day2(input_file: std.fs.File) !struct { i64, i64 } {
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

fn day3(input_file: std.fs.File) !struct { i64, i64 } {
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

fn day4(input_file: std.fs.File) !struct { i64, i64 } {
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
fn day5(input_file: std.fs.File) !struct { i64, i64 } {
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

fn parseI32(buf: []const u8) std.fmt.ParseIntError!i32 {
    return std.fmt.parseInt(i32, buf, 10);
}

fn parseIntUntilEnd(comptime T: type, buf: []const u8) std.fmt.ParseIntError!struct {i32, usize} {
    var end: usize = 0;
    while (end < buf.len and std.ascii.isDigit(buf[end])) end += 1;
    const n = try std.fmt.parseInt(T, buf, 10);
    return .{n, end};
}

fn get2d(comptime T: type, slice: []const []const T, i: isize, j: isize, default: T) T {
    return
        if (i < 0 or i >= slice.len or j < 0 or j >= slice[@intCast(i)].len) default
        else slice[@intCast(i)][@intCast(j)];
}

fn is_blank(str: []const u8) bool {
    return std.mem.trim(u8, str, " ").len == 0;
}