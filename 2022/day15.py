import re
import sys
from typing import Iterable

Position = tuple[int, int]

LINE_REGEX = re.compile(r'Sensor at x=(-?\d+), y=(-?\d+): closest beacon is at x=(-?\d+), y=(-?\d+)')


def parse_input(lines: Iterable[str]) -> Iterable[tuple[Position, Position]]:
    for line in lines:
        s_x, s_y, b_x, b_y = LINE_REGEX.match(line).groups()
        yield (int(s_x), int(s_y)), (int(b_x), int(b_y))


def impossible_beacon_locations(sensors_beacons: Iterable[tuple[Position, Position]], y: int) -> int:
    known_beacon_positions = set()
    impossible_x_intervals = []
    for sensor, beacon in sensors_beacons:
        if beacon[1] == y:
            known_beacon_positions.add(beacon[0])
        distance = manhattan_distance(sensor, beacon)
        distance_to_y = abs(sensor[1] - y)
        if distance_to_y <= distance:
            radius = distance - distance_to_y
            impossible_x_intervals.append((sensor[0] - radius, sensor[0] + radius))
    joined_intervals = get_joined_intervals(impossible_x_intervals)
    return sum(end - start + 1 for start, end in joined_intervals) - len(known_beacon_positions)


def manhattan_distance(p1: Position, p2: Position) -> int:
    return abs(p1[0] - p2[0]) + abs(p1[1] - p2[1])


def get_constraints(
    sensors_beacons: Iterable[tuple[Position, Position]]
) -> Iterable[tuple[int, int, int, int]]:
    for sensor, beacon in sensors_beacons:
        distance = manhattan_distance(sensor, beacon)
        x, y = sensor
        yield x + y - distance, x + y + distance, x - y - distance, x - y + distance


def check_sum(position_sum: int, constraints: Iterable[tuple[int, int, int, int]]) -> tuple[bool, int]:
    difference_radius = min(position_sum, 8_000_000 - position_sum)
    applicable_constraints = [c for c in constraints if c[0] <= position_sum <= c[1]]
    diff_intervals = ((c1, c2) for _, _, c1, c2 in applicable_constraints)
    joined_intervals = get_joined_intervals(diff_intervals)
    middle_interval = get_middle_interval(joined_intervals)
    if not middle_interval:
        return False, 0
    missing_point = get_missing_point(middle_interval, difference_radius, position_sum)
    if missing_point is not None:
        return False, missing_point
    return True, get_next_sum_to_check(applicable_constraints, position_sum, middle_interval)


def get_middle_interval(intervals: Iterable[tuple[int, int]]) -> tuple[int, int] | None:
    middle_intervals = [interval for interval in intervals if interval[0] <= 0 <= interval[1]]
    if not middle_intervals:
        return None
    assert len(middle_intervals) == 1
    return middle_intervals[0]


def get_missing_point(interval: tuple[int, int], difference_radius: int, position_sum: int) -> int | None:
    if interval[0] > -difference_radius or interval[1] < difference_radius:
        if interval[0] > -difference_radius:
            missing_point = interval[0] - 1
            if (missing_point - position_sum) % 2 != 0:
                missing_point -= 1
        else:
            missing_point = interval[1] + 1
            if (missing_point - position_sum) % 2 != 0:
                missing_point += 1
        return missing_point


def get_next_sum_to_check(
    constraints: Iterable[tuple[int, int, int, int]],
    position_sum: int,
    interval: tuple[int, int]
) -> int:
    min_max_sum = min(c for _, c, _, _ in constraints)
    if position_sum < 4_000_000:
        middle_radius = min(-interval[0], interval[1])
        return min(min_max_sum, middle_radius, 4_000_000) + 1
    else:
        return min_max_sum + 1


def get_joined_intervals(intervals: Iterable[tuple[int, int]]) -> list[tuple[int, int]]:
    joined_intervals = []
    for min_diff, max_diff in sorted(intervals):
        if not joined_intervals:
            joined_intervals.append((min_diff, max_diff))
        else:
            last_interval = joined_intervals[-1]
            if min_diff > last_interval[1] + 1:
                joined_intervals.append((min_diff, max_diff))
            elif max_diff > last_interval[1]:
                joined_intervals[-1] = (last_interval[0], max_diff)
    return joined_intervals


def check_sums(constraints: Iterable[tuple[int, int, int, int]]) -> tuple[int, int]:
    constraints = list(constraints)
    position_sum = 0
    while True:
        excluded, new_position_sum = check_sum(position_sum, constraints)
        if not excluded:
            position_diff = new_position_sum
            return (position_sum + position_diff) // 2, (position_sum - position_diff) // 2
        else:
            assert new_position_sum <= 8_000_000
            position_sum = new_position_sum


def main():
    sensors_beacons = list(parse_input(line.strip() for line in sys.stdin))
    print(impossible_beacon_locations(sensors_beacons, 2_000_000))
    found_x, found_y = check_sums(get_constraints(sensors_beacons))
    print(4_000_000 * found_x + found_y)


if __name__ == '__main__':
    main()
