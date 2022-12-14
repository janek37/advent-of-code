import sys
from itertools import pairwise
from typing import Iterable


def parse_input(lines: Iterable[str]) -> Iterable[list[tuple[int, int]]]:
    for line in lines:
        yield [tuple(int(s) for s in pair.split(',')) for pair in line.split(' -> ')]


def get_sand_capacity(blocked_positions: set[tuple[int, int]], lowest_level: int, floor: bool = False) -> int:
    capacity = 0
    while True:
        new_sand = simulate_sand(blocked_positions, lowest_level, floor)
        if not new_sand:
            return capacity
        blocked_positions.add(new_sand)
        capacity += 1
        if new_sand == (500, 0):
            return capacity


def simulate_sand(
    blocked_positions: set[tuple[int, int]],
    lowest_level: int,
    floor: bool = False
) -> tuple[int, int] | None:
    sand_x = 500
    sand_y = 0
    while True:
        if sand_y == lowest_level and not floor:
            return
        else:
            new_positions = [(sand_x, sand_y+1), (sand_x-1, sand_y+1), (sand_x+1, sand_y+1)]
            for new_position in new_positions:
                if new_position not in blocked_positions and (not floor or new_position[1] != lowest_level+2):
                    sand_x, sand_y = new_position
                    break
            else:
                break
    return sand_x, sand_y


def get_rock_positions(rock_lines: Iterable[Iterable[tuple[int, int]]]) -> Iterable[tuple[int, int]]:
    for rock_line in rock_lines:
        for line_start, line_end in pairwise(rock_line):
            yield from get_rock_line_positions(line_start, line_end)


def get_lowest_level(rock_lines: Iterable[Iterable[tuple[int, int]]]) -> int:
    return max(line_point[1] for rock_line in rock_lines for line_point in rock_line)


def get_rock_line_positions(start: tuple[int, int], end: tuple[int, int]) -> Iterable[tuple[int, int]]:
    if start[0] == end[0]:
        for y in full_range(start[1], end[1]):
            yield start[0], y
    elif start[1] == end[1]:
        for x in full_range(start[0], end[0]):
            yield x, start[1]


def full_range(a0: int, a1: int) -> Iterable[int]:
    min_a = min(a0, a1)
    max_a = max(a0, a1)
    return range(min_a, max_a + 1)


def main():
    rock_lines = list(parse_input(line.strip() for line in sys.stdin))
    rock_positions = set(get_rock_positions(rock_lines))
    lowest_level = get_lowest_level(rock_lines)
    first_capacity = get_sand_capacity(rock_positions, lowest_level)
    print(first_capacity)
    second_capacity = get_sand_capacity(rock_positions, lowest_level, floor=True)
    print(first_capacity + second_capacity)


if __name__ == '__main__':
    main()
