import sys
from collections.abc import Iterator


PIPE_TYPES = {
    '|': {(0, -1), (0, 1)},
    'J': {(0, -1), (-1, 0)},
    'L': {(0, -1), (1, 0)},
    '7': {(0, 1), (-1, 0)},
    'F': {(0, 1), (1, 0)},
    '-': {(-1, 0), (1, 0)},
}

PipeMap = list[list[set[tuple[int, int]] | None]]


def parse_input(lines: Iterator[str]) -> tuple[PipeMap, tuple[int, int]]:
    pipe_map = []
    start = None
    for i, line in enumerate(lines):
        pipe_map.append([PIPE_TYPES.get(char) for char in line])
        if 'S' in line:
            start = (line.index('S'), i)
    return pipe_map, start


def find_loop(pipe_map: PipeMap, start: tuple[int, int]) -> list[tuple[int, int]]:
    for direction in [(-1, 0), (1, 0), (0, -1), (0, 1)]:
        if loop := find_loop_in_direction(pipe_map, start, direction):
            return loop


def find_loop_in_direction(
    pipe_map: PipeMap, start: tuple[int, int], direction: tuple[int, int]
) -> list[tuple[int, int]] | None:
    loop = [start]
    dir_x, dir_y = direction
    loc_x, loc_y = (start[0] + dir_x, start[1] + dir_y)
    while (loc_x, loc_y) != start:
        loop.append((loc_x, loc_y))
        pipe = pipe_map[loc_y][loc_x]
        if not pipe or (-dir_x, -dir_y) not in pipe:
            return
        dir_x, dir_y = list(pipe - {(-dir_x, -dir_y)})[0]
        loc_x, loc_y = loc_x + dir_x, loc_y + dir_y
    return loop


RIGHT_TURN = {
    (0, -1): (1, 0),
    (0, 1): (-1, 0),
    (-1, 0): (0, -1),
    (1, 0): (0, 1),
}
LEFT_TURN = {
    (0, -1): (-1, 0),
    (0, 1): (1, 0),
    (-1, 0): (0, 1),
    (1, 0): (0, -1),
}


def get_inside_area(pipe_map: PipeMap, loop: list[tuple[int, int]]) -> int:
    left_side = set()
    right_side = set()
    loop_set = set(loop)
    for location, previous_location, next_location in zip(loop, loop[-1:] + loop[:-1], loop[1:] + loop[:1]):
        prev_direction = location[0] - previous_location[0], location[1] - previous_location[1]
        next_direction = next_location[0] - location[0], next_location[1] - location[1]
        for direction in (prev_direction, next_direction):
            for turn, side in [(LEFT_TURN, left_side), (RIGHT_TURN, right_side)]:
                side_direction = turn[direction]
                side_location = location[0] + side_direction[0], location[1] + side_direction[1]
                if side_location not in loop_set and side_location not in side:
                    side.update(fill_area(pipe_map, loop_set, side_location))
    width = len(pipe_map[0])
    height = len(pipe_map)
    edge = (
        {(0, y) for y in range(height)}
        | {(width - 1, y) for y in range(height)}
        | {(x, 0) for x in range(width)}
        | {(x, height - 1) for x in range(width)}
    )
    # This solution assumes that the loop does not contain the whole edge
    if edge.issubset(loop_set):
        raise Exception("Incorrect assumption")
    return len(left_side) if edge & right_side else len(right_side)


def fill_area(pipe_map: PipeMap, loop: set[tuple[int, int]], start: tuple[int, int]) -> set[tuple[int, int]]:
    to_fill = [start]
    area = set()
    while to_fill:
        location = to_fill.pop()
        is_in_bounds = 0 <= location[0] < len(pipe_map[0]) and 0 <= location[1] < len(pipe_map)
        if is_in_bounds and location not in loop and location not in area:
            area.add(location)
            to_fill.extend(get_adjacent(location))
    return area


def get_adjacent(location: tuple[int, int]) -> list[tuple[int, int]]:
    x, y = location
    return [
        (x - 1, y),
        (x + 1, y),
        (x, y - 1),
        (x, y + 1),
    ]


def main():
    pipe_map, start = parse_input(line.rstrip('\n') for line in sys.stdin)
    loop = find_loop(pipe_map, start)
    print(len(loop) // 2)
    print(get_inside_area(pipe_map, loop))


if __name__ == '__main__':
    main()
