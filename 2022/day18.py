import sys
from typing import Iterable

Position = tuple[int, int, int]
Side = tuple[float, float, float]


def parse_input(lines: Iterable[str]) -> Iterable[Position]:
    for line in lines:
        yield tuple(int(x) for x in line.split(','))


def get_sides(cube: Position) -> set[Side]:
    x, y, z = cube
    return {
        (x + 0.5, y, z),
        (x - 0.5, y, z),
        (x, y + 0.5, z),
        (x, y - 0.5, z),
        (x, y, z + 0.5),
        (x, y, z - 0.5),
    }


def get_adjacent_cubes(cube: Position) -> list[Position]:
    x, y, z = cube
    return [
        (x + 1, y, z),
        (x - 1, y, z),
        (x, y + 1, z),
        (x, y - 1, z),
        (x, y, z + 1),
        (x, y, z - 1),
    ]


def get_visible_sides(cubes: Iterable[Position]) -> set[Position]:
    visible_sides = set()
    for cube in cubes:
        cube_sides = get_sides(cube)
        visible_sides ^= cube_sides
    return visible_sides


def get_outer_side_count(cubes: list[Position]) -> int:
    x_range, y_range, z_range = get_ranges(cubes)
    steam = get_initial_steam(x_range, y_range, z_range)
    cubes_set = set(cubes)
    outer_side_count = 0
    positions_to_fill = [(x_range.start, y_range.start, z_range.start)]
    while positions_to_fill:
        position = positions_to_fill.pop()
        if position not in cubes_set and position not in steam:
            adjacent_cubes = get_adjacent_cubes(position)
            if any(cube in steam for cube in adjacent_cubes):
                steam.add(position)
                outer_side_count += sum(1 for cube in adjacent_cubes if cube in cubes_set)
            positions_to_fill.extend(adjacent_cubes)
    return outer_side_count


def get_ranges(cubes: list[Position]) -> tuple[range, range, range]:
    min_x = min(x for x, y, z in cubes)
    max_x = max(x for x, y, z in cubes)
    min_y = min(y for x, y, z in cubes)
    max_y = max(y for x, y, z in cubes)
    min_z = min(z for x, y, z in cubes)
    max_z = max(z for x, y, z in cubes)
    x_range = range(min_x - 1, max_x + 2)
    y_range = range(min_y - 1, max_y + 2)
    z_range = range(min_z - 1, max_z + 2)
    return x_range, y_range, z_range


def get_initial_steam(x_range: range, y_range: range, z_range: range) -> set[Position]:
    steam = {(x_range.start - 1, y, z) for y in y_range for z in z_range}
    steam.update((x_range.stop, y, z) for y in y_range for z in z_range)
    steam.update((x, y_range.start - 1, z) for x in x_range for z in z_range)
    steam.update((x, y_range.stop, z) for x in x_range for z in z_range)
    steam.update((x, y, z_range.start - 1) for y in y_range for x in x_range)
    steam.update((x, y, z_range.stop) for y in y_range for x in x_range)
    return steam


def main():
    cubes = list(parse_input(line.strip() for line in sys.stdin))
    visible_sides = get_visible_sides(cubes)
    print(len(visible_sides))
    print(get_outer_side_count(cubes))


if __name__ == '__main__':
    main()
