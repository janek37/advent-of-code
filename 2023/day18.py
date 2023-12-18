import sys
from collections.abc import Iterator

from typing import NamedTuple


class Direction(NamedTuple):
    dx: int
    dy: int


DIRECTIONS = {
    'U': Direction(0, -1),
    'D': Direction(0, 1),
    'L': Direction(-1, 0),
    'R': Direction(1, 0),
}


def parse_input(lines: Iterator[str]) -> Iterator[tuple[tuple[Direction, int], tuple[Direction, int]]]:
    for line in lines:
        d, length, color = line.split()
        length2 = color[2:-2]
        d2 = 'RDLU'[int(color[-2])]
        yield (DIRECTIONS[d], int(length)), (DIRECTIONS[d2], int(length2, 16))


def get_vertices(trench_data: list[tuple[Direction, int]]) -> Iterator[tuple[int, int]]:
    coord = 0, 0
    yield coord
    for direction, length in trench_data:
        coord = coord[0] + direction.dx * length, coord[1] + direction.dy * length
        yield coord


def get_area(trench_data: list[tuple[Direction, int]]) -> int:
    vertices = list(get_vertices(trench_data))
    length = sum(l for d, l in trench_data)
    shoelace_area = abs(sum(
        (v1[0] + v2[0])*(v1[1] - v2[1]) / 2
        for v1, v2 in zip(vertices, vertices[1:])
    ))
    return int(shoelace_area + length/2 + 1)


def main():
    trench_data = list(parse_input(line.rstrip('\n') for line in sys.stdin))
    trench1 = [d[0] for d in trench_data]
    print(get_area(trench1))
    trench2 = [d[1] for d in trench_data]
    print(get_area(trench2))


if __name__ == '__main__':
    main()
