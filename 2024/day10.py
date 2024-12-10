import sys
from enum import Enum
from typing import Iterator


class Direction(tuple[int, int], Enum):
    LEFT = (-1, 0)
    RIGHT = (1, 0)
    UP = (0, -1)
    DOWN = (0, 1)


def find_trailheads(topo_map: list[list[int]]) -> Iterator[tuple[int, int]]:
    for x, row in enumerate(topo_map):
        for y, height in enumerate(row):
            if height == 0:
                yield x, y


def find_trailhead_score(topo_map: list[list[int]], trailhead: tuple[int, int]) -> int:
    positions = {trailhead}
    height = 0
    while height != 9:
        new_positions = set()
        for position in positions:
            new_positions.update(find_next_steps(topo_map, position))
        positions = new_positions
        height += 1
    return len(positions)


def find_trailhead_ratings(topo_map: list[list[int]]) -> Iterator[int]:
    ratings = {}
    for height in range(9, -1, -1):
        for x, row in enumerate(topo_map):
            for y, h in enumerate(row):
                if h == height:
                    if height == 9:
                        ratings[(x, y)] = 1
                    else:
                        ratings[(x, y)] = sum(ratings[next_step] for next_step in find_next_steps(topo_map, (x, y)))
                        if height == 0:
                            yield ratings[(x, y)]


def find_next_steps(topo_map: list[list[int]], position: tuple[int, int]) -> Iterator[tuple[int, int]]:
    x, y = position
    for direction in Direction:
        neighbor_x, neighbor_y = x + direction.value[0], y + direction.value[1]
        if 0 <= neighbor_x < len(topo_map) and 0 <= neighbor_y < len(topo_map[0]):
            if topo_map[neighbor_x][neighbor_y] == topo_map[x][y] + 1:
                yield neighbor_x, neighbor_y


def main():
    topo_map = list(list(map(int, line.strip())) for line in sys.stdin)
    print(sum(find_trailhead_score(topo_map, trailhead) for trailhead in find_trailheads(topo_map)))
    print(sum(find_trailhead_ratings(topo_map)))


if __name__ == '__main__':
    main()
