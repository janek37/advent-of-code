import sys
from collections import defaultdict
from collections.abc import Iterator
from typing import NamedTuple


class Position(NamedTuple):
    x: int
    y: int


class BigPosition(NamedTuple):
    position: Position
    big_x: int
    big_y: int


def parse_input(lines: Iterator[str]) -> tuple[list[str], Position]:
    garden = []
    start = None
    for y, line in enumerate(lines):
        garden.append(line.replace('S', '.'))
        if 'S' in line:
            start = Position(line.index('S'), y)
    return garden, start


def get_graph(garden: list[str]) -> dict[Position, list[Position]]:
    width = len(garden[0])
    height = len(garden[1])
    graph = {}
    for y, row in enumerate(garden):
        for x, plot in enumerate(row):
            if plot == '.':
                neighbors = []
                for dx, dy in [(-1, 0), (1, 0), (0, -1), (0, 1)]:
                    x1 = x + dx
                    y1 = y + dy
                    if 0 <= x1 < width and 0 <= y1 < height and garden[y1][x1] == '.':
                        neighbors.append(Position(x1, y1))
                graph[Position(x, y)] = neighbors
    return graph


def get_destination_count(graph: dict[Position, list[Position]], start: Position, steps: int) -> int:
    position_set = {start}
    for i in range(steps):
        new_set = set()
        for position in position_set:
            new_set.update(graph[position])
        position_set = new_set
    return len(position_set)


def get_big_destinations(graph: dict[Position, list[Position]], width: int, height: int, start: BigPosition, steps: int) -> set[BigPosition]:
    position_set = {start}
    for i in range(steps):
        new_set = set()
        for position, big_x, big_y in position_set:
            new_set.update([BigPosition(neighbor, big_x, big_y) for neighbor in graph[position]])
            if position.x == 0:
                new_set.add(BigPosition(Position(width - 1, position.y), big_x - 1, big_y))
            if position.x == width - 1:
                new_set.add(BigPosition(Position(0, position.y), big_x + 1, big_y))
            if position.y == 0:
                new_set.add(BigPosition(Position(position.x, height - 1), big_x, big_y - 1))
            if position.y == height - 1:
                new_set.add(BigPosition(Position(position.x, 0), big_x, big_y + 1))
        position_set = new_set
    return position_set


def get_big_positions_by_grid(big_positions: set[BigPosition]) -> dict[tuple[int, int], int]:
    counts = defaultdict(int)
    for position, big_x, big_y in big_positions:
        counts[(big_x, big_y)] += 1
    return counts


def get_big_destination_count(graph: dict[Position, list[Position]], width: int, height: int, start: Position, steps: int) -> int:
    big_start = BigPosition(start, 0, 0)
    # could be optimized by not simulating squares that are already filled
    big_positions = get_big_destinations(graph, width, height, big_start, steps % width + width*2)
    counts = get_big_positions_by_grid(big_positions)
    tip = counts[(-2, 0)] + counts[(2, 0)] + counts[(0, -2)] + counts[(0, 2)]
    edge1 = counts[(-2, -1)] + counts[(-2, 1)] + counts[(2, -1)] + counts[(2, 1)]
    edge2 = counts[(-1, -1)] + counts[(-1, 1)] + counts[(1, -1)] + counts[(1, 1)]
    center1 = counts[(0, 1)]
    center2 = counts[(0, 0)]
    num = steps // width
    return tip + edge1*num + edge2*(num - 1) + center1*num*num + center2*(num - 1)*(num - 1)


def main():
    garden, start = parse_input(line.rstrip('\n') for line in sys.stdin)
    width = len(garden[0])
    height = len(garden)
    assert width == height, "wrong assumption"
    graph = get_graph(garden)
    print(get_destination_count(graph, start, 64))
    print(get_big_destination_count(graph, width, height, start, 26501365))


if __name__ == '__main__':
    main()
