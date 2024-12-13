import sys
from enum import Enum
from typing import NamedTuple, Iterator


class Position(NamedTuple):
    x: int
    y: int


class Orientation(Enum):
    TOP = (0, -1)
    BOTTOM = (0, 1)
    LEFT = (-1, 0)
    RIGHT = (1, 0)


class Edge(NamedTuple):
    orientation: Orientation
    inside_position: Position

    def continuations(self) -> "tuple[Edge, Edge]":
        if self.orientation in (Orientation.TOP, Orientation.BOTTOM):
            return (
                Edge(self.orientation, Position(self.inside_position.x - 1, self.inside_position.y)),
                Edge(self.orientation, Position(self.inside_position.x + 1, self.inside_position.y))
            )
        else:
            return (
                Edge(self.orientation, Position(self.inside_position.x, self.inside_position.y - 1)),
                Edge(self.orientation, Position(self.inside_position.x, self.inside_position.y + 1)),
            )


def find_fence_prices(garden: list[str]) -> Iterator[int]:
    for region in find_regions(garden):
        yield len(region) * sum(1 for edge in find_edges(region))


def find_discounted_prices(garden: list[str]) -> Iterator[int]:
    for region in find_regions(garden):
        edges = list(find_edges(region))
        yield len(region) * sum(1 for side in find_sides(edges))


def find_regions(garden: list[str]) -> Iterator[set[Position]]:
    unassigned = {Position(x, y) for x in range(len(garden[0])) for y in range(len(garden))}
    while unassigned:
        position = next(iter(unassigned))
        region = flood_fill(garden, position)
        unassigned -= region
        yield region


def find_edges(region: set[Position]) -> Iterator[Edge]:
    return (Edge(Orientation((n.x - p.x, n.y - p.y)), p) for p in region for n in neighbors(p) if n not in region)


def find_sides(edges: list[Edge]) -> Iterator[set[Position]]:
    unassigned = set(edges)
    while unassigned:
        start_edge = next(iter(unassigned))
        side = set()
        stack = [start_edge]
        while stack:
            edge = stack.pop()
            if edge not in side:
                side.add(edge)
                for continuation in edge.continuations():
                    if continuation in unassigned:
                        stack.append(continuation)
        unassigned -= side
        yield side


def flood_fill(garden: list[str], start: Position) -> set[Position]:
    visited = set()
    stack = [start]
    while stack:
        position = stack.pop()
        if position not in visited:
            visited.add(position)
            for neighbor in neighbors(position, garden):
                if at_pos(garden, neighbor) == at_pos(garden, start):
                    stack.append(neighbor)
    return visited


def at_pos(garden: list[str], pos: Position) -> str | None:
    if 0 <= pos.x < len(garden[0]) and 0 <= pos.y < len(garden):
        return garden[pos.y][pos.x]


def neighbors(pos: Position, garden: list[str] | None = None) -> Iterator[Position]:
    for dx, dy in [(-1, 0), (1, 0), (0, -1), (0, 1)]:
        npos = Position(pos.x + dx, pos.y + dy)
        if garden is None or at_pos(garden, npos) is not None:
            yield npos


def main():
    garden = list(line.strip() for line in sys.stdin)
    print(sum(find_fence_prices(garden)))
    print(sum(find_discounted_prices(garden)))


if __name__ == '__main__':
    main()
