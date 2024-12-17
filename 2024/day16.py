import sys
from collections import defaultdict
from enum import Enum
from heapq import heappop, heappush
from typing import Iterator, NamedTuple


class Position(NamedTuple):
    x: int
    y: int


class Orientation(Enum):
    VERTICAL = 0
    HORIZONTAL = 1

    def __lt__(self, other: "Orientation") -> bool:
        return self.value < other.value


type Maze = list[str]


type Graph = dict[tuple[Position, Orientation], list[tuple[Position, Orientation, int, set(Position)]]]


def parse_input(lines: Iterator[str]) -> tuple[Maze, Position, Position]:
    maze = []
    start = end = Position(0, 0)
    for y, line in enumerate(lines):
        line = line.strip()
        maze.append(line)
        if 'S' in line:
            start = Position(line.index('S'), y)
        if 'E' in line:
            end = Position(line.index('E'), y)
    return maze, start, end


type BestEdges = dict[tuple[Position, Orientation], list[tuple[Position, Orientation, set[Position], int]]]


def find_best_route(maze: Maze, start: Position, end: Position) -> tuple[int, set[Position]]:
    graph = make_graph(maze, start, end)
    # dijkstra
    queue: list[tuple[int, Position, Orientation | None]] = [(0, start, Orientation.HORIZONTAL)]
    best_scores = defaultdict(lambda: float('infinity'))
    best_edges: BestEdges = defaultdict(list)
    while queue:
        score, node, orientation = heappop(queue)
        if node == end:
            return score, collect_tiles(best_edges, start, end, score)
        for crossroad, far_orientation, cost, tiles in graph[(node, orientation)]:
            new_score = score + cost
            if new_score < best_scores[(crossroad, far_orientation)]:
                best_scores[(crossroad, far_orientation)] = new_score
                heappush(queue, (new_score, crossroad, far_orientation))
                best_edges[(crossroad, far_orientation)] = []
            if new_score == best_scores[(crossroad, far_orientation)]:
                best_edges[(crossroad, far_orientation)].append((node, orientation, tiles, new_score))


def collect_tiles(best_edges: BestEdges, start: Position, end: Position, score: int) -> set[Position]:
    tiles = set()
    for best_path in best_paths(best_edges, start, end, score):
        tiles |= best_path
    return tiles


def best_paths(best_edges: BestEdges, start: Position, end: Position, score: int) -> Iterator[set[Position]]:
    stack = [(end, orientation, set(), set()) for orientation in Orientation if any(edge[3] == score for edge in best_edges[(end, orientation)])]
    while stack:
        tile, orientation, tiles_so_far, path = stack.pop()
        if tile == start:
            yield tiles_so_far
        for crossroad, far_orientation, tiles, _ in best_edges[(tile, orientation)]:
            if (crossroad, far_orientation) not in path:
                stack.append((crossroad, far_orientation, tiles_so_far | tiles, path | {(crossroad, far_orientation)}))


def make_graph(maze: Maze, start: Position, end: Position) -> Graph:
    graph: Graph = defaultdict(list)
    for y, row in enumerate(maze):
        for x, char in enumerate(row):
            pos = Position(x, y)
            if char != '#' and neighbor_count(maze, pos) > 2 or pos in (start, end):
                for crossroad, cost, near_orientation, far_orientation, tiles in nearby_crossroads(maze, pos, start, end):
                    graph[(pos, near_orientation)].append((crossroad, far_orientation, cost, tiles))
                graph[(pos, Orientation.VERTICAL)].append((pos, Orientation.HORIZONTAL, 1000, {pos}))
                graph[(pos, Orientation.HORIZONTAL)].append((pos, Orientation.VERTICAL, 1000, {pos}))

    return graph


def neighbor_count(maze: Maze, pos: Position) -> int:
    return sum(1 for n in neighbors(maze, pos))


def nearby_crossroads(maze: Maze, pos: Position, start: Position, end: Position) -> Iterator[tuple[Position, int, Orientation, Orientation, set[Position]]]:
    for neighbor, orientation in neighbors(maze, pos):
        cost = 1
        path = {pos, neighbor}
        prev_orientation = orientation
        while neighbor_count(maze, neighbor) == 2 and neighbor not in (start, end):
            for next_neighbor, next_orientation in neighbors(maze, neighbor):
                if next_neighbor not in path:
                    path.add(next_neighbor)
                    cost += 1
                    if prev_orientation != next_orientation:
                        cost += 1000
                    neighbor = next_neighbor
                    prev_orientation = next_orientation
                    break
            else:
                break
        if neighbor_count(maze, neighbor) > 2 or neighbor in (start, end):
            yield neighbor, cost, orientation, prev_orientation, path


def neighbors(maze: Maze, pos: Position) -> Iterator[tuple[Position, Orientation]]:
    for dx, dy in [(-1, 0), (1, 0), (0, -1), (0, 1)]:
        npos = Position(pos.x + dx, pos.y + dy)
        if at_pos(maze, npos) != '#':
            yield npos, Orientation.HORIZONTAL if dy == 0 else Orientation.VERTICAL


def at_pos(maze: Maze, pos: Position) -> str | None:
    if 0 <= pos.x < len(maze[0]) and 0 <= pos.y < len(maze):
        return maze[pos.y][pos.x]


def main():
    maze, start, end = parse_input(sys.stdin)
    best_score, tiles = find_best_route(maze, start, end)
    print(best_score)
    print(len(tiles))


if __name__ == '__main__':
    main()
