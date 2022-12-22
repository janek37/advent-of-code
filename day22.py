import re
import sys
from dataclasses import dataclass
from enum import Enum
from typing import Iterable, Iterator, Callable

Path = Iterable[int | str]

Edges = list[tuple[int, int]]


def parse_input(lines: Iterator[str]) -> tuple[list[str], Path]:
    monkey_map = list(parse_map(lines))
    path = parse_path(next(lines))
    return monkey_map, path


def parse_map(lines: Iterable[str]):
    for line in lines:
        if line:
            yield line
        else:
            break


def parse_path(line: str) -> Path:
    parts: list[str] = re.findall(r'\d+|[LR]', line)
    return (int(x) if x.isdigit() else x for x in parts)


def get_edges(monkey_map: list[str]) -> tuple[Edges, Edges]:
    horizontal_edges = []
    for line in monkey_map:
        start, stop = re.search(r'[.#]+', line).span()
        horizontal_edges.append((start, stop - 1))
    vertical_edges = []
    for x in range(max(len(line) for line in monkey_map)):
        lines_with_x = [y for y, line in enumerate(monkey_map) if x < len(line) and line[x] != ' ']
        vertical_edges.append((lines_with_x[0], lines_with_x[-1]))
    return horizontal_edges, vertical_edges


class Facing(Enum):
    UP = (0, -1)
    DOWN = (0, 1)
    LEFT = (-1, 0)
    RIGHT = (1, 0)

    def rotate(self, direction: str):
        if direction == 'R':
            if self == Facing.UP:
                return Facing.RIGHT
            if self == Facing.RIGHT:
                return Facing.DOWN
            if self == Facing.DOWN:
                return Facing.LEFT
            if self == Facing.LEFT:
                return Facing.UP
        elif direction == 'L':
            if self == Facing.UP:
                return Facing.LEFT
            if self == Facing.LEFT:
                return Facing.DOWN
            if self == Facing.DOWN:
                return Facing.RIGHT
            if self == Facing.RIGHT:
                return Facing.UP

    def code(self):
        if self == Facing.RIGHT:
            return 0
        elif self == Facing.DOWN:
            return 1
        elif self == Facing.LEFT:
            return 2
        elif self == Facing.UP:
            return 3


CUBE_EDGES: list[tuple[
    int | tuple[int, int],
    int | tuple[int, int],
    Facing,
    int | Callable[[int, int], int],
    int | Callable[[int, int], int],
    Facing,
]] = [
    ((50, 99), -1, Facing.UP, 0, lambda x, y: x + 100, Facing.RIGHT),
    ((100, 149), -1, Facing.UP, lambda x, y: x - 100, 199, Facing.UP),
    (49, (0, 49), Facing.LEFT, 0, lambda x, y: 149 - y, Facing.RIGHT),
    (150, (0, 49), Facing.RIGHT, 99, lambda x, y: 149 - y, Facing.LEFT),
    ((100, 149), 50, Facing.DOWN, 99, lambda x, y: x - 50, Facing.LEFT),
    (49, (50, 99), Facing.LEFT, lambda x, y: y - 50, 100, Facing.DOWN),
    (100, (50, 99), Facing.RIGHT, lambda x, y: y + 50, 49, Facing.UP),
    ((0, 49), 99, Facing.UP, 50, lambda x, y: x + 50, Facing.RIGHT),
    (-1, (100, 149), Facing.LEFT, 50, lambda x, y: 149 - y, Facing.RIGHT),
    (100, (100, 149), Facing.RIGHT, 149, lambda x, y: 149 - y, Facing.LEFT),
    ((50, 99), 150, Facing.DOWN, 49, lambda x, y: x + 100, Facing.LEFT),
    (-1, (150, 199), Facing.LEFT, lambda x, y: y - 100, 0, Facing.DOWN),
    (50, (150, 199), Facing.RIGHT, lambda x, y: y - 100, 149, Facing.UP),
    ((0, 49), 200, Facing.DOWN, lambda x, y: x + 100, 0, Facing.DOWN),
]


@dataclass
class Mover:
    monkey_map: list[str]
    vertical_edges: Edges
    horizontal_edges: Edges

    def move(self, position: tuple[int, int], facing: Facing) -> tuple[int, int]:
        x, y = position
        delta_x, delta_y = facing.value
        horizontal_edge_x1, horizontal_edge_x2 = self.horizontal_edges[y]
        vertical_edge_y1, vertical_edge_y2 = self.vertical_edges[x]
        new_x = (x + delta_x - horizontal_edge_x1) % (horizontal_edge_x2 - horizontal_edge_x1 + 1) + horizontal_edge_x1
        new_y = (y + delta_y - vertical_edge_y1) % (vertical_edge_y2 - vertical_edge_y1 + 1) + vertical_edge_y1
        if self.monkey_map[new_y][new_x] == '#':
            return position
        else:
            assert self.monkey_map[new_y][new_x] == '.'
            return new_x, new_y

    def move_on_cube(self, position: tuple[int, int], facing: Facing) -> tuple[tuple[int, int], Facing]:
        old_facing = facing
        x, y = position
        delta_x, delta_y = facing.value
        new_x, new_y = x + delta_x, y + delta_y
        if not (
            0 <= new_y < len(self.monkey_map)
            and 0 <= new_x < len(self.monkey_map[new_y])
            and self.monkey_map[new_y][new_x] != ' '
        ):
            for old_x, old_y, older_facing, get_new_x, get_new_y, new_facing in CUBE_EDGES:
                if older_facing != old_facing:
                    continue
                if isinstance(old_x, int) and new_x != old_x:
                    continue
                if isinstance(old_y, int) and new_y != old_y:
                    continue
                if isinstance(old_x, tuple) and not (old_x[0] <= new_x <= old_x[1]):
                    continue
                if isinstance(old_y, tuple) and not (old_y[0] <= new_y <= old_y[1]):
                    continue
                new_x, new_y = (
                    (get_new_x if isinstance(get_new_x, int) else get_new_x(new_x, new_y)),
                    (get_new_y if isinstance(get_new_y, int) else get_new_y(new_x, new_y)),
                )
                facing = new_facing
                break
        if self.monkey_map[new_y][new_x] == '#':
            return position, old_facing
        else:
            assert self.monkey_map[new_y][new_x] == '.'
            return (new_x, new_y), facing


def use_path(path: Path, mover: Mover, on_cube: bool = False) -> tuple[tuple[int, int], Facing]:
    position = (mover.horizontal_edges[0][0], 0)
    facing = Facing.RIGHT
    for path_item in path:
        if isinstance(path_item, int):
            for i in range(path_item):
                if on_cube:
                    position, facing = mover.move_on_cube(position, facing)
                else:
                    position = mover.move(position, facing)
        else:
            facing = facing.rotate(path_item)
    return position, facing


def get_password(position, facing):
    x, y = position
    return 1000 * (y + 1) + 4 * (x + 1) + facing.code()


def main():
    monkey_map, path = parse_input(line.rstrip('\n') for line in sys.stdin)
    path = list(path)
    horizontal_edges, vertical_edges = get_edges(monkey_map)
    mover = Mover(monkey_map, vertical_edges, horizontal_edges)
    position, facing = use_path(path, mover)
    print(get_password(position, facing))
    position, facing = use_path(path, mover, on_cube=True)
    print(get_password(position, facing))


if __name__ == '__main__':
    main()
