import re
import sys
from dataclasses import dataclass
from enum import Enum
from typing import Iterable, Iterator

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
        x, y = self.value
        if direction == 'R':
            return Facing((-y, x))
        elif direction == 'L':
            return Facing((y, -x))
        elif direction == '2':
            return Facing((-x, -y))

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
    tuple[int, int],
    Facing,
    tuple[int, int],
    Facing,
]] = [
    ((1, 0), Facing.UP, (0, 3), Facing.RIGHT),
    ((1, 0), Facing.LEFT, (0, 2), Facing.RIGHT),
    ((2, 0), Facing.UP, (0, 3), Facing.UP),
    ((2, 0), Facing.RIGHT, (1, 2), Facing.LEFT),
    ((2, 0), Facing.DOWN, (1, 1), Facing.LEFT),
    ((1, 1), Facing.LEFT, (0, 2), Facing.DOWN),
    ((1, 2), Facing.DOWN, (0, 3), Facing.LEFT),
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
        new_position = x + delta_x, y + delta_y
        if self.get_tile(new_position) is None:
            for old_face, old_facing, new_face, new_facing in CUBE_EDGES:
                if not (old_facing == facing and (x // 50, y // 50) == old_face):
                    old_facing, new_facing = new_facing.rotate('2'), old_facing.rotate('2')
                    old_face, new_face = new_face, old_face
                if old_facing == facing and (x // 50, y // 50) == old_face:
                    normal_x, normal_y = x - 50 * old_face[0], y - 50 * old_face[1]
                    temp_facing = old_facing
                    while temp_facing != new_facing:
                        temp_facing = temp_facing.rotate('R')
                        normal_x, normal_y = 49 - normal_y, normal_x
                    new_facing_x, new_facing_y = new_facing.value
                    new_position = (
                        normal_x + 50 * (new_face[0] - new_facing_x) + new_facing_x,
                        normal_y + 50 * (new_face[1] - new_facing_y) + new_facing_y,
                    )
                    facing = new_facing
                    break
        if self.get_tile(new_position) == '#':
            return position, old_facing
        else:
            return new_position, facing

    def get_tile(self, position: tuple[int, int]) -> str | None:
        x, y = position
        if 0 <= y < len(self.monkey_map) and 0 <= x < len(self.monkey_map[y]):
            tile = self.monkey_map[y][x]
            if tile != ' ':
                return tile


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
