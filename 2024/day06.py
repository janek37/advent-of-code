import sys
from enum import Enum
from typing import Iterator


class Direction(tuple[int, int], Enum):
    LEFT = (-1, 0)
    RIGHT = (1, 0)
    UP = (0, -1)
    DOWN = (0, 1)

    def turn_right(self):
        match self:
            case Direction.LEFT:
                return Direction.UP
            case Direction.UP:
                return Direction.RIGHT
            case Direction.RIGHT:
                return Direction.DOWN
            case Direction.DOWN:
                return Direction.LEFT


def parse_input(lines: Iterator[str]) -> tuple[list[str], tuple[int, int]]:
    lab = []
    start = (0, 0)
    for y, line in enumerate(lines):
        lab.append(line.strip())
        if '^' in line:
            start = (line.index('^'), y)
    return lab, start


def is_inside(lab: list[str], pos: tuple[int, int]) -> bool:
    return 0 <= pos[0] < len(lab[0]) and 0 <= pos[1] < len(lab)


def is_wall(lab: list[str], pos: tuple[int, int]) -> bool:
    return is_inside(lab, pos) and lab[pos[1]][pos[0]] == '#'


def next_step(lab: list[str], pos: tuple[int, int], direction: Direction) -> tuple[tuple[int, int], Direction]:
    next_pos = (pos[0] + direction.value[0], pos[1] + direction.value[1])
    while is_wall(lab, next_pos):
        direction = direction.turn_right()
        next_pos = (pos[0] + direction.value[0], pos[1] + direction.value[1])
    return next_pos, direction


def simulate_guard(lab: list[str], start: tuple[int, int]) -> set[tuple[int, int]]:
    pos = start
    direction = Direction.UP
    visited = set()
    while is_inside(lab, pos):
        visited.add(pos)
        pos, direction = next_step(lab, pos, direction)
    return visited


def is_loop(lab: list[str], start: tuple[int, int]) -> bool:
    pos = start
    direction = Direction.UP
    visited = set()
    while is_inside(lab, pos):
        if (pos, direction) in visited:
            return True
        visited.add((pos, direction))
        pos, direction = next_step(lab, pos, direction)
    return False


def find_loops(lab: list[str], start: tuple[int, int]) -> Iterator[tuple[int, int]]:
    candidates = simulate_guard(lab, start)
    candidates.remove(start)
    for pos in candidates:
        x, y = pos
        new_lab = list(lab)
        new_lab[y] = lab[y][:x] + '#' + lab[y][x+1:]
        assert new_lab[y][x] == '#'
        if is_loop(new_lab, start):
            yield pos


def main():
    lab, start = parse_input(sys.stdin)
    print(len(simulate_guard(lab, start)))
    print(len(list(find_loops(lab, start))))


if __name__ == '__main__':
    main()
