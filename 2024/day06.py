import sys
from enum import Enum
from typing import Iterator, Sequence


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


JumpMap = dict[tuple[Direction, int], list[int | None]]


def make_jump_map(lab: list[str]) -> JumpMap:
    jump_map = {}
    for y, row in enumerate(lab):
        for direction in [Direction.LEFT, Direction.RIGHT]:
            jump_map[(direction, y)] = make_jumps(row, direction[0])

    for x in range(len(lab[0])):
        col = [row[x] for row in lab]
        for direction in [Direction.UP, Direction.DOWN]:
            jump_map[(direction, x)] = make_jumps(col, direction[1])

    return jump_map


def update_jump_map(lab: list[str], jump_map: JumpMap, pos: tuple[int, int]) -> JumpMap:
    new_jump_map = jump_map.copy()
    x, y = pos
    for direction in [Direction.LEFT, Direction.RIGHT]:
        new_jump_map[(direction, y)] = make_jumps(lab[y], direction[0])
    for direction in [Direction.UP, Direction.DOWN]:
        new_jump_map[(direction, x)] = make_jumps([row[x] for row in lab], direction[1])
    return new_jump_map


def make_jumps(line: Sequence[str], direction: int) -> list[int | None]:
    jumps = [None] * len(line)
    last_dest = None
    was_wall = False
    i = len(line) - 1 if direction == 1 else 0
    while 0 <= i < len(line):
        if was_wall:
            last_dest = i
        jumps[i] = last_dest
        was_wall = line[i] == '#'
        i -= direction
    return jumps


def next_jump(jump_map: JumpMap, pos: tuple[int, int], direction: Direction) -> tuple[tuple[int, int] | None, Direction]:
    x, y = pos
    if direction in [Direction.UP, Direction.DOWN]:
        next_y = jump_map[(direction, x)][y]
        if next_y is None:
            return None, direction
        next_pos = (x, next_y)
    else:
        next_x = jump_map[(direction, y)][x]
        if next_x is None:
            return None, direction
        next_pos = (next_x, y)
    return next_pos, direction.turn_right()


def is_loop(jump_map: JumpMap, start: tuple[int, int]) -> bool:
    pos = start
    direction = Direction.UP
    visited = set()
    while pos is not None:
        if (pos, direction) in visited:
            return True
        visited.add((pos, direction))
        pos, direction = next_jump(jump_map, pos, direction)
    return False


def find_loops(lab: list[str], start: tuple[int, int]) -> Iterator[tuple[int, int]]:
    candidates = simulate_guard(lab, start)
    candidates.remove(start)
    jump_map = make_jump_map(lab)
    for pos in candidates:
        x, y = pos
        new_lab = list(lab)
        new_lab[y] = lab[y][:x] + '#' + lab[y][x+1:]
        assert new_lab[y][x] == '#'
        new_jump_map = update_jump_map(new_lab, jump_map, pos)
        if is_loop(new_jump_map, start):
            yield pos


def main():
    lab, start = parse_input(sys.stdin)
    print(len(simulate_guard(lab, start)))
    print(len(list(find_loops(lab, start))))


if __name__ == '__main__':
    main()
