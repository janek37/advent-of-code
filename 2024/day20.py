import sys
from typing import Iterator, NamedTuple

type Maze = list[str]


class Position(NamedTuple):
    x: int
    y: int


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


def find_cheats(track: dict[Position, int], max_cheat_length: int) -> Iterator[int]:
    for cheat_start, i in track.items():
        for dx in range(-max_cheat_length, max_cheat_length + 1):
            for dy in range(-(max_cheat_length - abs(dx)), max_cheat_length - abs(dx) + 1):
                cheat_end = Position(cheat_start.x + dx, cheat_start.y + dy)
                if cheat_end in track:
                    yield track.get(cheat_end, 0) - i - (abs(dx) + abs(dy))


def maze_to_track(maze: Maze, start: Position, end: Position) -> dict[Position, int]:
    track = {}
    current = start
    i = 0
    while True:
        track[current] = i
        if current == end:
            break
        for neighbor in neighbors(maze, current):
            if neighbor not in track:
                current = neighbor
                break
        i += 1
    return track


def neighbors(maze: Maze, pos: Position) -> Iterator[Position]:
    for dx, dy in [(-1, 0), (1, 0), (0, -1), (0, 1)]:
        npos = Position(pos.x + dx, pos.y + dy)
        if at_pos(maze, npos) != '#':
            yield npos


def at_pos(maze: Maze, pos: Position) -> str | None:
    if 0 <= pos.x < len(maze[0]) and 0 <= pos.y < len(maze):
        return maze[pos.y][pos.x]


def main():
    maze, start, end = list(parse_input(sys.stdin))
    track = maze_to_track(maze, start, end)
    print(sum(1 for save in find_cheats(track, max_cheat_length=2) if save >= 100))
    print(sum(1 for save in find_cheats(track, max_cheat_length=20) if save >= 100))


if __name__ == '__main__':
    main()
