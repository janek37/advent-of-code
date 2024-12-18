import re
import sys
from collections import deque
from typing import Iterator


WIDTH = 71
HEIGHT = 71


def parse_input(lines: Iterator[str]) -> Iterator[tuple[int, int]]:
    for line in lines:
        yield tuple(map(int, re.findall(r'\d+', line)))


def find_shortes_path(corrupt_locations: set[tuple[int, int]]) -> int | None:
    # bfs
    queue = deque([((0, 0), 0)])
    visited = set()
    while queue:
        position, steps = queue.popleft()
        if position == (WIDTH - 1, HEIGHT - 1):
            return steps
        if position not in visited:
            visited.add(position)
            for neighbor in neighbors(position, corrupt_locations):
                queue.append((neighbor, steps + 1))


def neighbors(position: tuple[int, int], walls: set[tuple[int, int]]) -> Iterator[tuple[int, int]]:
    x, y = position
    for dx, dy in [(-1, 0), (1, 0), (0, -1), (0, 1)]:
        nx, ny = x + dx, y + dy
        if 0 <= nx < WIDTH and 0 <= ny < HEIGHT and (nx, ny) not in walls:
            yield nx, ny


def find_first_blocking_byte(corrupt_locations: list[tuple[int, int]]) -> tuple[int, int]:
    good = 1024
    bad = len(corrupt_locations)
    # binary search
    while bad > good + 1:
        new = (good + bad) // 2
        if find_shortes_path(set(corrupt_locations[:new])) is None:
            bad = new
        else:
            good = new
    return corrupt_locations[bad - 1]


def main():
    falling_bytes = list(parse_input(sys.stdin))
    print(find_shortes_path(set(falling_bytes[:1024])))
    x, y = find_first_blocking_byte(falling_bytes)
    print(f"{x},{y}")


if __name__ == '__main__':
    main()
