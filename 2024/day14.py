import re
import sys
from typing import Iterator

WIDTH = 101
HEIGHT = 103


def parse_input(lines: Iterator[str]) -> Iterator[tuple[int, int, int, int]]:
    for line in lines:
        yield tuple(map(int, re.findall(r'-?\d+', line)))


def simulate_robot(robot: tuple[int, int, int, int], steps: int) -> tuple[int, int]:
    px, py, vx, vy = robot
    return (px + vx * steps) % WIDTH, (py + vy * steps) % HEIGHT


def count_by_quadrant(positions: Iterator[tuple[int, int]]) -> int:
    middle_x = WIDTH // 2
    middle_y = HEIGHT // 2
    quadrant_counts = [0, 0, 0, 0]
    for position in positions:
        x, y = position
        if x != middle_x and y != middle_y:
            # slight bool abuse
            quadrant_id = 2 * (x < middle_x) + (y < middle_y)
            quadrant_counts[quadrant_id] += 1
    return quadrant_counts[0] * quadrant_counts[1] * quadrant_counts[2] * quadrant_counts[3]


def find_easter_egg(robots: list[tuple[int, int, int, int]]) -> int:
    lines = [set((x, y) for x in range(WIDTH)) for y in range(HEIGHT)]
    for i in range(WIDTH * HEIGHT):
        robot_positions = set(simulate_robot(robot, i) for robot in robots)
        max_line, y = max((len(robot_positions & line), y) for y, line in enumerate(lines))
        if max_line >= 30:
            contiguous = 0
            for x in range(WIDTH):
                if (x, y) in robot_positions:
                    contiguous += 1
                else:
                    contiguous = 0
                if contiguous == 30:
                    return i


def main():
    robots = list(parse_input(sys.stdin))
    robot_positions = (simulate_robot(robot, 100) for robot in robots)
    print(count_by_quadrant(robot_positions))
    print(find_easter_egg(robots))


if __name__ == '__main__':
    main()
