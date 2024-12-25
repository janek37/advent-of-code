import sys
from typing import Iterator


type Heights = tuple[int, int, int, int, int]


def parse_input(lines: Iterator[str]) -> tuple[list[Heights], list[Heights]]:
    schematic = []
    locks = []
    keys = []
    for line in lines:
        line = line.strip()
        if line:
            schematic.append(line)
            if len(schematic) == 7:
                heights = tuple(column.count(column[0]) for column in zip(*schematic))
                (keys if schematic[0][0] == "." else locks).append(heights)
                schematic = []
    return locks, keys


def check(lock: Heights, key: Heights) -> bool:
    return all(lock_height <= key_height for lock_height, key_height in zip(lock, key))


def main():
    locks, keys = parse_input(sys.stdin)
    print(sum(1 for lock in locks for key in keys if check(lock, key)))


if __name__ == '__main__':
    main()
