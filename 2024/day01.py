import sys
from typing import Iterator


def parse_input(lines: Iterator[str]) -> tuple[list[int], list[int]]:
    l1 = []
    l2 = []
    for line in lines:
        x1, x2 = line.split()
        l1.append(int(x1))
        l2.append(int(x2))
    return l1, l2


def calculate_distance(l1: list[int], l2: list[int]) -> int:
    return sum(abs(x1 - x2) for x1, x2 in zip(sorted(l1), sorted(l2)))


def calculate_similarity(l1: list[int], l2: list[int]) -> int:
    l1_set = set(l1)
    return sum(x for x in l2 if x in l1_set)


def main():
    l1, l2 = parse_input(sys.stdin)
    print(calculate_distance(l1, l2))
    print(calculate_similarity(l1, l2))


if __name__ == '__main__':
    main()
