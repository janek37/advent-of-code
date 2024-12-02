import sys
from typing import Iterator


def parse_input(lines: Iterator[str]) -> Iterator[list[int]]:
    for line in lines:
        yield [int(part) for part in line.split()]


def is_safe(report: list[int]) -> bool:
    differences = [next_level - prev_level for prev_level, next_level in zip(report[:-1], report[1:])]
    return all(d in (1, 2, 3) for d in differences) or all(d in (-1, -2, -3) for d in differences)


def is_almost_safe(report: list[int]) -> bool:
    return is_safe(report) or any(is_safe(report[:i] + report[i + 1:]) for i in range(len(report)))


def main():
    reports = list(parse_input(sys.stdin))
    print(sum(1 for report in reports if is_safe(report)))
    print(sum(1 for report in reports if is_almost_safe(report)))


if __name__ == '__main__':
    main()
