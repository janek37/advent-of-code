import re
import sys
from typing import Iterable, Tuple

RANGE_REGEX = re.compile(r'(\d+)-(\d+)')

Range = Tuple[int, int]


def parse_range(s: str) -> Range:
    g1, g2 = RANGE_REGEX.match(s).groups()
    return int(g1), int(g2)


def parse_input(lines: Iterable[str]) -> Iterable[Tuple[Range, Range]]:
    for line in lines:
        yield tuple(parse_range(part) for part in line.split(','))


def contains(range1: Range, range2: Range) -> bool:
    return range1[0] <= range2[0] and range1[1] >= range2[1]


def overlaps(range1: Range, range2: Range) -> bool:
    r1, r2 = sorted([range1, range2])
    return r2[0] <= r1[1]


def main():
    range_pairs = list(parse_input(sys.stdin))
    print(sum(1 for range1, range2 in range_pairs if contains(range1, range2) or contains(range2, range1)))
    print(sum(1 for range1, range2 in range_pairs if overlaps(range1, range2)))


if __name__ == '__main__':
    main()
