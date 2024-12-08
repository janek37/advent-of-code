import math
import sys
from collections import defaultdict
from itertools import combinations
from string import ascii_letters, digits
from typing import Iterator

ANTENNA_SYMBOLS = set(ascii_letters + digits)


def get_antennae_groups(rows: list[str]) -> list[list[tuple[int, int]]]:
    antennae_by_freq = defaultdict(list)
    for x, row in enumerate(rows):
        for y, ch in enumerate(row):
            if ch in ANTENNA_SYMBOLS:
                antennae_by_freq[ch].append((x, y))
    return list(antennae_by_freq.values())


def find_antinodes(antenna1: tuple[int, int], antenna2: tuple[int, int], width: int, height: int) -> Iterator[tuple[int, int]]:
    x1, y1 = antenna1
    x2, y2 = antenna2
    antinode_pair = [(x2 * 2 - x1, y2 * 2 - y1), (x1 * 2 - x2, y1 * 2 - y2)]
    for antinode in antinode_pair:
        if 0 <= antinode[0] < width and 0 <= antinode[1] < height:
            yield antinode


def find_all_antinodes(antennae_groups: list[list[tuple[int, int]]], width: int, height: int) -> set[tuple[int, int]]:
    antinodes = set()
    for group in antennae_groups:
        for a1, a2 in combinations(group, 2):
            antinodes |= set(find_antinodes(a1, a2, width, height))
    return antinodes


def find_antinodes2(antenna1: tuple[int, int], antenna2: tuple[int, int], width: int, height: int) -> Iterator[tuple[int, int]]:
    x1, y1 = antenna1
    x2, y2 = antenna2
    gcd = math.gcd(abs(x2 - x1), abs(y2 - y1))
    step = (x2 - x1)//gcd, (y2 - y1)//gcd
    for mult in range(0, 1000):
        antinode = x1 + mult * step[0], y1 + mult * step[1]
        if 0 <= antinode[0] < width and 0 <= antinode[1] < height:
            yield antinode
        else:
            break
    for mult in range(-1, -1000, -1):
        antinode = x1 + mult * step[0], y1 + mult * step[1]
        if 0 <= antinode[0] < width and 0 <= antinode[1] < height:
            yield antinode
        else:
            break


def find_all_antinodes2(antennae_groups: list[list[tuple[int, int]]], width: int, height: int) -> set[tuple[int, int]]:
    antinodes = set()
    for group in antennae_groups:
        for a1, a2 in combinations(group, 2):
            antinodes |= set(find_antinodes2(a1, a2, width, height))
    return antinodes


def main():
    rows = list(line.strip() for line in sys.stdin)
    antennae_groups = get_antennae_groups(rows)
    print(len(find_all_antinodes(antennae_groups, len(rows), len(rows[0]))))
    print(len(find_all_antinodes2(antennae_groups, len(rows), len(rows[0]))))


if __name__ == '__main__':
    main()
