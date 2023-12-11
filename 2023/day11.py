import sys
from collections.abc import Iterator
from itertools import combinations


def find_galaxies(universe: list[str], expansion_rate: int) -> Iterator[tuple[int, int]]:
    empty_columns = set(find_empty_columns(universe))
    y = 0
    for row in universe:
        x = 0
        if '#' not in row:
            y += expansion_rate
            continue
        for i, char in enumerate(row):
            if char == '#':
                yield x, y
            if i in empty_columns:
                x += expansion_rate
            else:
                x += 1
        y += 1


def find_empty_columns(universe: list[str]) -> Iterator[int]:
    for i, column in enumerate(zip(*universe)):
        if all(char == '.' for char in column):
            yield i


def get_distances(galaxies: Iterator[tuple[int, int]]) -> Iterator[int]:
    return (abs(g1[0] - g2[0]) + abs(g1[1] - g2[1]) for g1, g2 in combinations(galaxies, 2))


def main():
    universe = list(line.rstrip('\n') for line in sys.stdin)
    print(sum(get_distances(find_galaxies(universe, expansion_rate=2))))
    print(sum(get_distances(find_galaxies(universe, expansion_rate=1000000))))


if __name__ == '__main__':
    main()
