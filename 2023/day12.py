import sys
from collections.abc import Iterator
from functools import cache


def parse_input(lines: Iterator[str]) -> Iterator[tuple[str, tuple[int, ...]]]:
    for line in lines:
        row, groups = line.split()
        yield row, tuple(map(int, groups.split(',')))


@cache
def count_arrangements(row: str, groups: tuple[int, ...]) -> int:
    first = groups[0]
    rest_length = sum(groups[1:]) + len(groups) - 1
    count = 0
    for first_position in range(len(row) - rest_length - first + 1):
        prefix = '.' * first_position + '#' * first + '.'
        if is_possible_prefix(prefix, row):
            if len(groups) == 1:
                if all(char != '#' for char in row[len(prefix):]):
                    count += 1
            else:
                count += count_arrangements(row[len(prefix):], groups[1:])
    return count


def is_possible_prefix(prefix: str, row: str) -> bool:
    if len(prefix) > len(row):
        if any(char == '#' for char in prefix[len(row):]):
            return False
    return all(char1 == char2 or char2 == '?' for char1, char2 in zip(prefix, row))


def main():
    row_data = list(parse_input(line.rstrip('\n') for line in sys.stdin))
    print(sum(count_arrangements(row, groups) for row, groups in row_data))
    unfolded_row_data = [('?'.join([row]*5), groups*5) for row, groups in row_data]
    print(sum(count_arrangements(row, groups) for row, groups in unfolded_row_data))


if __name__ == '__main__':
    main()
