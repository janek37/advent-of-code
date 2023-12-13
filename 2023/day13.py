import sys
from collections.abc import Iterator


def parse_input(lines: Iterator[str]) -> Iterator[list[str]]:
    pattern = []
    for line in lines:
        if line:
            pattern.append(line)
        else:
            yield pattern
            pattern = []
    if pattern:
        yield pattern


def find_mirror_position(pattern: list[str]) -> int | None:
    for i in range(1, len(pattern)):
        for n, row in enumerate(pattern):
            if n == i:
                return i
            mirrored = i * 2 - 1 - n
            if mirrored < len(pattern) and row != pattern[mirrored]:
                break


def find_smudged_mirror_position(pattern: list[str]) -> int | None:
    for i in range(1, len(pattern)):
        diff_count = 0
        for n, row in enumerate(pattern):
            if n == i and diff_count == 1:
                return i
            mirrored = i * 2 - 1 - n
            if mirrored < len(pattern):
                diff_count += sum(1 for char1, char2 in zip(row, pattern[mirrored]) if char1 != char2)
                if diff_count > 1:
                    break


def diagonal_flip(pattern: list[str]) -> list[str]:
    return [''.join(chars) for chars in zip(*pattern)]


def get_mirror_summary(pattern: list[str]):
    horizontal = find_mirror_position(pattern)
    if horizontal is not None:
        return 100 * horizontal
    return find_mirror_position(diagonal_flip(pattern))


def get_smudged_mirror_summary(pattern: list[str]):
    horizontal = find_smudged_mirror_position(pattern)
    if horizontal is not None:
        return 100 * horizontal
    return find_smudged_mirror_position(diagonal_flip(pattern))


def main():
    patterns = list(parse_input(line.rstrip('\n') for line in sys.stdin))
    print(sum(get_mirror_summary(pattern) for pattern in patterns))
    print(sum(get_smudged_mirror_summary(pattern) for pattern in patterns))


if __name__ == '__main__':
    main()
