import sys
from collections.abc import Iterator


def parse_input(lines: Iterator[str]) -> Iterator[list[int]]:
    for line in lines:
        yield list(map(int, line.split()))


def extrapolate(sequence: list[int]) -> int:
    if all(n == 0 for n in sequence):
        return 0
    return sequence[-1] + extrapolate(get_deltas(sequence))


def get_deltas(sequence: list[int]) -> list[int]:
    return [elem2 - elem1 for elem1, elem2 in zip(sequence, sequence[1:])]


def main():
    sequences = list(parse_input(line.rstrip('\n') for line in sys.stdin))
    print(sum(extrapolate(sequence) for sequence in sequences))
    print(sum(extrapolate(sequence[::-1]) for sequence in sequences))


if __name__ == '__main__':
    main()
