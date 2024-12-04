import sys
from typing import Iterator


def count_xmas(s: str) -> int:
    return s.count("XMAS") + s.count("SAMX")


def columns(rows: list[str]) -> list[str]:
    return list(''.join(col) for col in zip(*rows))


def diagonals(rows: list[str]) -> Iterator[str]:
    # assuming a square
    size = len(rows)
    for x0 in range(size):
        yield ''.join(rows[i][x0 + i] for i in range(size - x0))
        yield ''.join(rows[size - 1 - i][x0 + i] for i in range(size - x0))
    for y0 in range(1, size - 1):
        yield ''.join(rows[y0 + i][i] for i in range(size - y0))
        yield ''.join(rows[y0 - i][i] for i in range(y0 + 1))


def count_all_xmas(rows: list[str]) -> int:
    return (
        sum(count_xmas(row) for row in rows)
        + sum(count_xmas(column) for column in columns(rows))
        + sum(count_xmas(diagonal) for diagonal in diagonals(rows))
    )


def count_all_x_mas(rows: list[str]) -> int:
    size = len(rows)
    count = 0
    for x in range(1, size - 1):
        for y in range(1, size - 1):
            if (
                rows[y][x] == 'A'
                and rows[y-1][x-1] + rows[y+1][x+1] in ('MS', 'SM')
                and rows[y-1][x+1] + rows[y+1][x-1] in ('MS', 'SM')
            ):
                count += 1
    return count


def main():
    words_earch = list(line.strip() for line in sys.stdin)
    print(count_all_xmas(words_earch))
    print(count_all_x_mas(words_earch))


if __name__ == '__main__':
    main()
