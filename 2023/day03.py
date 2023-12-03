import itertools
import re
import sys
from collections.abc import Iterator
from typing import Iterable, NamedTuple


class Location(NamedTuple):
    x: int
    y: int


class Number(NamedTuple):
    value: int
    start: Location
    end: Location

    def adjacent_locations(self) -> set[Location]:
        return {
            Location(x=i, y=j)
            for i in range(self.start.x - 1, self.end.x + 1)
            for j in range(self.start.y - 1, self.start.y + 2)
        }


class Symbol(NamedTuple):
    value: str
    location: Location


def parse_input(lines: Iterable[str]) -> Iterator[tuple[Number, Symbol]]:
    lines = list(lines)
    numbers = parse_numbers(lines)
    symbols = parse_symbols(lines)
    symbols_by_location = {symbol.location: symbol for symbol in symbols}
    for number in numbers:
        for location in number.adjacent_locations() & symbols_by_location.keys():
            yield number, symbols_by_location[location]


def parse_numbers(lines: Iterable[str]) -> Iterator[Number]:
    for row, line in enumerate(lines):
        for match in re.finditer(r'\d+', line):
            value = int(match.group())
            yield Number(value, start=Location(match.start(), row), end=Location(match.end(), row))


def parse_symbols(lines: Iterable[str]) -> Iterator[Symbol]:
    for row, line in enumerate(lines):
        for match in re.finditer(r'[^.\d]', line):
            yield Symbol(match.group(), Location(x=match.start(), y=row))


def get_gear_ratios(adjacencies: list[tuple[Number, Symbol]]) -> Iterator[int]:
    numbers_by_symbol = group_numbers_by_symbol(adjacencies)
    return (
        numbers[0].value * numbers[1].value
        for symbol, numbers in numbers_by_symbol
        if symbol.value == '*' and len(numbers) == 2
    )


def group_numbers_by_symbol(adjacencies: list[tuple[Number, Symbol]]) -> list[tuple[Symbol, list[Number]]]:
    sorted_adjacencies = sorted(adjacencies, key=lambda t: t[1])
    grouped = itertools.groupby(sorted_adjacencies, key=lambda t: t[1])
    return [(symbol, [number for number, symbol in group]) for symbol, group in grouped]


def main():
    adjacencies = list(parse_input(line.rstrip('\n') for line in sys.stdin))
    print(sum(number.value for number in set(number for number, symbol in adjacencies)))
    print(sum(get_gear_ratios(adjacencies)))


if __name__ == '__main__':
    main()
