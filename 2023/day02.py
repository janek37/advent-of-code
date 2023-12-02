import sys
from collections.abc import Iterator
from dataclasses import dataclass
from typing import Iterable


@dataclass
class CubeSet:
    red: int = 0
    green: int = 0
    blue: int = 0

    def is_subset(self, other: "CubeSet") -> bool:
        return self.red <= other.red and self.green <= other.green and self.blue <= other.blue

    def __or__(self, other: "CubeSet") -> "CubeSet":
        return CubeSet(
            red=max(self.red, other.red),
            green=max(self.green, other.green),
            blue=max(self.blue, other.blue),
        )

    def power(self) -> int:
        return self.red * self.green * self.blue


@dataclass
class Game:
    id: int
    sets: list[CubeSet]

    def minimal_set(self) -> CubeSet:
        result = CubeSet()
        for cube_set in self.sets:
            result |= cube_set
        return result


def parse_input(lines: Iterable[str]) -> Iterator[Game]:
    for line in lines:
        game_id = int(line.split(':')[0].split(' ')[1])
        sets = [parse_set(part) for part in line.split(': ')[1].split('; ')]
        yield Game(id=game_id, sets=sets)


def parse_set(s: str) -> CubeSet:
    color_counts = {}
    for part in s.split(', '):
        count, color = part.split(' ')
        color_counts[color] = int(count)
    return CubeSet(**color_counts)


def main():
    games = list(parse_input(line.rstrip('\n') for line in sys.stdin))
    reference = CubeSet(red=12, green=13, blue=14)
    print(sum(game.id for game in games if game.minimal_set().is_subset(reference)))
    print(sum(game.minimal_set().power() for game in games))


if __name__ == '__main__':
    main()
