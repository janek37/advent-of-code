import re
import sys
from enum import Enum
from typing import Iterable

Position = tuple[int, int]


class Direction(Enum):
    NORTH = (0, -1), (-1, -1), (1, -1)
    SOUTH = (0, 1), (-1, 1), (1, 1)
    WEST = (-1, 0), (-1, -1), (-1, 1)
    EAST = (1, 0), (1, -1), (1, 1)

    def move(self, position: Position) -> Position:
        x, y = position
        delta_x, delta_y = self.value[0]
        return x + delta_x, y + delta_y

    def to_check(self, position: Position) -> Iterable[Position]:
        x, y = position
        for delta_x, delta_y in self.value:
            yield x + delta_x, y + delta_y


def parse_input(lines: Iterable[str]) -> Iterable[Position]:
    for y, line in enumerate(lines):
        yield from ((match.span()[0], y) for match in re.finditer(r'#', line))


def simulate_round(elves: list[Position], priorities: list[Direction]) -> Iterable[Position]:
    elves_set = set(elves)
    assert len(elves) == len(elves_set)
    proposed_moves = {}
    target_positions = set()
    duplicate_positions = set()
    for elf in elves:
        if all(neighbor not in elves_set for neighbor in get_neighbors(elf)):
            continue
        for direction in priorities:
            if all(position_to_check not in elves_set for position_to_check in direction.to_check(elf)):
                target_position = direction.move(elf)
                if target_position in target_positions:
                    duplicate_positions.add(target_position)
                else:
                    proposed_moves[elf] = target_position
                    target_positions.add(target_position)
                break
    for elf in elves:
        if elf in proposed_moves and proposed_moves[elf] not in duplicate_positions:
            yield proposed_moves[elf]
        else:
            yield elf


def get_neighbors(position: Position) -> Iterable[Position]:
    x0, y0 = position
    for x in range(x0 - 1, x0 + 2):
        for y in range(y0 - 1, y0 + 2):
            if (x, y) != (x0, y0):
                yield x, y


def simulate_rounds(elves: list[Position], count: int) -> list[Position]:
    priorities = [Direction.NORTH, Direction.SOUTH, Direction.WEST, Direction.EAST]
    for _ in range(count):
        elves = list(simulate_round(elves, priorities))
        priorities = priorities[1:] + priorities[:1]
    return elves


def simulate_all_rounds(elves: list[Position]) -> int:
    priorities = [Direction.NORTH, Direction.SOUTH, Direction.WEST, Direction.EAST]
    rounds = 0
    while True:
        new_elves = list(simulate_round(elves, priorities))
        rounds += 1
        if new_elves == elves:
            break
        priorities = priorities[1:] + priorities[:1]
        elves = new_elves
    return rounds


def ground_covered(elves: list[Position]) -> int:
    min_x = min(x for x, y in elves)
    max_x = max(x for x, y in elves)
    min_y = min(y for x, y in elves)
    max_y = max(y for x, y in elves)
    return (max_x - min_x + 1) * (max_y - min_y + 1) - len(elves)


def main():
    elves = list(parse_input(line.rstrip('\n') for line in sys.stdin))
    new_elves = simulate_rounds(elves, 10)
    print(ground_covered(new_elves))
    print(simulate_all_rounds(elves))


if __name__ == '__main__':
    main()
