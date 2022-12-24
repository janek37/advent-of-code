import sys
from collections import deque
from dataclasses import dataclass, field
from enum import Enum
from typing import Iterable

Position = tuple[int, int]
Blizzard = tuple[Position, "Direction"]


class Direction(Enum):
    UP = (0, -1)
    DOWN = (0, 1)
    LEFT = (-1, 0)
    RIGHT = (1, 0)
    STAY = (0, 0)

    @classmethod
    def from_str(cls, s: str) -> "Direction":
        if s == '^':
            return cls.UP
        elif s == 'v':
            return cls.DOWN
        elif s == '<':
            return cls.LEFT
        elif s == '>':
            return cls.RIGHT

    def to_str(self):
        if self == Direction.UP:
            return '^'
        elif self == Direction.DOWN:
            return 'v'
        elif self == Direction.LEFT:
            return '<'
        elif self == Direction.RIGHT:
            return '>'


def parse_input(lines: Iterable[str]) -> tuple[int, int, list[Blizzard]]:
    blizzards = []
    width = height = 0
    for y, line in enumerate(lines):
        width = len(line)
        height = y
        for x, char in enumerate(line):
            if char in '^v<>':
                pass
                blizzards.append(((x, y), Direction.from_str(char)))
    return width, height + 1, blizzards


@dataclass
class Valley:
    width: int
    height: int
    initial_blizzards: list[Blizzard]

    def simulate_blizzards(self, blizzards: list[Blizzard]) -> Iterable[Blizzard]:
        for (x, y), direction in blizzards:
            delta_x, delta_y = direction.value
            new_x = (x + delta_x - 1) % (self.width - 2) + 1
            new_y = (y + delta_y - 1) % (self.height - 2) + 1
            yield (new_x, new_y), direction

    _cached_blizzards: list[tuple[list[Blizzard], set[Position]]] = field(default_factory=list)

    def get_blizzard_positions(self, time: int) -> set[Position]:
        if time < len(self._cached_blizzards):
            return self._cached_blizzards[time][1]
        if len(self._cached_blizzards) < time:
            self.get_blizzard_positions(time - 1)
        assert len(self._cached_blizzards) == time

        if time == 0:
            blizzards = self.initial_blizzards
        else:
            blizzards = list(self.simulate_blizzards(self._cached_blizzards[time - 1][0]))
        positions = set(position for position, direction in blizzards)
        self._cached_blizzards.append((blizzards, positions))
        return positions

    @property
    def goal(self):
        return self.width - 2, self.height - 1

    def find_route(self, start: Position, goal: Position, time: int = 0) -> int | None:
        queue: deque[tuple[int, Position]] = deque()
        queue.appendleft((time, start))
        visited: set[tuple[int, Position]] = set()

        while queue:
            time, position = queue.pop()
            if (time, position) not in visited:
                visited.add((time, position))
                if position == goal:
                    return time
                x, y = position

                for direction in [Direction.DOWN, Direction.RIGHT, Direction.STAY, Direction.LEFT, Direction.UP]:
                    delta_x, delta_y = direction.value
                    new_x = x + delta_x
                    new_y = y + delta_y
                    if self.can_move(time + 1, (new_x, new_y)):
                        queue.appendleft((time + 1, (new_x, new_y)))

    def find_triple_route(self) -> int | None:
        time = self.find_route((1, 0), self.goal)
        time = self.find_route(self.goal, (1, 0), time)
        time = self.find_route((1, 0), self.goal, time)
        return time

    def can_move(self, time: int, position: Position) -> bool:
        x, y = position
        if not (0 < x < self.width - 1):
            return False
        if not (0 < y < self.height - 1) and (x, y) != (1, 0) and (x, y) != self.goal:
            return False
        return position not in self.get_blizzard_positions(time)


def main():
    width, height, blizzards = parse_input(line.rstrip('\n') for line in sys.stdin)
    valley = Valley(width, height, blizzards)
    print(valley.find_route((1, 0), valley.goal))
    print(valley.find_triple_route())


if __name__ == '__main__':
    main()
