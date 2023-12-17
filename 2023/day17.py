import heapq
import sys
from collections import defaultdict
from typing import NamedTuple, Iterator

from typing_extensions import Self


class Direction(NamedTuple):
    dx: int
    dy: int


EAST = Direction(1, 0)
WEST = Direction(-1, 0)
NORTH = Direction(0, -1)
SOUTH = Direction(0, 1)


class Position(NamedTuple):
    x: int
    y: int
    direction: Direction
    run: int

    def neighbors(self, width: int, height: int) -> Iterator[Self]:
        excluded_directions = [Direction(-self.direction.dx, -self.direction.dy)]
        if self.run == 3:
            excluded_directions.append(self.direction)
        for direction in (EAST, WEST, NORTH, SOUTH):
            if direction not in excluded_directions:
                yield from self._new_position(direction, width, height)

    def _new_position(self, direction: Direction, width, height) -> Iterator[Self]:
        run = self.run + 1 if direction == self.direction else 1
        x = self.x + direction.dx
        y = self.y + direction.dy
        if 0 <= x < width and 0 <= y < height:
            yield self.__class__(x, y, direction, run)

    @property
    def min_run(self) -> int:
        return 0


class UltraPosition(Position):
    def neighbors(self, width: int, height: int) -> Iterator[Self]:
        directions = (EAST, WEST, NORTH, SOUTH)
        excluded_directions = [Direction(-self.direction.dx, -self.direction.dy)]
        if self.run == 10:
            excluded_directions.append(self.direction)
        if 1 <= self.run < 4:
            directions = [self.direction]
        for direction in directions:
            if direction not in excluded_directions:
                yield from self._new_position(direction, width, height)

    @property
    def min_run(self) -> int:
        return 4


def dijkstra(heat_loss_map: list[list[int]], start: Position) -> int:
    width = len(heat_loss_map[0])
    height = len(heat_loss_map)
    goal = width - 1, height - 1
    priority_queue = [(0, start, [])]
    total_heat_loss_map = defaultdict(lambda: float('infinity'))
    while priority_queue:
        heat_loss, position, path = heapq.heappop(priority_queue)
        if (position.x, position.y) == goal and position.run >= position.min_run:
            return heat_loss
        for new_position in position.neighbors(width, height):
            new_heat_loss = heat_loss + heat_loss_map[new_position.y][new_position.x]
            if new_heat_loss < total_heat_loss_map[new_position]:
                new_path = path + [position]
                total_heat_loss_map[new_position] = new_heat_loss
                heapq.heappush(priority_queue, (new_heat_loss, new_position, new_path))


def main():
    heat_loss_map = list(list(map(int, line.rstrip('\n'))) for line in sys.stdin)
    print(dijkstra(heat_loss_map, Position(0, 0, EAST, 0)))
    print(dijkstra(heat_loss_map, UltraPosition(0, 0, EAST, 0)))


if __name__ == '__main__':
    main()
