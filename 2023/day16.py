import sys
from typing import NamedTuple


class Position(NamedTuple):
    x: int
    y: int
    dx: int
    dy: int

    def move(self, dx: int, dy: int):
        return Position(self.x + dx, self.y + dy, dx, dy)


def beam_step(beam: Position, contrapion: list[str]) -> tuple[Position, ...]:
    # caching saves about a second (3.5s -> 2.5s)
    if not hasattr(beam_step, '_cache'):
        beam_step._cache = {}
    if beam in beam_step._cache:
        return beam_step._cache[beam]
    tile = contrapion[beam.y][beam.x]
    if tile == '.':
        next_beam = (beam.move(beam.dx, beam.dy),)
    elif tile == '/':
        next_beam = (beam.move(-beam.dy, -beam.dx),)
    elif tile == '\\':
        next_beam = (beam.move(beam.dy, beam.dx),)
    elif tile == '|':
        if beam.dx == 0:
            next_beam = (beam.move(beam.dx, beam.dy),)
        else:
            next_beam = beam.move(0, -1), beam.move(0, 1)
    elif tile == '-':
        if beam.dy == 0:
            next_beam = (beam.move(beam.dx, beam.dy),)
        else:
            next_beam = beam.move(-1, 0), beam.move(1, 0)
    else:
        raise ValueError
    beam_step._cache[beam] = next_beam
    return next_beam


def get_energized_tiles(contraption: list[str], beam: Position) -> int:
    width = len(contraption[0])
    height = len(contraption)
    beams = [beam]
    visited_positions = set()
    visited_tiles = set()
    while beams:
        beam = beams.pop()
        next_beams = beam_step(beam, contraption)
        for next_beam in next_beams:
            if next_beam not in visited_positions and 0 <= next_beam.x < width and 0 <= next_beam.y < height:
                beams.append(next_beam)
        visited_positions.add(beam)
        visited_tiles.add((beam.x, beam.y))
    return len(visited_tiles)


def get_max_energized_tiles(contraption: list[str]) -> int:
    width = len(contraption[0])
    height = len(contraption)
    left = max(get_energized_tiles(contraption, Position(0, y, 1, 0)) for y in range(height))
    right = max(get_energized_tiles(contraption, Position(width - 1, y, -1, 0)) for y in range(height))
    top = max(get_energized_tiles(contraption, Position(x, 0, 0, 1)) for x in range(width))
    bottom = max(get_energized_tiles(contraption, Position(x, height - 1, 0, -1)) for x in range(width))
    return max(left, right, top, bottom)


def main():
    contraption = list((line.rstrip('\n') for line in sys.stdin))
    print(get_energized_tiles(contraption, Position(x=0, y=0, dx=1, dy=0)))
    print(get_max_energized_tiles(contraption))


if __name__ == '__main__':
    main()
