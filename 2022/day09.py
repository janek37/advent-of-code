import sys
from typing import Iterable

DIRECTIONS = {
    'D': (0, -1),
    'U': (0, 1),
    'L': (-1, 0),
    'R': (1, 0),
}


def parse_input(lines: Iterable[str]) -> Iterable[tuple[tuple[int, int], int]]:
    for line in lines:
        direction, steps = line.split()
        yield DIRECTIONS[direction], int(steps)


def simulate_rope(moves: Iterable[tuple[tuple[int, int], int]], rope_length: int) -> Iterable[tuple[int, int]]:
    knots = [(0, 0)] * rope_length
    for direction, steps in moves:
        for _ in range(steps):
            simulate_move(direction, knots)
            yield knots[-1]


def simulate_move(direction: tuple[int, int], knots: list[tuple[int, int]]) -> None:
    delta_x, delta_y = direction
    head_x, head_y = knots[0]
    knots[0] = head_x + delta_x, head_y + delta_y
    for i, knot in enumerate(knots[1:]):
        previous_knot = knots[i]
        knots[i+1] = update_knot(previous_knot, knot)


def update_knot(new_head: tuple[int, int], tail: tuple[int, int]) -> tuple[int, int]:
    head_x, head_y = new_head
    tail_x, tail_y = tail
    touching = abs(head_x - tail_x) <= 1 and abs(head_y - tail_y) <= 1
    if not touching:
        if abs(head_x - tail_x) > 0:
            tail_x += (head_x - tail_x) // abs(head_x - tail_x)
        if abs(head_y - tail_y) > 0:
            tail_y += (head_y - tail_y) // abs(head_y - tail_y)
    return tail_x, tail_y


def main():
    moves = list(parse_input(sys.stdin))

    tail_positions = {(0, 0)}
    for tail_position in simulate_rope(moves, 2):
        tail_positions.add(tail_position)
    print(len(tail_positions))

    tail_positions = {(0, 0)}
    for tail_position in simulate_rope(moves, 10):
        tail_positions.add(tail_position)
    print(len(tail_positions))


if __name__ == '__main__':
    main()
