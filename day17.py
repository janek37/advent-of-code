import sys
from typing import Set, Iterable

Position = tuple[int, int]
Chamber = Set[Position]

SHAPES = [
    [(0, 0), (1, 0), (2, 0), (3, 0)],
    [(1, 2), (0, 1), (1, 1), (2, 1), (1, 0)],
    [(2, 2), (2, 1), (2, 0), (1, 0), (0, 0)],
    [(0, 3), (0, 2), (0, 1), (0, 0)],
    [(0, 1), (1, 1), (0, 0), (1, 0)],
]


def simulate_blocks(jet_pattern: list[int], block_count: int) -> int:
    chamber = set()
    height = 0
    time = 0
    seen = {}
    for i in range(block_count):
        key = (i % 5, time % len(jet_pattern))
        if key in seen:
            last_height, last_i, last_height_diff = seen[key]
            height_diff = height - last_height
            i_diff = i - last_i
            if height_diff == last_height_diff and (block_count - i) % i_diff == 0:
                return height + (block_count - i) // i_diff * height_diff
            seen[key] = (height, i, height_diff)
        else:
            seen[key] = (height, i, None)
        time, height = simulate_block(SHAPES[i % 5], jet_pattern, chamber, height, time)
    return height


def simulate_block(
    block: list[Position], jet_pattern: list[int], chamber: Chamber, height: int, time: int
) -> tuple[int, int]:
    block_position = (2, height + 3)
    while True:
        jet_direction = jet_pattern[time % len(jet_pattern)]
        block_position = push_block(block, block_position, jet_direction, chamber)
        block_position, stop = move_down(block, block_position, chamber)
        time += 1
        if stop:
            highest_point = settle_block(block, block_position, chamber)
            if highest_point + 1 > height:
                height = highest_point + 1
            return time, height


def push_block(block: list[Position], block_position: Position, jet_direction: int, chamber: Chamber) -> Position:
    position_x, position_y = block_position
    new_position = (position_x + jet_direction, position_y)
    for block_x, block_y in block_part_positions(block, new_position):
        if not (0 <= block_x < 7) or (block_x, block_y) in chamber:
            return block_position
    return new_position


def move_down(block: list[Position], block_position: Position, chamber: Chamber) -> tuple[Position, bool]:
    position_x, position_y = block_position
    new_position = (position_x, position_y - 1)
    for block_x, block_y in block_part_positions(block, new_position):
        if block_y < 0 or (block_x, block_y) in chamber:
            return block_position, True
    return new_position, False


def settle_block(block: list[Position], block_position: Position, chamber: Chamber) -> int:
    highest_point = None
    for block_x, block_y in block_part_positions(block, block_position):
        chamber.add((block_x, block_y))
        if highest_point is None or block_y > highest_point:
            highest_point = block_y
    return highest_point


def block_part_positions(block: list[Position], block_position: Position) -> Iterable[Position]:
    position_x, position_y = block_position
    for block_x, block_y in block:
        yield position_x + block_x, position_y + block_y


def main():
    jet_pattern = [-1 if ch == '<' else 1 for ch in sys.stdin.read().strip()]
    height = simulate_blocks(jet_pattern, 2022)
    print(height)
    print(simulate_blocks(jet_pattern, 1000000000000))


if __name__ == '__main__':
    main()
