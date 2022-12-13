import sys
from typing import Iterable

DISPLAY_WIDTH = 40


def parse_input(lines: Iterable[str]) -> Iterable[int | None]:
    for line in lines:
        line = line.strip()
        if line == 'noop':
            yield None
        else:
            yield int(line.split()[1])


def simulate_cycles(instructions: Iterable[int | None]) -> Iterable[int]:
    x = 1
    for instruction in instructions:
        yield x
        if instruction is not None:
            yield x
            x += instruction


def crt(sprite_positions: Iterable[int]) -> Iterable[bool]:
    for cycle, x in enumerate(sprite_positions):
        cycle = cycle % DISPLAY_WIDTH
        yield abs(cycle - x) <= 1


def draw(pixels: Iterable[bool]) -> None:
    for x, is_lit in enumerate(pixels):
        print('#' if is_lit else '.', end='')
        if (x + 1) % 40 == 0:
            print()


def main():
    instructions = list(parse_input(sys.stdin))
    total_strength = 0
    for cycle, x in enumerate(simulate_cycles(instructions), start=1):
        if cycle in (20, 60, 100, 140, 180, 220):
            total_strength += cycle * x
    print(total_strength)
    draw(crt(simulate_cycles(instructions)))


if __name__ == '__main__':
    main()
