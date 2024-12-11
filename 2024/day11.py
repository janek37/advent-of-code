import sys
from functools import cache


@cache
def fast_forward(stone: int, steps: int) -> int:
    next_step = blink(stone)
    if steps == 1:
        return len(next_step)
    else:
        return sum(fast_forward(s, steps - 1) for s in next_step)


def blink(stone: int) -> list[int]:
    if stone == 0:
        return [1]
    str_stone = str(stone)
    if len(str_stone) % 2 == 0:
        half = len(str_stone) // 2
        return [int(str_stone[:half]), int(str_stone[half:])]
    else:
        return [stone * 2024]


def main():
    stones = list(int(part) for part in sys.stdin.readline().split())
    print(sum(fast_forward(s, 25) for s in stones))
    print(sum(fast_forward(s, 75) for s in stones))


if __name__ == '__main__':
    main()
