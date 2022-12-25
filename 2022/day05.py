import re
import sys
from typing import Iterable

MOVE_REGEX = re.compile(r'move (\d+) from (\d+) to (\d+)')


def parse_input(lines: Iterable[str]) -> tuple[list[list[str]], Iterable[tuple[int, int, int]]]:
    stack_lines = []
    for line in lines:
        if line.strip() == '':
            break
        stack_lines.append(line)
    stacks = parse_stacks(stack_lines)
    moves = parse_moves(lines)
    return stacks, moves


def parse_stacks(stack_lines: list[str]) -> list[list[str]]:
    stack_count = len(stack_lines[-1]) // 4
    stacks = [[] for _ in range(stack_count)]
    for line in stack_lines[-2::-1]:
        for i, stack in enumerate(stacks):
            crate = line[4*i + 1]
            if crate != ' ':
                stack.append(crate)
    return stacks


def parse_moves(lines: Iterable[str]) -> Iterable[tuple[int, int, int]]:
    for line in lines:
        amount, from_stack, to_stack = MOVE_REGEX.match(line).groups()
        yield int(amount), int(from_stack) - 1, int(to_stack) - 1


def move_crates(stacks: list[list[str]], from_stack: int, to_stack: int, amount: int = 1) -> None:
    crates = stacks[from_stack][-amount:]
    del stacks[from_stack][-amount:]
    stacks[to_stack].extend(crates)


def main():
    stacks, moves = parse_input(sys.stdin)
    stacks_copy = [list(stack) for stack in stacks]
    moves = list(moves)
    for amount, from_stack, to_stack in moves:
        for _ in range(amount):
            move_crates(stacks, from_stack, to_stack)
    print(''.join(stack[-1] for stack in stacks))
    stacks = stacks_copy
    for amount, from_stack, to_stack in moves:
        move_crates(stacks, from_stack, to_stack, amount)
    print(''.join(stack[-1] for stack in stacks))


if __name__ == '__main__':
    main()
