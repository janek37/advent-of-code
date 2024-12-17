import sys
from dataclasses import dataclass
from typing import Iterator


@dataclass
class State:
    a: int
    b: int
    c: int
    ip: int


def parse_input(lines: Iterator[str]) -> tuple[State, list[int]]:
    lines = list(lines)
    return State(
        a=int(lines[0].split(": ")[1]),
        b=int(lines[1].split(": ")[1]),
        c=int(lines[2].split(": ")[1]),
        ip=0,
    ), list(map(int, lines[4].split(": ")[1].split(",")))


def run_program(program: list[int], state: State) -> Iterator[int]:
    while 0 <= state.ip < len(program):
        opcode = program[state.ip]
        operand = program[state.ip + 1]
        output = run_op(state, opcode, operand)
        if output is not None:
            yield output
        state.ip += 2


def run_op(state: State, opcode: int, operand: int) -> int | None:
    combo = get_combo_operand(state, operand)
    match opcode:
        case 0:  # adv
            state.a = state.a // (1 << combo)
        case 1:  # bxl
            state.b = state.b ^ operand
        case 2:  # bst
            state.b = combo % 8
        case 3:  # jnz
            if state.a != 0:
                state.ip = operand - 2
        case 4:  # bxc
            state.b = state.b ^ state.c
        case 5:  # out
            return combo % 8
        case 6:  # bdv
            state.b = state.a // (1 << combo)
        case 7:  # cdv
            state.c = state.a // (1 << combo)
        case _:
            raise ValueError(f"Unknown opcode {opcode}")
    return None


def get_combo_operand(state: State, operand: int) -> int:
    if operand < 4:
        return operand
    if operand == 4:
        return state.a
    if operand == 5:
        return state.b
    if operand == 6:
        return state.c
    raise ValueError(f"Invalid combo operand {operand}")


def find_valid_a(program: list[int], acc: int = 0, position: int = 0) -> Iterator[int]:
    # not general, based on my input
    if position == len(program):
        yield acc
    output = program[len(program) - position - 1]
    for digit in range(8):
        new_acc = acc * 8 + digit
        b = digit ^ 5
        c = new_acc // (1 << b)
        b = b ^ c ^ 6
        if b % 8 == output:
            yield from find_valid_a(program, new_acc, position + 1)


def main():
    initial_state,  program = list(parse_input(sys.stdin))
    print(",".join(map(str, run_program(program, initial_state))))
    print(next(find_valid_a(program)))


if __name__ == '__main__':
    main()
