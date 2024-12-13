import re
import sys
from typing import Iterator


type ClawMachine = tuple[tuple[int, int], tuple[int, int], tuple[int, int]]


def parse_input(lines: Iterator[str]) -> Iterator[ClawMachine]:
    button_a = (0, 0)
    button_b = (0, 0)
    for line in lines:
        numbers = tuple(map(int, re.findall(r'\d+', line)))
        if line.startswith('Button A'):
            button_a = numbers
        elif line.startswith('Button B'):
            button_b = numbers
        elif line.startswith('Prize'):
            yield button_a, button_b, numbers


def solve_machine(machine: ClawMachine) -> int | None:
    (a_x, a_y), (b_x, b_y), (prize_x, prize_y) = machine
    # a*a_x + b*b_x = prize_x
    # a*a_y + b*b_y = prize_y
    # so:
    # a * (a_x*b_y - a_y*b_x) = prize_x*b_y - prize_y*b_x
    a_multiplier = a_x*b_y - a_y*b_x
    a_rhs = prize_x*b_y - prize_y*b_x
    if a_multiplier != 0:
        if a_rhs % a_multiplier == 0 and a_rhs // a_multiplier >= 0:
            a = a_rhs // a_multiplier
            b_rhs = prize_x - a*a_x
            if b_rhs % b_x == 0 and b_rhs // b_x >= 0:
                b = b_rhs // b_x
                return 3*a + b
    elif a_rhs == 0:
        # this could be tricky to implement, luckily not needed for my input
        raise NotImplementedError
    return None


def update_machine(machine: ClawMachine) -> ClawMachine:
    prize_x, prize_y = machine[2]
    return machine[0], machine[1], (prize_x + 10000000000000, prize_y + 10000000000000)


def main():
    claw_machines = list(parse_input(sys.stdin))
    solutions = (solve_machine(machine) for machine in claw_machines)
    print(sum(filter(None, solutions)))
    updated_machines = map(update_machine, claw_machines)
    updated_solutions = (solve_machine(machine) for machine in updated_machines)
    print(sum(filter(None, updated_solutions)))


if __name__ == '__main__':
    main()
