import itertools
import math
import sys
from typing import Iterator

Nodes = dict[str, tuple[str, str]]


def parse_input(lines: Iterator[str]) -> tuple[str, Nodes]:
    instructions = next(lines)
    next(lines)  # skip empty line
    nodes = {}
    for line in lines:
        node_id, neighbors = line.split(' = ')
        neighbors = neighbors[1:-1].split(', ')
        nodes[node_id] = tuple(neighbors)
    return instructions, nodes


def move(instructions: Iterator[str], nodes: Nodes, start: str, goal: str) -> int:
    current = start
    for i, instruction in enumerate(instructions, start=1):
        current = nodes[current][instruction == 'R']
        if current == goal:
            return i


def find_cycle(instructions: str, nodes: Nodes, start: str) -> int:
    step1 = start
    step2 = start
    input_period = len(instructions)
    instructions1 = itertools.cycle(instructions)
    instructions2 = itertools.cycle(instructions)
    for (i, instruction1) in enumerate(instructions1, start=1):
        step1 = nodes[step1][instruction1 == 'R']
        step2 = nodes[step2][next(instructions2) == 'R']
        step2 = nodes[step2][next(instructions2) == 'R']
        if step1 == step2 and i % input_period == (i * 2) % input_period:
            return i


def main():
    instructions, nodes = parse_input(line.rstrip('\n') for line in sys.stdin)
    print(move(itertools.cycle(instructions), nodes, 'AAA', 'ZZZ'))
    ghosts = (node for node in nodes if node.endswith('A'))
    periods = [find_cycle(instructions, nodes, ghost) for ghost in ghosts]
    # it happens that for every ghost the only Z position is exactly at the end of the period
    # it doesn't have to be like that, but it simplifies A LOT
    print(math.lcm(*periods))


if __name__ == '__main__':
    main()
