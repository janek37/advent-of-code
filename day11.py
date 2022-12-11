import re
from collections import defaultdict
from dataclasses import dataclass
from functools import reduce
from typing import Iterable, List, Dict
import aocd

MONKEY_REGEX = re.compile(
    r'Monkey (\d+):\n'
    r'\s+Starting items: (.*)\n'
    r'\s+Operation: new = (.*)\n'
    r'\s+Test: divisible by (\d+)\n'
    r'\s+If true: throw to monkey (\d+)\n'
    r'\s+If false: throw to monkey (\d+)'
)


@dataclass
class Monkey:
    id: int
    operation: List[str]
    test_divisible_by: int
    if_true: int
    if_false: int
    items: List[int]
    inspected_count: int = 0
    calm: bool = True
    modulo: int = None

    def inspect_items(self) -> Dict[int, List[int]]:
        thrown_items = defaultdict(list)
        for item in self.items:
            item = self.new_worry_level(item)
            if self.calm:
                item = item // 3
            else:
                item = item % self.modulo
            throw_to = self.if_true if item % self.test_divisible_by == 0 else self.if_false
            thrown_items[throw_to].append(item)
            self.inspected_count += 1
        self.items = []
        return thrown_items

    def new_worry_level(self, old) -> int:
        op1, op, op2 = self.operation
        operands = [op1, op2]
        values = [old if operand == 'old' else int(operand) for operand in operands]
        if op == '+':
            return sum(values)
        elif op == '*':
            return values[0] * values[1]
        else:
            raise ValueError(f'Unknown operator: {op}')


def parse_input(data: str, calm: bool = True, modulo: int = 0) -> Iterable[Monkey]:
    for monkey_declaration in data.split('\n\n'):
        monkey_match = MONKEY_REGEX.match(monkey_declaration)
        assert monkey_match, monkey_declaration
        monkey_id = int(monkey_match.group(1))
        items = [int(item) for item in monkey_match.group(2).split(', ')]
        operation = monkey_match.group(3).split()
        test_divisible_by = int(monkey_match.group(4))
        if_true = int(monkey_match.group(5))
        if_false = int(monkey_match.group(6))
        yield Monkey(
            id=monkey_id,
            items=items,
            operation=operation,
            test_divisible_by=test_divisible_by,
            if_true=if_true,
            if_false=if_false,
            calm=calm,
            modulo=modulo,
        )


def simulate_round(monkeys):
    for monkey in monkeys:
        thrown_items = monkey.inspect_items()
        for monkey_id, items in thrown_items.items():
            monkeys[monkey_id].items.extend(items)


def simulate_rounds(monkeys, rounds):
    for _ in range(rounds):
        simulate_round(monkeys)


def monkey_business(monkeys, rounds):
    simulate_rounds(monkeys, rounds)
    inspected_counts = sorted((monkey.inspected_count for monkey in monkeys), reverse=True)
    return inspected_counts[0] * inspected_counts[1]


def main():
    data = aocd.get_data(year=2022)
    monkeys = list(parse_input(data))
    print(monkey_business(monkeys, 20))
    modulo = reduce(lambda a, b: a * b, (monkey.test_divisible_by for monkey in monkeys))
    monkeys = list(parse_input(data, calm=False, modulo=modulo))
    print(monkey_business(monkeys, 10000))


if __name__ == '__main__':
    main()
