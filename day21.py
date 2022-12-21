import operator
import sys
from dataclasses import dataclass
from fractions import Fraction
from typing import Iterable, Callable, TypeVar

OPERATORS = {
    '+': operator.add,
    '-': operator.sub,
    '*': operator.mul,
    '/': operator.truediv,
}

T = TypeVar('T')
Operator = Callable[[T, T], T]


@dataclass
class Num:
    value: Fraction


@dataclass
class Op:
    monkey1: str
    monkey2: str
    op: Operator


Monkey = Num | Op


def parse_input(lines: Iterable[str]) -> Iterable[tuple[str, Monkey]]:
    for line in lines:
        name, job = line.split(': ')
        if '0' <= job[0] <= '9':
            monkey = Num(Fraction(job))
        else:
            m1, op, m2 = job.split()
            monkey = Op(m1, m2, OPERATORS[op])
        yield name, monkey


def evaluate(name: str, monkeys: dict[str, Monkey]) -> tuple[int, bool]:
    match monkeys[name]:
        case Num(n):
            return n, name != 'humn'
        case Op(m1, m2, op):
            val1, clean1 = evaluate(m1, monkeys)
            val2, clean2 = evaluate(m2, monkeys)
            clean = clean1 and clean2
            result = op(val1, val2)
            if clean:
                monkeys[name] = Num(result)
            return result, clean


def eval_humn(value: int, monkey: str, monkeys: dict[str, Monkey]) -> float:
    monkeys['humn'] = Num(Fraction(value))
    return evaluate(monkey, monkeys)[0]


def human_binary_search(root: str, monkeys: dict[str, Monkey]) -> int:
    monkey, value = get_variable_monkey_and_constant_value(root, monkeys)

    def human(v: int):
        return eval_humn(v, monkey, monkeys)

    min_humn = -int(value)
    max_humn = int(value)
    if human(min_humn) > human(max_humn):
        min_humn, max_humn = max_humn, min_humn

    while min_humn != max_humn:
        new_humn = (min_humn + max_humn) // 2
        if human(new_humn) <= value:
            min_humn = new_humn
        if human(new_humn) >= value:
            max_humn = new_humn
    return min_humn


def find_human_value(root: str, monkeys: dict[str, Monkey]) -> int:
    monkey, value = get_variable_monkey_and_constant_value(root, monkeys)

    humat_at_0 = eval_humn(0, monkey, monkeys)
    human_at_1 = eval_humn(1, monkey, monkeys)
    return int((value - humat_at_0)/(human_at_1 - humat_at_0))


def get_variable_monkey_and_constant_value(root: str, monkeys: dict[str, Monkey]) -> tuple[str, int]:
    monkey1 = monkeys[root].monkey1
    monkey2 = monkeys[root].monkey2
    val1, clean1 = evaluate(monkey1, monkeys)
    val2, clean2 = evaluate(monkey2, monkeys)
    monkey = monkey2 if clean1 else monkey1
    value = val1 if clean1 else val2
    return monkey, int(value)


def main():
    monkeys = dict(parse_input(line.strip() for line in sys.stdin))
    print(evaluate('root', monkeys)[0])
    # print(human_binary_search('root', monkeys))
    print(find_human_value('root', monkeys))


if __name__ == '__main__':
    main()
