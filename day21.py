import operator
import sys
from dataclasses import dataclass
from typing import Iterable, Callable, Union, TypeVar

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
    value: int


@dataclass
class Op:
    monkey1: str
    monkey2: str
    op: Operator


def parse_input(lines: Iterable[str]) -> Iterable[tuple[str, Num | Op]]:
    for line in lines:
        name, job = line.split(': ')
        if '0' <= job[0] <= '9':
            monkey = Num(int(job))
        else:
            m1, op, m2 = job.split()
            monkey = Op(m1, m2, OPERATORS[op])
        yield name, monkey


def evaluate(name: str, monkeys: dict[str, Num | Op]) -> tuple[int, bool]:
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


def eval_humn(value: int, monkey: str, monkeys: dict[str, Op | Num]):
    monkeys['humn'] = Num(value)
    return evaluate(monkey, monkeys)[0]


def main():
    monkeys = dict(parse_input(line.strip() for line in sys.stdin))
    print(evaluate('root', monkeys)[0])
    val1, clean1 = evaluate(monkeys['root'].monkey1, monkeys)
    val2, clean2 = evaluate(monkeys['root'].monkey2, monkeys)
    monkey = monkeys['root'].monkey2 if clean1 else monkeys['root'].monkey1
    value = val1 if clean1 else val2
    min_humn = -value
    max_humn = value

    def human(v: int):
        return eval_humn(v, monkey, monkeys)

    if human(min_humn) > human(max_humn):
        min_humn, max_humn = max_humn, min_humn
    while min_humn != max_humn:
        new_humn = (min_humn + max_humn) // 2
        if human(new_humn) <= value:
            min_humn = new_humn
        if human(new_humn) >= value:
            max_humn = new_humn
        assert human(min_humn) <= value <= human(max_humn)
    print(min_humn)


if __name__ == '__main__':
    main()
