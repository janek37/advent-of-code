import re
import sys
from typing import Iterator


def parse_input(lines: Iterator[str]) -> Iterator[tuple[int, list[int]]]:
    for line in lines:
        nums = list(map(int, re.findall(r'\d+', line)))
        yield nums[0], nums[1:]


def check_equation(value: int, numbers: list[int]) -> bool:
    if len(numbers) == 1:
        return value == numbers[0]
    first, second = numbers[:2]
    return (
        check_equation(value, [first + second, *numbers[2:]])
        or check_equation(value, [first * second, *numbers[2:]])
    )


def check_equation2(value: int, numbers: list[int]) -> bool:
    if len(numbers) == 1:
        return value == numbers[0]
    first, second = numbers[:2]
    return (
        check_equation2(value, [first + second, *numbers[2:]])
        or check_equation2(value, [first * second, *numbers[2:]])
        or check_equation2(value, [int(str(first) + str(second)), *numbers[2:]])
    )


def main():
    equations = list(parse_input(sys.stdin))
    print(sum(value for value, numbers in equations if check_equation(value, numbers)))
    print(sum(value for value, numbers in equations if check_equation2(value, numbers)))


if __name__ == '__main__':
    main()
