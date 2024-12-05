import sys
from functools import cmp_to_key
from typing import Iterator


def parse_input(lines: Iterator[str]) -> tuple[set[tuple[int, int]], list[list[int]]]:
    rules = set()
    for line in lines:
        line = line.strip()
        if line == '':
            break
        a, b = map(int, line.split('|'))
        rules.add((a, b))
    updates = [list(map(int, line.split(','))) for line in lines]
    return rules, updates


def check_update(update: list[int], rules: set[tuple[int, int]]) -> bool:
    for i, page1 in enumerate(update):
        for j, page2 in enumerate(update[i+1:], start=i+1):
            if (page2, page1) in rules:
                return False
    return True


def sort_update(update: list[int], rules: set[tuple[int, int]]) -> list[int]:
    def compare_page(page1: int, page2: int) -> int:
        if (page1, page2) in rules:
            return -1
        elif (page2, page1) in rules:
            return 1
        else:
            return 0

    return sorted(update, key=cmp_to_key(compare_page))


def middle_page(update: list[int]) -> int:
    return update[len(update) // 2]


def main():
    rules, updates = list(parse_input(sys.stdin))
    print(sum(middle_page(update) for update in updates if check_update(update, rules)))
    print(sum(middle_page(sort_update(update, rules)) for update in updates if not check_update(update, rules)))


if __name__ == '__main__':
    main()
