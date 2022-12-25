import sys
from functools import reduce
from typing import Iterable, Generator, Sequence


def parse_input(lines: Iterable[str]) -> Generator[str, None, None]:
    for line in lines:
        yield line.strip()


def rucksack_common_item(rucksack: str) -> str:
    half_len = len(rucksack) // 2
    return find_common_item([rucksack[:half_len], rucksack[half_len:]])


def find_common_item(collections: Iterable[str]) -> str:
    sets = (set(c) for c in collections)
    common_items = reduce(lambda s1, s2: s1 & s2, sets)
    return next(iter(common_items))


def elf_groups(rucksacks: Sequence[str]) -> Generator[Sequence[str], None, None]:
    for i in range(0, len(rucksacks), 3):
        yield rucksacks[i:i+3]


def priority(item: str) -> int:
    if 'a' <= item <= 'z':
        return ord(item) - ord('a') + 1
    else:
        return ord(item) - ord('A') + 27


def main():
    rucksacks = list(parse_input(sys.stdin))
    print(sum(priority(rucksack_common_item(rucksack)) for rucksack in rucksacks))
    print(sum(priority(find_common_item(group)) for group in elf_groups(rucksacks)))


if __name__ == '__main__':
    main()
