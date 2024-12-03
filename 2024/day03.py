import re
import sys
from typing import Iterator


def find_muls(s: str) -> Iterator[tuple[int, int]]:
    return ((int(a), int(b)) for a, b in re.findall(r'mul\(([0-9]{1,3}),([0-9]{1,3})\)', s))


def find_enabled_muls(s: str) -> Iterator[tuple[int, int]]:
    enabled = True
    for a, b, instruction in re.findall(r'mul\(([0-9]{1,3}),([0-9]{1,3})\)|(do(?:n\'t)?)\(\)', s):
        if instruction == "do":
            enabled = True
        elif instruction == "don't":
            enabled = False
        elif enabled:
            yield int(a), int(b)


def main():
    memory_dump = sys.stdin.read()
    print(sum(a * b for a, b in find_muls(memory_dump)))
    print(sum(a * b for a, b in find_enabled_muls(memory_dump)))


if __name__ == '__main__':
    main()
