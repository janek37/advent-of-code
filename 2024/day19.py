import sys
from typing import Iterator


def parse_input(lines: Iterator[str]) -> tuple[list[str], list[str]]:
    towels = next(lines).strip().split(", ")
    next(lines)  # skip empty line
    designs = [line.strip() for line in lines]
    return towels, designs


_cache = {'': 1}

def count_ways(design: str, towels: list[str]) -> int:
    if design in _cache:
        return _cache[design]
    else:
        count = sum(count_ways(design[len(towel):], towels) for towel in towels if design.startswith(towel))
        _cache[design] = count
        return count


def main():
    towels, designs = list(parse_input(sys.stdin))
    ways = [count_ways(design, towels) for design in designs]
    print(len(ways) - ways.count(0))
    print(sum(ways))



if __name__ == '__main__':
    main()
