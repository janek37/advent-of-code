import math
import operator
import sys
from functools import reduce
from typing import Iterator


def parse_input(lines: Iterator[str]) -> tuple[list[int], list[int], int, int]:
    times_line = next(lines)
    times = [int(part) for part in times_line.split(':')[1].strip().split()]
    big_time = int(times_line.split(':')[1].replace(' ', ''))
    records_line = next(lines)
    records = [int(part) for part in records_line.split(':')[1].strip().split()]
    big_record = int(records_line.split(':')[1].replace(' ', ''))
    return times, records, big_time, big_record


def get_winning_ways_count(time: int, record: int) -> int:
    sqrt_delta = math.sqrt(time*time/4 - record)
    min_hold_time = time / 2 - sqrt_delta
    max_hold_time = time / 2 + sqrt_delta
    return math.floor(max_hold_time) - math.ceil(min_hold_time) + 1


def main():
    times, records, big_time, big_record = parse_input(line.rstrip('\n') for line in sys.stdin)
    print(reduce(operator.mul, (get_winning_ways_count(time, record) for time, record in zip(times, records))))
    print(get_winning_ways_count(big_time, big_record))


if __name__ == '__main__':
    main()
