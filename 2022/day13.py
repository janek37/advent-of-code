import functools
import re
import sys
from enum import Enum
from typing import Iterable


class Ordering(Enum):
    LESS = -1
    EQUAL = 0
    GREATER = 1


def parse_input(lines: Iterable[str]) -> Iterable[tuple]:
    for line in lines:
        if not line:
            continue
        assert re.fullmatch(r'[\[\]0-9,]+', line), line
        yield eval(line)  # sorry


def make_pairs(items: Iterable) -> Iterable[tuple]:
    items_iter = iter(items)
    while True:
        try:
            yield next(items_iter), next(items_iter)
        except StopIteration:
            break


def compare_packet_parts(left, right) -> Ordering:
    if isinstance(left, int) and isinstance(right, int):
        if left < right:
            return Ordering.LESS
        elif left == right:
            return Ordering.EQUAL
        else:
            return Ordering.GREATER
    if isinstance(left, int):
        left = [left]
    if isinstance(right, int):
        right = [right]
    # at this point both parts are lists
    for left_item, right_item in zip(left, right):
        ordering = compare_packet_parts(left_item, right_item)
        if ordering is not Ordering.EQUAL:
            return ordering
    # one list is a prefix of the other
    return compare_packet_parts(len(left), len(right))


def main():
    packets = list(parse_input(line.strip() for line in sys.stdin))
    packet_pairs = make_pairs(packets)
    print(sum(
        i for i, (left, right) in enumerate(packet_pairs, start=1)
        if compare_packet_parts(left, right) is not Ordering.GREATER
    ))
    sorted_packets = sorted(
        packets + [[[2]], [[6]]],
        key=functools.cmp_to_key(lambda left, right: compare_packet_parts(left, right).value)
    )
    print((sorted_packets.index([[2]])+1) * (sorted_packets.index([[6]])+1))


if __name__ == '__main__':
    main()
