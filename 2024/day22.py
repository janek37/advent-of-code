import sys
from collections import defaultdict


def simulate_pseudorandom(code: int, iterations: int) -> int:
    for _ in range(iterations):
        code = (code ^ (code << 6)) & 0xffffff
        code = code ^ (code >> 5)
        code = (code ^ (code << 11)) & 0xffffff
    return code


def add_price_by_sequence(code: int, iterations: int, totals: dict[tuple[int, int, int, int], int]) -> None:
    sequence = (None, None, None, None)
    seen = set()
    for i in range(iterations):
        last_price = code % 10
        code = (code ^ (code << 6)) & 0xffffff
        code = code ^ (code >> 5)
        code = (code ^ (code << 11)) & 0xffffff
        sequence = (*sequence[1:], code % 10 - last_price)
        if i >= 3 and sequence not in seen:
            seen.add(sequence)
            totals[sequence] += code % 10


def find_best_total(codes: list[int], iterations: int) -> int:
    totals = defaultdict(int)
    for code in codes:
        add_price_by_sequence(code, iterations, totals)
    return max(totals.values())


def main():
    initial_codes = [int(line.strip()) for line in sys.stdin]
    print(sum(simulate_pseudorandom(code, 2000) for code in initial_codes))
    print(find_best_total(initial_codes, 2000))


if __name__ == '__main__':
    main()
