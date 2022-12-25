import sys
from functools import reduce
from itertools import zip_longest

SNAFU_DIGITS = {
    '2': 2,
    '1': 1,
    '0': 0,
    '-': -1,
    '=': -2,
}

SNAFU_REVERSE = '012=-'


def add_snafu_digits(d1: str, d2: str, d3: str = '0') -> str:
    s = sum(SNAFU_DIGITS[d] for d in (d1, d2, d3))
    last_digit = (s + 2) % 5 - 2
    first_digit = (s - last_digit) // 5
    return SNAFU_REVERSE[first_digit] + SNAFU_REVERSE[last_digit]


def add_snafu(n1: str, n2: str) -> str:
    carry = '0'
    result = ''
    for d1, d2 in zip_longest(reversed(n1), reversed(n2), fillvalue='0'):
        carry, r = add_snafu_digits(d1, d2, carry)
        result = r + result
    return result


def main():
    print(reduce(add_snafu, (line.rstrip('\n') for line in sys.stdin), ''))


if __name__ == '__main__':
    main()
