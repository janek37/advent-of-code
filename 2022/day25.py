import sys
from functools import reduce
from itertools import zip_longest

ADDITION_TABLE = {
    '==': '-1', '=-': '-2', '=0': '0=', '=1': '0-', '=2': '00',
    '-=': '-2', '--': '0=', '-0': '0-', '-1': '00', '-2': '01',
    '0=': '0=', '0-': '0-', '00': '00', '01': '01', '02': '02',
    '1=': '0-', '1-': '00', '10': '01', '11': '02', '12': '1=',
    '2=': '00', '2-': '01', '20': '02', '21': '1=', '22': '1-',
}


def add_snafu_digits(d1: str, d2: str) -> str:
    return ADDITION_TABLE[d1 + d2]


def add_snafu_digits_with_carry(d1: str, d2: str, carry: str) -> str:
    carry1, s = add_snafu_digits(d1, d2)
    carry2, s = add_snafu_digits(s, carry)
    _, final_carry = add_snafu_digits(carry1, carry2)
    return final_carry + s


def add_snafu(n1: str, n2: str) -> str:
    carry = '0'
    result = ''
    for d1, d2 in zip_longest(reversed(n1), reversed('0' + n2), fillvalue='0'):
        carry, r = add_snafu_digits_with_carry(d1, d2, carry)
        result = r + result
    return result.lstrip('0')


DIGITS = '=-012'


def snafu_to_int(snafu: str) -> int:
    result = 0
    for digit in snafu:
        result *= 5
        result += DIGITS.index(digit) - 2
    return result


def int_to_snafu(n: int) -> str:
    result = ''
    while n != 0:
        digit = (n + 2) % 5 - 2
        n = (n - digit) // 5
        result = DIGITS[digit + 2] + result
    return result


def main():
    snafus = [line.rstrip('\n') for line in sys.stdin]
    print(reduce(add_snafu, snafus, ''))
    # print(int_to_snafu(sum(snafu_to_int(snafu) for snafu in snafus)))


if __name__ == '__main__':
    main()
