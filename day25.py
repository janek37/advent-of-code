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
    for d1, d2 in zip_longest(reversed(n1), reversed(n2), fillvalue='0'):
        carry, r = add_snafu_digits_with_carry(d1, d2, carry)
        result = r + result
    return result


def main():
    print(reduce(add_snafu, (line.rstrip('\n') for line in sys.stdin), ''))


if __name__ == '__main__':
    main()
