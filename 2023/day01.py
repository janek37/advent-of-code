import re
import sys


DIGITS = {
    'one': 1,
    'two': 2,
    'three': 3,
    'four': 4,
    'five': 5,
    'six': 6,
    'seven': 7,
    'eight': 8,
    'nine': 9,
}

DIGIT_REGEX = re.compile(fr'(?=(\d|{"|".join(DIGITS.keys())}))')


def find_edge_digits(s):
    found = [match.group(1) for match in DIGIT_REGEX.finditer(s)]
    return parse_digit(found[0]) + parse_digit(found[-1])


def parse_digit(s):
    return str(DIGITS.get(s, s))


def main():
    regex = re.compile(r'^\D*(\d).*(\d)\D*$')
    lines = list(sys.stdin)
    cleaned = (regex.sub(r'\1\2', re.sub(r'(\d)', r'\1\1', line, count=1)) for line in lines)
    print(sum(int(s) for s in cleaned))
    print(sum(int(find_edge_digits(line)) for line in lines))


if __name__ == '__main__':
    main()
