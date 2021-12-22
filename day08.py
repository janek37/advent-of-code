def parse_input(lines):
    for line in lines:
        observed, output = line.strip().split(' | ')
        yield tuple([frozenset(d) for d in x.split()] for x in (observed, output))


def count_unambiguous_digits(output):
    return sum(1 for d in output if len(d) in (2, 3, 4, 7))


def determine_digits(observed):
    one, seven, four, eight = (next(d for d in observed if len(d) == i) for i in [2, 3, 4, 7])
    len_six = [d for d in observed if len(d) == 6]
    len_five = [d for d in observed if len(d) == 5]
    six = next(d for d in len_six if len(d & one) == 1)
    zero = next(d for d in len_six if len(d & four) == 3 and d != six)
    nine = next(d for d in len_six if d not in (zero, six))
    two = next(d for d in len_five if len(d & four) == 2)
    five = next(d for d in len_five if d.issubset(six))
    three = next(d for d in len_five if d not in (two, five))
    return {
        zero: '0', one: '1', two: '2', three: '3', four: '4',
        five: '5', six: '6', seven: '7', eight: '8', nine: '9',
    }


def translate_digits(output, dictionary):
    num = int(''.join(dictionary[d] for d in output))
    print(num)
    return num


def main():
    import sys
    data = list(parse_input(sys.stdin))
    print(sum(count_unambiguous_digits(output) for observed, output in data))
    print(sum(translate_digits(output, determine_digits(observed)) for observed, output in data))


if __name__ == '__main__':
    main()
