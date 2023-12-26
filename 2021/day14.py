from collections import Counter, defaultdict


def parse_input(lines):
    template = lines[0].strip()
    rules = {line[:2]: line[6] for line in lines[2:]}
    return template, rules


def add_counts(counts1, counts2):
    return defaultdict(
        int,
        {
            char: counts1[char] + counts2[char]
            for char in set(counts1) | set(counts2)
        },
    )


memoized = {}


def count_inserted_pair(pair, rules, step_count):
    if (pair, step_count) in memoized:
        return memoized[(pair, step_count)]

    if pair not in rules or step_count == 0:
        counts = defaultdict(int)
    elif step_count == 1:
        counts = defaultdict(int, {rules[pair]: 1})
    else:
        left_pair = pair[0] + rules[pair]
        right_pair = rules[pair] + pair[1]
        counts = add_counts(
            count_inserted_pair(left_pair, rules, step_count-1),
            count_inserted_pair(right_pair, rules, step_count-1),
        )
        counts[rules[pair]] += 1
    memoized[(pair, step_count)] = counts
    return counts


def count_inserted(template, rules, step_count):
    counts = defaultdict(int)
    for char1, char2 in zip(template, template[1:]):
        counts = add_counts(counts, count_inserted_pair(char1+char2, rules, step_count))
    return counts


def total_counts(template, rules, step_count):
    return add_counts(count_inserted(template, rules, step_count), Counter(template))


def get_value(counts):
    s = sorted((count, char) for char, count in counts.items())
    return s[-1][0] - s[0][0]


def main():
    import sys
    template, rules = parse_input(sys.stdin.readlines())
    counts = total_counts(template, rules, 10)
    print(get_value(counts))
    counts = total_counts(template, rules, 40)
    print(get_value(counts))


if __name__ == '__main__':
    main()
