CLOSING = {
    '{': '}',
    '(': ')',
    '[': ']',
    '<': '>',
}

ERROR_SCORES = {
    ')': 3,
    ']': 57,
    '}': 1197,
    '>': 25137,
}

COMPLETE_SCORES = {
    ')': 1,
    ']': 2,
    '}': 3,
    '>': 4,
}


def find_first_error(line):
    stack = []
    for char in line:
        if char in CLOSING.keys():
            stack.append(CLOSING[char])
        else:
            if char == stack[-1]:
                stack.pop()
            else:
                return char


def get_error_scores(lines):
    for line in lines:
        error = find_first_error(line.strip())
        if error:
            yield ERROR_SCORES[error]


def autocomplete(line):
    stack = []
    for char in line:
        if char in CLOSING.keys():
            stack.append(CLOSING[char])
        else:
            if char == stack[-1]:
                stack.pop()
            else:
                return
    return ''.join(reversed(stack))


def get_complete_score(completion):
    score = 0
    for char in completion:
        score = score*5 + COMPLETE_SCORES[char]
    return score


def get_complete_scores(lines):
    for line in lines:
        completion = autocomplete(line.strip())
        if completion:
            yield get_complete_score(completion)


def main():
    import sys
    lines = list(sys.stdin)
    print(sum(get_error_scores(lines)))
    scores = sorted(get_complete_scores(lines))
    print(scores[len(scores)//2])


if __name__ == '__main__':
    main()

