def parse_input(lines):
    dots = []
    folds = []
    for line in lines:
        if ',' in line:
            dots.append(tuple(int(s) for s in line.strip().split(',')))
        elif line.startswith('fold along '):
            axis = line[11]
            value = int(line[13:])
            folds.append((axis, value))
    return set(dots), folds


def make_fold(dots, fold):
    axis, value = fold
    new_dots = set()
    for x, y in dots:
        if axis == 'x' and x > value:
            new_dots.add((2*value - x, y))
        elif axis == 'y' and y > value:
            new_dots.add((x, 2*value - y))
        else:
            new_dots.add((x, y))
    return new_dots


def dot_lines(dots, width, height):
    lines = [[' '] * width for _ in range(height)]
    for x, y in dots:
        lines[y][x] = '#'
    return [''.join(line) for line in lines]


def main():
    import sys
    dots, folds = parse_input(sys.stdin)
    print(len(make_fold(dots, folds[0])))
    for fold in folds:
        dots = make_fold(dots, fold)
    for line in dot_lines(dots, 40, 6):
        print(line)


if __name__ == '__main__':
    main()
