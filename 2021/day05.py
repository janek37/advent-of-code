def parse_input(input_lines):
    for input_line in input_lines:
        line_ends = input_line.strip().split(' -> ')
        x1, y1 = [int(s) for s in line_ends[0].split(',')]
        x2, y2 = [int(s) for s in line_ends[1].split(',')]
        yield (x1, y1, x2, y2)


def filter_vertical_horizontal(lines):
    return [(x1, y1, x2, y2) for x1, y1, x2, y2 in lines if x1 == x2 or y1 == y2]


def get_points(line):
    x1, y1, x2, y2 = line
    if x1 == x2:
        return set((x1, y) for y in range(min(y1, y2), max(y1, y2)+1))
    elif y1 == y2:
        return set((x, y1) for x in range(min(x1, x2), max(x1, x2)+1))
    else:
        l = abs(x1 - x2)
        return set(((x1*i + x2*(l-i))//l, (y1*i + y2*(l-i))//l) for i in range(0, l+1))


def get_intersections(lines_points):
    partial_sum = set()
    repeats = set()
    for line_points in lines_points:
        repeats |= partial_sum & line_points
        partial_sum |= line_points
    return repeats


def main():
    import sys
    lines = list(parse_input(sys.stdin))
    vh_lines = filter_vertical_horizontal(lines)
    lines_points = (get_points(line) for line in vh_lines)
    intersections = get_intersections(lines_points)
    print(len(intersections))
    lines_points = (get_points(line) for line in lines)
    intersections = get_intersections(lines_points)
    print(len(intersections))


if __name__ == '__main__':
    main()
