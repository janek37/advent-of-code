from queue import Queue


def parse_input(lines):
    return [[int(d) for d in line.strip()] for line in lines]


def adjacent_locations(heightmap, x, y):
    x_size = len(heightmap[0])
    y_size = len(heightmap)
    for (dx, dy) in [(-1, 0), (1, 0), (0, -1), (0, 1)]:
        if 0 <= x+dx < x_size and 0 <= y+dy < y_size:
            yield x+dx, y+dy


def adjacent_heights(heightmap, x, y):
    for x1, y1 in adjacent_locations(heightmap, x, y):
        yield heightmap[y1][x1]


def is_low_point(heightmap, x, y):
    return all(h > heightmap[y][x] for h in adjacent_heights(heightmap, x, y))


def risk_levels(heightmap):
    for y, row in enumerate(heightmap):
        for x, height in enumerate(row):
            if is_low_point(heightmap, x, y):
                yield height + 1


def basin_size(heightmap, x, y):
    basin = {(x, y)}
    checked = set()
    left_to_check = Queue()
    left_to_check.put((x, y))
    while not left_to_check.empty():
        x1, y1 = left_to_check.get()
        if (x1, y1) not in checked and heightmap[y1][x1] != 9:
            basin.add((x1, y1))
            for x2, y2 in adjacent_locations(heightmap, x1, y1):
                if (x2, y2) not in checked:
                    left_to_check.put((x2, y2))
        checked.add((x1, y1))
    return len(basin)


def basin_sizes(heightmap):
    for y, row in enumerate(heightmap):
        for x, height in enumerate(row):
            if is_low_point(heightmap, x, y):
                yield basin_size(heightmap, x, y)


def main():
    import sys
    heightmap = parse_input(sys.stdin)
    print(sum(risk_levels(heightmap)))
    sorted_basin_sizes = sorted(basin_sizes(heightmap), reverse=True)
    # print(sorted_basin_sizes)
    print(sorted_basin_sizes[0]*sorted_basin_sizes[1]*sorted_basin_sizes[2])


if __name__ == '__main__':
    main()
