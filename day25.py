def make_step(map_):
    map_, moved_east = move_east(map_)
    map_, moved_south = move_south(map_)
    return map_, moved_east or moved_south


def move_east(map_):
    width = len(map_[0])
    new_map = []
    moved = False
    for row in map_:
        new_row = list(row)
        for j, space in enumerate(row):
            if space == '>':
                next_j = (j+1) % width
                if row[next_j] == '.':
                    new_row[j] = '.'
                    new_row[next_j] = '>'
                    moved = True
        new_map.append(new_row)
    return new_map, moved


def move_south(map_):
    height = len(map_)
    width = len(map_[0])
    new_map = [list(row) for row in map_]
    moved = False
    for i in range(height):
        for j in range(width):
            if map_[i][j] == 'v':
                next_i = (i+1) % height
                if map_[next_i][j] == '.':
                    new_map[i][j] = '.'
                    new_map[next_i][j] = 'v'
                    moved = True
    return new_map, moved


def count_steps(map_):
    step_count = 0
    while True:
        # for row in map_:
        #     print(''.join(row))
        # print()
        step_count += 1
        map_, moved = make_step(map_)
        if not moved:
            return step_count


def main():
    import sys
    print(count_steps([line.strip() for line in sys.stdin]))


if __name__ == '__main__':
    main()
