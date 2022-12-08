import sys
from typing import Iterable, List, Tuple, Set


def parse_input(lines: Iterable[str]) -> Iterable[List[int]]:
    for line in lines:
        yield [int(ch) for ch in line.strip()]


def visible(row: Iterable[Tuple[int, int]]) -> Iterable[int]:
    tallest_so_far = -1
    for i, tree in row:
        if tree > tallest_so_far:
            tallest_so_far = tree
            yield i


def visible_from_direction(trees: List[List[int]], horizontal: bool, reverse: bool) -> Iterable[Tuple[int, int]]:
    step = -1 if reverse else 1
    if horizontal:
        for i, row in enumerate(trees):
            for visible_x in visible(list(enumerate(row))[::step]):
                yield visible_x, i
    else:  # vertical
        for x in range(len(trees[0])):
            column = (row[x] for row in trees)
            for visible_y in visible(list(enumerate(column))[::step]):
                yield x, visible_y


def all_visible(trees: List[List[int]]) -> Set[Tuple[int, int]]:
    v = set()
    for horizontal in (True, False):
        for reverse in (True, False):
            v = v.union(visible_from_direction(trees, horizontal, reverse))
    return v


def viewing_distance(height, row: Iterable[int]) -> int:
    dist = 0
    for tree in row:
        dist += 1
        if tree >= height:
            break
    return dist


def viewing_distance_from_direction(trees: List[List[int]], x: int, y: int, horizontal: bool, reverse: bool) -> int:
    step = -1 if reverse else 1
    height = trees[y][x]
    if horizontal:
        return viewing_distance(height, trees[y][x+step::step])
    else:  # vertical
        column = [row[x] for row in trees]
        return viewing_distance(height, column[y+step::step])


def scenic_score(trees: List[List[int]], x: int, y: int):
    score = 1
    for horizontal in (True, False):
        for reverse in (True, False):
            score = score * viewing_distance_from_direction(trees, x, y, horizontal, reverse)
    return score


def best_spot(trees: List[List[int]]) -> Tuple[int, int]:
    best_score = 0
    for y in range(len(trees)):
        for x in range(len(trees[0])):
            score = scenic_score(trees, x, y)
            if score > best_score:
                best_score = score
    return best_score


def main():
    trees = list(parse_input(sys.stdin))
    print(len(all_visible(trees)))
    print(best_spot(trees))


if __name__ == '__main__':
    main()
