import heapq
import sys
from collections import defaultdict
from collections.abc import Iterator
from typing import NamedTuple


class XYZ(NamedTuple):
    x: int
    y: int
    z: int


Brick = tuple[XYZ, XYZ]


def parse_input(lines: Iterator[str]) -> Iterator[Brick]:
    for line in lines:
        part1, part2 = line.split('~')
        yield XYZ(*map(int, part1.split(','))), XYZ(*map(int, part2.split(',')))


def settle(bricks: list[Brick]):
    sorted_bricks = sorted(bricks, key=lambda b: b[0].z, reverse=True)
    settled = []
    cubes = defaultdict(list)
    while sorted_bricks:
        brick = sorted_bricks.pop()
        x0 = brick[0].x
        x1 = brick[1].x
        y0 = brick[0].y
        y1 = brick[1].y
        z = brick[0].z
        while z > 1 and all(XYZ(x, y, z-1) not in cubes for x in range(x0, x1+1) for y in range(y0, y1+1)):
            z -= 1
        z0 = z
        z1 = z0 + brick[1].z - brick[0].z
        settled_brick = (XYZ(x0, y0, z), XYZ(x1, y1, z1))
        settled.append((XYZ(x0, y0, z), XYZ(x1, y1, z1)))
        for x in range(x0, x1+1):
            for y in range(y0, y1+1):
                for z in range(z0, z1+1):
                    cubes[XYZ(x, y, z)].append(settled_brick)
    bricks_above = {}
    bricks_below = defaultdict(set)
    for brick in settled:
        bricks_above[brick] = get_bricks_above(brick, cubes)
        for brick_above in bricks_above[brick]:
            bricks_below[brick_above].add(brick)
    return settled, bricks_above, bricks_below


def get_bricks_above(brick: Brick, cubes: dict[XYZ, list[Brick]]) -> set[Brick]:
    x0 = brick[0].x
    x1 = brick[1].x
    y0 = brick[0].y
    y1 = brick[1].y
    cubes_above = [XYZ(x, y, brick[1].z+1) for x in range(x0, x1+1) for y in range(y0, y1+1)]
    bricks_above = set()
    for cube in cubes_above:
        bricks_above.update(cubes[cube])
    return bricks_above


def count_safe_bricks(
    bricks: list[Brick], bricks_above: dict[Brick, set[Brick]], bricks_below: dict[Brick, set[Brick]]
) -> int:
    return sum(1 for brick in bricks if is_safe(brick, bricks_above, bricks_below))


def is_safe(brick: Brick, bricks_above: dict[Brick, set[Brick]], bricks_below: dict[Brick, set[Brick]]) -> bool:
    for brick_above in bricks_above[brick]:
        if len(bricks_below[brick_above]) == 1:
            return False
    return True


def get_chain_reaction(
    brick: Brick, bricks_above: dict[Brick, set[Brick]], bricks_below: dict[Brick, set[Brick]]
) -> int:
    if is_safe(brick, bricks_above, bricks_below):
        return 0
    falling = set()
    bricks_to_consider = [(brick[0].z, brick)]
    while bricks_to_consider:
        z, brick_to_consider = heapq.heappop(bricks_to_consider)
        if brick_to_consider != brick:
            if any(brick_below not in falling for brick_below in bricks_below[brick_to_consider]):
                continue
        falling.add(brick_to_consider)
        for brick_above in bricks_above[brick_to_consider]:
            heapq.heappush(bricks_to_consider, (brick_above[0].z, brick_above))
    return len(falling) - 1


def main():
    bricks = list(parse_input(line.rstrip('\n') for line in sys.stdin))
    settled, bricks_above, bricks_below = settle(bricks)
    print(count_safe_bricks(settled, bricks_above, bricks_below))
    print(sum(get_chain_reaction(brick, bricks_above, bricks_below) for brick in settled))


if __name__ == '__main__':
    main()
