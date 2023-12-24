import itertools
import sys
from collections.abc import Iterator
from typing import NamedTuple

import sympy


class XYZ(NamedTuple):
    x: int
    y: int
    z: int


def parse_input(lines: Iterator[str]) -> Iterator[tuple[XYZ, XYZ]]:
    for line in lines:
        position, velocity = line.split(" @ ")
        position = map(int, position.split(", "))
        velocity = map(int, velocity.split(", "))
        yield XYZ(*position), XYZ(*velocity)


def check_collision(stone1: tuple[XYZ, XYZ], stone2: tuple[XYZ, XYZ]) -> bool:
    area_start = 200000000000000
    area_end = 400000000000000
    pos1, v1 = stone1
    pos2, v2 = stone2
    # find t1, t2 such that (pos1.x + v1.x*t1, pos1.y + v1.y*t1) == (pos2.x + v2.x*t2, pos2.y + v2.y*t2)
    if v1.x*v2.y - v1.y*v2.x == 0:
        # assuming that the common path crosses the area
        return v1.x*(pos1.y - pos2.y) + v1.y*(pos2.x - pos1.x) == 0
    t2 = (v1.x*(pos1.y - pos2.y) + v1.y*(pos2.x - pos1.x))/(v1.x*v2.y - v1.y*v2.x)
    t1 = (pos2.x - pos1.x + v2.x * t2) / v1.x
    if t1 < 0 or t2 < 0:
        return False
    intersection = pos2.x + v2.x*t2, pos2.y + v2.y*t2
    return area_start <= intersection[0] <= area_end and area_start <= intersection[1] <= area_end


def find_stone_position(hailstones: list[tuple[XYZ, XYZ]]) -> XYZ:
    pos1, v1 = hailstones[0]
    pos2, v2 = hailstones[1]
    pos3, v3 = hailstones[2]
    posx, posy, posz, vx, vy, vz, t1, t2, t3 = sympy.symbols("posx posy posz vx vy vz t1 t2 t3", real=True)
    # 9 equations, 9 unknowns
    equations = [
        sympy.Eq(posx + vx * t1, pos1.x + v1.x * t1),
        sympy.Eq(posy + vy * t1, pos1.y + v1.y * t1),
        sympy.Eq(posz + vz * t1, pos1.z + v1.z * t1),
        sympy.Eq(posx + vx * t2, pos2.x + v2.x * t2),
        sympy.Eq(posy + vy * t2, pos2.y + v2.y * t2),
        sympy.Eq(posz + vz * t2, pos2.z + v2.z * t2),
        sympy.Eq(posx + vx * t3, pos3.x + v3.x * t3),
        sympy.Eq(posy + vy * t3, pos3.y + v3.y * t3),
        sympy.Eq(posz + vz * t3, pos3.z + v3.z * t3),
    ]
    solution = sympy.solve(equations)[0]  # there's only one solution for my input
    return XYZ(solution[posx], solution[posy], solution[posz])


def main():
    hailstones = list(parse_input(line.rstrip('\n') for line in sys.stdin))
    print(sum(1 for stone1, stone2 in itertools.combinations(hailstones, 2) if check_collision(stone1, stone2)))
    print(sum(find_stone_position(hailstones)))


if __name__ == '__main__':
    main()
