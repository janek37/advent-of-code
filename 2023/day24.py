import itertools
import sys
from collections.abc import Iterator
from fractions import Fraction
from typing import NamedTuple


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
    # Reddit suggested using cross products:
    # pos + v*t == pos1 + v1*t is equivalent to (pos - pos1) x (v - v1) == 0
    # so pos x (v1 - v2) + (pos1 - pos2) x v + pos2 x v2 - pos1 x v1 == 0

    cp1 = cross_product(pos1, v1)
    cp2 = cross_product(pos2, v2)
    cp3 = cross_product(pos3, v3)
    linear_system = [
        [0, v1.z - v2.z, v2.y - v1.y, 0, pos2.z - pos1.z, pos1.y - pos2.y, cp2.x - cp1.x],
        [v2.z - v1.z, 0, v1.x - v2.x, pos1.z - pos2.z, 0, pos2.x - pos1.x, cp2.y - cp1.y],
        [v1.y - v2.y, v2.x - v1.x, 0, pos2.y - pos1.y, pos1.x - pos2.x, 0, cp2.z - cp1.z],
        [0, v1.z - v3.z, v3.y - v1.y, 0, pos3.z - pos1.z, pos1.y - pos3.y, cp3.x - cp1.x],
        [v3.z - v1.z, 0, v1.x - v3.x, pos1.z - pos3.z, 0, pos3.x - pos1.x, cp3.y - cp1.y],
        [v1.y - v3.y, v3.x - v1.x, 0, pos3.y - pos1.y, pos1.x - pos3.x, 0, cp3.z - cp1.z],
    ]
    solution = solve_linear_system(linear_system)
    assert all(sol.denominator == 1 for sol in solution[:3])
    return XYZ(int(solution[0]), int(solution[1]), int(solution[2]))


def solve_linear_system(linear_system: list[list[int]]) -> list[Fraction]:
    n = len(linear_system)
    coefficients = []
    while linear_system:
        new_coef, linear_system = gaussian_elimination(linear_system)
        coefficients.append(new_coef)
    solution = []
    for var_no in range(n - 1, -1, -1):
        solution.append(sum(coef * sol for coef, sol in zip(coefficients[var_no], solution[n - var_no - 2::-1] + [1])))
    return solution[::-1]


def gaussian_elimination(linear_system: list[list[Fraction | int]]) -> tuple[list[Fraction], list[list[Fraction]]]:
    row_to_eliminate = next(row for row in linear_system if row[0] != 0)
    coefficients = [Fraction(-value, row_to_eliminate[0]) for value in row_to_eliminate[1:]]
    new_system = []
    for row in linear_system:
        if row != row_to_eliminate:
            new_system.append([value + row[0]*coefficient for value, coefficient in zip(row[1:], coefficients)])
    return coefficients, new_system


def cross_product(a: XYZ, b: XYZ) -> XYZ:
    return XYZ(a.y*b.z - a.z*b.y, a.z*b.x - a.x*b.z, a.x*b.y - a.y*b.x)


def main():
    hailstones = list(parse_input(line.rstrip('\n') for line in sys.stdin))
    print(sum(1 for stone1, stone2 in itertools.combinations(hailstones, 2) if check_collision(stone1, stone2)))
    print(sum(find_stone_position(hailstones)))


if __name__ == '__main__':
    main()
