import sys
from enum import Enum
from typing import Iterator


class Tile(Enum):
    WALL = "#"
    BOX = "O"
    BOX_LEFT = "["
    BOX_RIGHT = "]"
    EMPTY = "."


class Direction(tuple[int, int], Enum):
    UP = (0, -1)
    DOWN = (0, 1)
    LEFT = (-1, 0)
    RIGHT = (1, 0)

    @classmethod
    def from_move(cls, move: str):
        return {
            "^": cls.UP,
            "v": cls.DOWN,
            "<": cls.LEFT,
            ">": cls.RIGHT,
        }[move]


type Warehouse = list[list[Tile]]


def parse_input(lines: Iterator[str]) -> tuple[Warehouse, tuple[int, int], str]:
    map_section = True
    warehouse_map = []
    movements = ""
    robot = (0, 0)
    for y, line in enumerate(lines):
        line = line.strip()
        if not line:
            map_section = False
        elif map_section:
            if "@" in line:
                robot = line.index("@"), y
            line = line.replace("@", ".")
            row = [Tile(val) for val in line]
            warehouse_map.append(row)
        else:
            movements += line
    return warehouse_map, robot, movements


def simulate_robot(warehouse: Warehouse, robot: tuple[int, int], movements: str) -> Warehouse:
    for move in movements:
        robot = move_robot(warehouse, robot, move)
    return warehouse


def move_robot(warehouse: Warehouse, robot: tuple[int, int], move: str) -> tuple[int, int]:
    direction = Direction.from_move(move)
    move_target = find_move_target(warehouse, robot, direction)
    if move_target is None:
        return robot
    new_robot = robot[0] + direction[0], robot[1] + direction[1]
    if move_target != new_robot:
        x, y = new_robot
        warehouse[y][x] = Tile.EMPTY
        tx, ty = move_target
        warehouse[ty][tx] = Tile.BOX
    return new_robot


def find_move_target(warehouse: Warehouse, robot: tuple[int, int], direction: Direction) -> tuple[int, int] | None:
    next_position = robot[0] + direction[0], robot[1] + direction[1]
    while at_pos(warehouse, next_position) == Tile.BOX:
        next_position = next_position[0] + direction[0], next_position[1] + direction[1]
    if at_pos(warehouse, next_position) in (Tile.WALL, None):
        return None
    else:
        return next_position


def get_gps(warehouse: Warehouse) -> Iterator[int]:
    for y, row in enumerate(warehouse):
        for x, tile in enumerate(row):
            if tile in (Tile.BOX, Tile.BOX_LEFT):
                yield 100*y + x


def scale_up(warehouse: Warehouse) -> Warehouse:
    new_warehouse = []
    for row in warehouse:
        new_row = []
        for tile in row:
            match tile:
                case Tile.WALL:
                    new_row.extend([Tile.WALL, Tile.WALL])
                case Tile.BOX:
                    new_row.extend([Tile.BOX_LEFT, Tile.BOX_RIGHT])
                case Tile.EMPTY:
                    new_row.extend([Tile.EMPTY, Tile.EMPTY])
                case _:
                    raise ValueError(f"Unknown tile {tile}")
        new_warehouse.append(new_row)
    return new_warehouse


def simulate_robot2(warehouse: Warehouse, robot: tuple[int, int], movements: str) -> Warehouse:
    for move in movements:
        robot = move_robot2(warehouse, robot, move)
    return warehouse


def move_robot2(warehouse: Warehouse, robot: tuple[int, int], move: str) -> tuple[int, int]:
    direction = Direction.from_move(move)
    new_robot, boxes = get_movements(warehouse, robot, direction)
    for box in reversed(boxes):
        warehouse[box[1]][box[0]] = Tile.EMPTY
        warehouse[box[1]][box[0] + 1] = Tile.EMPTY
        new_box = box[0] + direction[0], box[1] + direction[1]
        warehouse[new_box[1]][new_box[0]] = Tile.BOX_LEFT
        warehouse[new_box[1]][new_box[0] + 1] = Tile.BOX_RIGHT
    return new_robot


def get_movements(warehouse: Warehouse, robot: tuple[int, int], direction: Direction) -> tuple[tuple[int, int], list[tuple[int, int]]]:
    next_position = robot[0] + direction[0], robot[1] + direction[1]
    next_tile = at_pos(warehouse, next_position)
    boxes = []
    if next_tile == Tile.EMPTY:
        return next_position, []
    elif next_tile == Tile.WALL:
        return robot, []
    elif direction == Direction.LEFT:
        x, y = next_position
        while at_pos(warehouse, (x, y)) == Tile.BOX_RIGHT:
            boxes.append((x - 1, y))
            x -= 2
        if at_pos(warehouse, (x, y)) in (Tile.WALL, None):
            return robot, []
        else:
            return next_position, boxes
    elif direction == Direction.RIGHT:
        x, y = next_position
        while at_pos(warehouse, (x, y)) == Tile.BOX_LEFT:
            boxes.append((x, y))
            x += 2
        if at_pos(warehouse, (x, y)) in (Tile.WALL, None):
            return robot, []
        else:
            return next_position, boxes
    elif direction in (Direction.UP, Direction.DOWN):
        box_x, box_y = next_position
        if next_tile == Tile.BOX_RIGHT:
            box_x -= 1
        dy = direction[1]
        boxes = [(box_x, box_y)]
        layer = [(box_x, box_y)]
        while layer:
            boxes.extend(layer)
            layer = push_layer(warehouse, layer, dy)
        if layer is None:
            return robot, []
        else:
            return next_position, boxes


def push_layer(warehouse: Warehouse, layer: list[tuple[int, int]], dy: int) -> list[tuple[int, int]] | None:
    next_layer = set()
    for box in layer:
        next_position = box[0], box[1] + dy
        next_tile_left = at_pos(warehouse, next_position)
        right_position = next_position[0] + 1, next_position[1]
        next_tile_right = at_pos(warehouse, right_position)
        if Tile.WALL in (next_tile_left, next_tile_right):
            return None
        elif next_tile_left == Tile.BOX_LEFT:
            next_layer.add(next_position)
        else:
            if next_tile_left == Tile.BOX_RIGHT:
                next_layer.add((next_position[0] - 1, next_position[1]))
            if next_tile_right == Tile.BOX_LEFT:
                next_layer.add(right_position)
    return list(next_layer)


def at_pos(warehouse: Warehouse, position: tuple[int, int]) -> Tile | None:
    return warehouse[position[1]][position[0]] if is_inside(warehouse, position) else None


def is_inside(warehouse: Warehouse, position: tuple[int, int]) -> bool:
    return 0 <= position[0] < len(warehouse[0]) and 0 <= position[1] < len(warehouse)


def main():
    warehouse_map, robot, movements = list(parse_input(sys.stdin))
    warehouse2 = scale_up(warehouse_map)
    robot2 = robot[0] * 2, robot[1]
    print(sum(get_gps(simulate_robot(warehouse_map, robot, movements))))
    print(sum(get_gps(simulate_robot2(warehouse2, robot2, movements))))


if __name__ == '__main__':
    main()
