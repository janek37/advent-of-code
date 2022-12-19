import re
import sys
from dataclasses import dataclass
from enum import Enum
from functools import cache
from typing import Iterable, NamedTuple

BLUEPRINT_REGEX = re.compile(
    r'Blueprint (\d+): '
    r'Each ore robot costs (\d+) ore. '
    r'Each clay robot costs (\d+) ore. '
    r'Each obsidian robot costs (\d+) ore and (\d+) clay. '
    r'Each geode robot costs (\d+) ore and (\d+) obsidian.'
)


class Inventory(NamedTuple):
    ore: int = 0
    clay: int = 0
    obsidian: int = 0
    ore_robots: int = 0
    clay_robots: int = 0
    obsidian_robots: int = 0

    def add_resources(
        self,
        ore: int = 0,
        clay: int = 0,
        obsidian: int = 0,
        ore_robots: int = 0,
        clay_robots: int = 0,
        obsidian_robots: int = 0,
    ) -> "Inventory":
        return Inventory(
            self.ore + ore,
            self.clay + clay,
            self.obsidian + obsidian,
            self.ore_robots + ore_robots,
            self.clay_robots + clay_robots,
            self.obsidian_robots + obsidian_robots,
        )

    def produce(self) -> "Inventory":
        return self.add_resources(ore=self.ore_robots, clay=self.clay_robots, obsidian=self.obsidian_robots)


class RobotType(Enum):
    ORE = 0
    CLAY = 1
    OBSIDIAN = 2
    GEODE = 3


@dataclass
class Blueprint:
    id: int
    ore_robot_cost: int
    clay_robot_cost: int
    obsidian_robot_cost_ore: int
    obsidian_robot_cost_clay: int
    geode_robot_cost_ore: int
    geode_robot_cost_obsidian: int

    def quality_level(self, time: int) -> int:
        return self.id * self.max_geode_count(time)

    def max_geode_count(self, time: int) -> int:
        return RobotFactory(self).max_geode_count(time, Inventory(ore_robots=1))

    def can_afford(self, robot_type: RobotType, inventory: Inventory) -> bool:
        match robot_type:
            case RobotType.ORE:
                return self.ore_robot_cost <= inventory.ore
            case RobotType.CLAY:
                return self.clay_robot_cost <= inventory.ore
            case RobotType.OBSIDIAN:
                return (
                    self.obsidian_robot_cost_ore <= inventory.ore
                    and self.obsidian_robot_cost_clay <= inventory.clay
                )
            case RobotType.GEODE:
                return (
                    self.geode_robot_cost_ore <= inventory.ore
                    and self.geode_robot_cost_obsidian <= inventory.obsidian
                )

    def build_robot(self, robot_type: RobotType, inventory: Inventory) -> Inventory:
        match robot_type:
            case RobotType.ORE:
                return inventory.add_resources(ore=-self.ore_robot_cost, ore_robots=1)
            case RobotType.CLAY:
                return inventory.add_resources(ore=-self.clay_robot_cost, clay_robots=1)
            case RobotType.OBSIDIAN:
                return inventory.add_resources(
                    ore=-self.obsidian_robot_cost_ore, clay=-self.obsidian_robot_cost_clay, obsidian_robots=1
                )
            case RobotType.GEODE:
                return inventory.add_resources(
                    ore=-self.geode_robot_cost_ore, obsidian=-self.geode_robot_cost_obsidian
                )


class RobotFactory:
    def __init__(self, blueprint: Blueprint):
        self.blueprint = blueprint
        self._memoized_max_geode_count = {}

    @cache
    def quick_upper_bound(self, time: int, obsidian: int, obsidian_robots: int) -> int:
        # assume unlimited ore and clay
        if time <= 1:
            return 0
        obsidian_after_production = obsidian + obsidian_robots
        best = self.quick_upper_bound(
            time - 1, obsidian_after_production, obsidian_robots + 1
        )
        if obsidian >= self.blueprint.geode_robot_cost_obsidian:
            geode_count = time - 1 + self.quick_upper_bound(
                time - 1, obsidian_after_production - self.blueprint.geode_robot_cost_obsidian, obsidian_robots
            )
            if geode_count > best:
                best = geode_count
        return best

    def max_geode_count(self, time: int, inventory: Inventory, best_to_beat: int = 0) -> int:
        if time <= 1:
            return 0
        key = self.key(time, inventory)
        if key in self._memoized_max_geode_count:
            return self._memoized_max_geode_count[key]
        obsidian_potential = inventory.obsidian + (time - 2) * inventory.obsidian_robots + (time - 2) * (time - 3) // 2
        if obsidian_potential < self.blueprint.geode_robot_cost_obsidian:
            return 0
        if self.quick_upper_bound(time, inventory.obsidian, inventory.obsidian_robots) < best_to_beat:
            return 0
        inventory_after_production = inventory.produce()
        best = self.max_geode_count(time - 1, inventory_after_production, best_to_beat)
        if best > best_to_beat:
            best_to_beat = best
        for robot_type in RobotType:
            geode_count = 0
            if self.blueprint.can_afford(robot_type, inventory) and self.need_more(robot_type, time, inventory):
                new_inventory = self.blueprint.build_robot(robot_type, inventory_after_production)
                if robot_type == RobotType.GEODE:
                    geode_count += time - 1
                geode_count += self.max_geode_count(time - 1, new_inventory, best_to_beat - geode_count)
                if geode_count > best:
                    best = geode_count
                    if best > best_to_beat:
                        best_to_beat = best
        self._memoized_max_geode_count[key] = best
        return best

    def need_more(self, robot_type: RobotType, time: int, inventory: Inventory) -> bool:
        if time == 2 and robot_type != RobotType.GEODE:
            return False
        if time == 3 and robot_type == RobotType.CLAY:
            return False
        match robot_type:
            case RobotType.ORE:
                return inventory.ore < self.max_ore_spending(time) and inventory.ore_robots < self.max_ore_cost()
            case RobotType.CLAY:
                return (
                    inventory.clay < self.max_clay_spending(time)
                    and inventory.clay_robots < self.blueprint.obsidian_robot_cost_clay
                )
            case _:  # it's really hard to have too much obsidian
                return True

    def max_ore_spending(self, time: int) -> int:
        return self.max_ore_cost() * (time - 2) + self.blueprint.geode_robot_cost_ore

    def max_ore_cost(self) -> int:
        # ore robot cost intentionally omitted
        return max(
            self.blueprint.clay_robot_cost,
            self.blueprint.obsidian_robot_cost_ore,
            self.blueprint.geode_robot_cost_ore,
        )

    def max_clay_spending(self, time: int) -> int:
        return self.blueprint.obsidian_robot_cost_clay * (time - 3)

    def max_obsidian_spending(self, time: int) -> int:
        return self.blueprint.geode_robot_cost_obsidian * (time - 1)

    def key(self, time: int, inventory: Inventory) -> tuple[int, Inventory]:
        key_inventory = Inventory(
            ore=min(inventory.ore, self.max_ore_spending(time)),
            clay=min(inventory.clay, self.max_clay_spending(time)),
            obsidian=min(inventory.obsidian, self.max_obsidian_spending(time)),
            ore_robots=inventory.ore_robots,
            clay_robots=inventory.clay_robots,
            obsidian_robots=inventory.obsidian_robots,
        )
        return time, key_inventory


def parse_input(lines: Iterable[str]):
    for line in lines:
        yield Blueprint(*(int(x) for x in BLUEPRINT_REGEX.match(line).groups()))


def main():
    blueprints = list(parse_input(line.strip() for line in sys.stdin))
    print(sum(blueprint.quality_level(24) for blueprint in blueprints))
    print(blueprints[0].max_geode_count(32) * blueprints[1].max_geode_count(32) * blueprints[2].max_geode_count(32))


if __name__ == '__main__':
    main()
