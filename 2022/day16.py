import re
import sys
from typing import Iterable

Valves = dict[str, tuple[int, list[str]]]
Route = list[tuple[int, str]]

LINE_REGEX = re.compile(r'Valve ([A-Z]+) has flow rate=(\d+); tunnels? leads? to valves? ([A-Z, ]+)')


def parse_input(lines: Iterable[str]) -> Iterable[tuple[str, tuple[int, list[str]]]]:
    for line in lines:
        valve_id, flow_rate, tunnels = LINE_REGEX.match(line).groups()
        yield valve_id, (int(flow_rate), tunnels.split(', '))


def get_distances(valves: Valves, start: str, ends: Iterable[str]) -> dict[str, int]:
    ends = set(ends)
    distances = {}
    distance = 0
    visited = set()
    current_level = [start]
    while ends:
        next_level = set()
        for valve_id in current_level:
            if valve_id in visited:
                continue
            if valve_id in ends:
                distances[valve_id] = distance
                ends.discard(valve_id)
            for next_valve in valves[valve_id][1]:
                next_level.add(next_valve)
            visited.add(valve_id)
        distance += 1
        current_level = next_level
    return distances


def get_all_distances(valves: Valves, start_valves: list[str], end_valves: list[str]) -> dict[str, dict[str, int]]:
    return {
        valve_id: get_distances(valves, valve_id, end_valves)
        for valve_id in start_valves
    }


def find_routes(
    distances: dict[str, dict[str, int]],
    start: str,
    budget: int,
    visited: set[str],
) -> Iterable[Route]:
    found_any = False
    for next_valve_id, distance in distances[start].items():
        if next_valve_id not in visited and distance < budget - 1:
            for route in find_routes(distances, next_valve_id, budget - distance - 1, visited.union([next_valve_id])):
                yield [(distance, next_valve_id)] + route
            found_any = True
    if not found_any:
        yield []


def evaluate_route(route: Route, flow_rates: dict[str, int], time: int) -> int:
    time_left = time
    total_pressure = 0
    for distance, valve_id in route:
        time_left -= distance + 1
        total_pressure += flow_rates[valve_id] * time_left
    return total_pressure


_memoized_upper_bounds = {}


def quick_upper_bound(
    distances: dict[str, dict[str, int]],
    flow_rates: dict[str, int],
    starts: tuple[str, str],
    budgets: tuple[int, int],
    not_found: bool = False,
) -> int:
    key = tuple(sorted(zip(starts, budgets)))
    if key in _memoized_upper_bounds:
        return _memoized_upper_bounds[key]

    start = starts[0]
    budget = budgets[0]
    best_so_far = None
    for valve_id, distance in distances[start].items():
        if 0 < distance < budget - 1:
            new_budget = budget - distance - 1
            this_value = flow_rates[valve_id] * new_budget
            value = quick_upper_bound(
                distances,
                flow_rates,
                (starts[1], valve_id),
                (budgets[1], new_budget),
            )
            total_value = this_value + value
            if best_so_far is None or total_value > best_so_far:
                best_so_far = total_value
    if best_so_far:
        result = best_so_far
    else:
        if not_found:
            result = 0
        else:
            result = quick_upper_bound(
                distances,
                flow_rates,
                (starts[1], start),
                (budgets[1], budget),
                not_found=True,
            )
    _memoized_upper_bounds[key] = result
    return result


def find_double_routes(
    distances: dict[str, dict[str, int]],
    flow_rates: dict[str, int],
    starts: tuple[str, str],
    budgets: tuple[int, int],
    visited: set[str],
    not_found: bool = False,
    best_so_far: int | None = None,
) -> int:
    budget = budgets[0]
    start = starts[0]
    sorted_next_valve_ids = sorted(
        (
            (flow_rates[valve_id] * (budget - dist - 1), valve_id, dist)
            for valve_id, dist in distances[start].items()
            if valve_id not in visited and dist < budget - 1
        ),
        reverse=True,
    )
    found_any = False
    for _, valve_id, distance in sorted_next_valve_ids:
        new_visited = visited.union([valve_id])
        new_budget = budget - distance - 1
        this_value = flow_rates[valve_id] * new_budget
        if best_so_far is not None:
            upper_bound = this_value + quick_upper_bound(
                distances,
                flow_rates,
                (valve_id, starts[1]),
                (new_budget, budgets[1]),
            )
            if upper_bound < best_so_far:
                continue
        value = find_double_routes(
            distances,
            flow_rates,
            (starts[1], valve_id),
            (budgets[1], new_budget),
            new_visited,
            best_so_far=best_so_far - this_value,
        )
        total_value = this_value + value
        if best_so_far is None or total_value > best_so_far:
            best_so_far = total_value
            found_any = True
    if found_any:
        return best_so_far
    else:
        if not_found:
            return 0
        else:
            return find_double_routes(
                distances,
                flow_rates,
                (starts[1], start),
                (budgets[1], budget),
                visited,
                not_found=True,
                best_so_far=best_so_far,
            )


def main():
    valves = dict(parse_input(line.strip() for line in sys.stdin))
    flow_rates = {valve_id: flow_rate for valve_id, (flow_rate, tunnels) in valves.items() if flow_rate > 0}
    distances = get_all_distances(valves, ['AA'] + list(flow_rates), list(flow_rates))
    most_pressure = max(evaluate_route(route, flow_rates, 30) for route in find_routes(distances, 'AA', 30, {'AA'}))
    print(most_pressure)
    print(find_double_routes(distances, flow_rates, ('AA', 'AA'), (26, 26), {'AA'}, best_so_far=most_pressure))


if __name__ == '__main__':
    main()
