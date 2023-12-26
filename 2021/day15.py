import heapq
from itertools import product


def dijkstra(cave, x, y):
    start = (x, y)
    priority_queue = [(0, (len(cave[0])-1, len(cave)-1))]
    risk_map = [[float('infinity')]*len(cave[0]) for _ in cave]
    while priority_queue:
        risk, (x, y) = heapq.heappop(priority_queue)
        if (x, y) == start:
            return risk
        for x1, y1 in neighbors(cave, x, y):
            new_risk = risk + cave[y][x]
            if new_risk < risk_map[y1][x1]:
                risk_map[y1][x1] = new_risk
                heapq.heappush(priority_queue, (new_risk, (x1, y1)))


def neighbors(cave, x, y):
    if y+1 < len(cave):
        yield x, y+1
    if x+1 < len(cave[0]):
        yield x+1, y
    if x > 0:
        yield x-1, y
    if y > 0:
        yield x, y-1


# region my stupid O(V^3) algorithm
def low_total_risk_map(cave, previous_risk_map=None):
    risk_map = [[None]*len(cave[0]) for _ in cave]
    for y in range(len(cave)-1, -1, -1):
        for x in range(len(cave[y])-1, -1, -1):
            options = [risk_map[y1][x1] + cave[y1][x1] for x1, y1 in forward(cave, x, y)]
            options = options or [0]
            if previous_risk_map:
                options += [previous_risk_map[y1][x1] + cave[y1][x1] for x1, y1 in backward(x, y)]
            risk_map[y][x] = min(options)
    return risk_map


def lowest_total_risk(cave, x, y):
    risk_map = low_total_risk_map(cave)
    old_risk_map = None
    i = 0
    while old_risk_map != risk_map:
        i += 1
        old_risk_map = risk_map
        risk_map = low_total_risk_map(cave, risk_map)
        print(i)
        # print(risk_map)
    return risk_map[y][x]


def forward(cave, x, y):
    if y+1 < len(cave):
        yield x, y+1
    if x+1 < len(cave[0]):
        yield x+1, y


def backward(x, y):
    if y > 0:
        yield x, y-1
    if x > 0:
        yield x-1, y
# endregion


def large_cave(cave):
    new_cave = []
    for i, row in product(range(5), cave):
        new_row = []
        for j, risk in product(range(5), row):
            new_risk = risk + j + i
            while new_risk > 9:
                new_risk -= 9
            new_row.append(new_risk)
        new_cave.append(new_row)
    return new_cave


def main():
    import sys
    cave = [[int(d) for d in line.strip()] for line in sys.stdin]
    print(dijkstra(cave, 0, 0))
    print(dijkstra(large_cave(cave), 0, 0))


if __name__ == '__main__':
    main()
