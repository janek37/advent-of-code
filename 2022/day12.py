import sys
from heapq import heappush, heappop, heapify
from typing import Iterable

Vertex = tuple[int, int]


def parse_input(lines: Iterable[str]) -> tuple[list[list[int]], Vertex, Vertex]:
    start = (0, 0)
    end = (0, 0)
    heightmap = []
    for y, line in enumerate(lines):
        line = line.strip()
        if 'S' in line:
            start = (line.index('S'), y)
        if 'E' in line:
            end = (line.index('E'), y)
        line = line.replace('S', 'a').replace('E', 'z')
        heightmap.append([ord(ch) - ord('a') for ch in line])
    return heightmap, start, end


def vertices(heightmap: list[list[int]]) -> Iterable[Vertex]:
    return ((x, y) for x in range(len(heightmap[0])) for y in range(len(heightmap)))


def neighbors(heightmap: list[list[int]], vertex: Vertex) -> Iterable[Vertex]:
    vertex_x, vertex_y = vertex
    for delta_x, delta_y in [(0, 1), (0, -1), (1, 0), (-1, 0)]:
        neighbor_x = vertex_x + delta_x
        neighbor_y = vertex_y + delta_y
        if 0 <= neighbor_x < len(heightmap[0]) and 0 <= neighbor_y < len(heightmap):
            neighbor = neighbor_x, neighbor_y
            if get_height(heightmap, neighbor) + 1 >= get_height(heightmap, vertex):
                yield neighbor


def get_height(heightmap: list[list[int]], vertex: Vertex) -> int:
    return heightmap[vertex[1]][vertex[0]]


def dijkstra(graph: list[list[int]], source: Vertex) -> tuple[dict[Vertex, int], dict[Vertex, Vertex]]:
    q = []
    dist = {source: 0}
    prev = {}
    for v in vertices(graph):
        if v != source:
            dist[v] = sys.maxsize
        heappush(q, (dist[v], v))

    while q:
        _dist, u = heappop(q)

        for v in neighbors(graph, u):
            alt = dist[u] + 1
            if alt < dist[v]:
                dist[v] = alt
                prev[v] = u
                decrease_priority(q, v, alt)

    return dist, prev


def decrease_priority(q: list[tuple[int, Vertex]], v: Vertex, alt: int) -> None:
    for i, (dist, vertex) in enumerate(q):
        if vertex == v:
            q[i] = (alt, vertex)
            break
    heapify(q)


def get_path(prev: dict[Vertex, Vertex], start: Vertex, end: Vertex) -> list[Vertex]:
    path = []
    v = start
    while v != end:
        path.append(v)
        v = prev[v]
    return path


def main():
    data = sys.stdin.read()
    heightmap, start, end = parse_input(data.strip().split('\n'))
    dist, prev = dijkstra(heightmap, end)
    print(len(get_path(prev, start, end)))
    print(min(d for vertex, d in dist.items() if get_height(heightmap, vertex) == 0))


if __name__ == '__main__':
    main()
