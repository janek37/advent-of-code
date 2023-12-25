import sys
from collections import defaultdict, Counter
from collections.abc import Iterator


def parse_input(lines: Iterator[str]) -> dict[str, list[str]]:
    graph = defaultdict(list)
    for line in lines:
        node, rest = line.split(': ')
        ends = rest.split(' ')
        for end in ends:
            graph[node].append(end)
            graph[end].append(node)
    return graph


def get_component_size(graph: dict[str, list[str]]) -> int:
    start = list(graph)[0]
    component = {start}
    middle_edges = {(start, end) for end in graph[start]}
    while len(middle_edges) > 3:
        next_node = min(
            set(edge[1] for edge in middle_edges),
            key=lambda node: sum(-1 if end in component else 1 for end in graph[node]),
        )
        component.add(next_node)
        for end in graph[next_node]:
            if end in component:
                middle_edges.remove((end, next_node))
            else:
                middle_edges.add((next_node, end))
    return len(component)


def main():
    graph = parse_input(line.rstrip('\n') for line in sys.stdin)
    n = get_component_size(graph)
    print(n * (len(graph) - n))


if __name__ == '__main__':
    main()
