import sys
from collections import defaultdict
from typing import Iterator


def parse_input(lines: Iterator[str]) -> Iterator[tuple[str, str]]:
    for line in lines:
        node1, node2 = line.strip().split("-")
        yield node1, node2


def find_triangles_with_prefix(edges: list[tuple[str, str]], prefix: str) -> Iterator[tuple[str, str, str]]:
    neighbor_sets = get_neighbor_sets(edges)
    for edge in edges:
        for node in edge:
            if node.startswith(prefix):
                for third_node in neighbor_sets[edge[0]] & neighbor_sets[edge[1]]:
                    yield tuple(sorted([*edge, third_node]))


def find_largest_clique(edges: list[tuple[str, str]]) -> list[str]:
    neighbor_sets = get_neighbor_sets(edges)
    return _find_largest_clique(neighbor_sets, [], set(neighbor_sets))


def _find_largest_clique(neighbor_sets: dict[str, set[str]], clique: list[str], candidates: set[str]) -> list[str]:
    if not candidates:
        return clique
    maximal_cliques = []
    for node in candidates:
        new_clique = [*clique, node]
        new_candidates = candidates & neighbor_sets[node] - set(new_clique)
        new_candidates = {n for n in new_candidates if n > node}
        maximal_cliques.append(_find_largest_clique(neighbor_sets, new_clique, new_candidates))
    return max(maximal_cliques, key=len)


def get_neighbor_sets(edges: list[tuple[str, str]]) -> dict[str, set[str]]:
    neighbors = defaultdict(set)
    for edge in edges:
        node1, node2 = edge
        neighbors[node1].add(node2)
        neighbors[node2].add(node1)
    return neighbors


def main():
    network = list(parse_input(sys.stdin))
    print(len(set(find_triangles_with_prefix(network, "t"))))
    print(",".join(find_largest_clique(network)))


if __name__ == '__main__':
    main()
