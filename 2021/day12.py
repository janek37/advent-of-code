from collections import defaultdict


def get_next_nodes(path, edges):
    for next_node in edges[path[-1]]:
        if next_node not in path or next_node.isupper():
            yield next_node


def get_next_nodes2(path, edges):
    for next_node in edges[path[0][-1]]:
        if (not path[1] and next_node not in ('start', 'end')) or (next_node not in path[0] or next_node.isupper()):
            yield next_node


def append_path(path, next_node):
    new_path = path[0] + [next_node]
    if next_node.islower() and next_node in path[0]:
        assert not path[1]
        return new_path, True
    else:
        return new_path, path[1]


def find_paths(edges, path=None):
    if path is None:
        path = ['start']
    for next_node in get_next_nodes(path, edges):
        new_path = path + [next_node]
        if next_node == 'end':
            yield new_path
        else:
            yield from find_paths(edges, new_path)


def find_paths2(edges, path=None):
    if path is None:
        path = (['start'], False)
    for next_node in get_next_nodes2(path, edges):
        new_path = append_path(path, next_node)
        if next_node == 'end':
            yield new_path
        else:
            yield from find_paths2(edges, new_path)


def parse_input(lines):
    edges = defaultdict(list)
    for line in lines:
        node1, node2 = line.strip().split('-')
        edges[node1].append(node2)
        edges[node2].append(node1)
    return edges


def main():
    import sys
    edges = parse_input(sys.stdin)
    print(sum(1 for p in find_paths(edges)))
    print(sum(1 for p in find_paths2(edges)))


if __name__ == '__main__':
    main()

