# #############
# #01.2.3.4.56#
# ###7#9#B#D###
#   #8#A#C#E#
#   #########

HOMES_ITEMS = [
    ('A', [7, 8]),
    ('B', [9, 10]),
    ('C', [11, 12]),
    ('D', [13, 14]),
]

HOMES2_ITEMS = [
    ('A', [7, 8, 9, 10]),
    ('B', [11, 12, 13, 14]),
    ('C', [15, 16, 17, 18]),
    ('D', [19, 20, 21, 22]),
]

HOMES = [dict(HOMES_ITEMS), dict(HOMES2_ITEMS)]

COST = {
    'A': 1,
    'B': 10,
    'C': 100,
    'D': 1000,
}

GOAL = [list('AABBCCDD'), list('AAAABBBBCCCCDDDD')]


# (from, to), distance, passing
def make_moves_from(start, homes):
    if start == 0:
        for (s, e), distance, passing in make_moves_from(1, homes):
            yield (0, e), distance+1, [1] + passing
        return
    elif start == 6:
        for (s, e), distance, passing in make_moves_from(5, homes):
            yield (6, e), distance+1, [5] + passing
        return
    elif start == 1:
        base = ([], [2], [2, 3], [2, 3, 4])
    elif start == 2:
        base = ([], [], [3], [3, 4])
    elif start == 3:
        base = ([2], [], [], [4])
    elif start == 4:
        base = ([3, 2], [3], [], [])
    elif start == 5:
        base = ([4, 3, 2], [4, 3], [4], [])

    for home, passing in zip(homes, base):
        distance = len(passing)*2 + 2
        for i, end in enumerate(home[1]):
            yield (start, end), distance+i, passing + home[1][:i]


def make_moves(homes):
    for start in range(7):
        for (s, e), distance, passing in make_moves_from(start, homes):
            yield (s, e), distance, passing
            yield (e, s), distance, passing


MOVES = [list(make_moves(HOMES_ITEMS)), list(make_moves(HOMES2_ITEMS))]


def is_legal(move, state, part):
    (start, end), _distance, passing = move
    if state[start] == '.' or state[end] != '.':
        return False
    amphipod = state[start]
    home = HOMES[part][amphipod]
    if any(state[p] != '.' for p in passing):
        return False
    if end > 6 and end not in home:
        return False
    if end in home:
        end_index = home.index(end)
        if any(state[h] != amphipod for h in home[end_index+1:]):
            return False
    if start in home:
        start_index = home.index(start)
        if all(state[h] == amphipod for h in home[start_index+1:]):
            return False
    return True


def possible_moves(state, part):
    for move in MOVES[part]:
        if is_legal(move, state, part):
            yield move


def play_move(move, state):
    (start, end), distance, _passing = move
    new_state = list(state)
    new_state[end] = new_state[start]
    new_state[start] = '.'
    return new_state, distance * COST[state[start]]


def cost_candidates(state, part):
    for move in possible_moves(state, part):
        new_state, cost = play_move(move, state)
        partial_cost = find_lowest_cost(new_state, part)
        if partial_cost is not None:
            yield partial_cost + cost


memoized = {}


def find_lowest_cost(state, part):
    if (tuple(state), part) in memoized:
        return memoized[(tuple(state), part)]
    if state[7:] == GOAL[part]:
        result = 0
    else:
        candidates = list(cost_candidates(state, part))
        if not candidates:
            result = None
        else:
            result = min(candidates)
    memoized[(tuple(state), part)] = result
    return result


def main():
    state = list('.......BBACADDC')
    # state = list('.......BACDBCDA')
    print(find_lowest_cost(state, part=0))
    state = list('.......BDDBACBCABADDACC')
    # state = list('.......BDDACCBDBBACDACA')
    print(find_lowest_cost(state, part=1))


if __name__ == '__main__':
    main()
