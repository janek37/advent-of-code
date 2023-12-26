from collections import defaultdict
from dataclasses import dataclass
from itertools import cycle


def add_pos(pos, steps):
    return (pos + steps - 1) % 10 + 1


@dataclass
class Player:
    position: int
    score: int = 0

    def move(self, steps):
        self.position = add_pos(self.position, steps)
        self.score += self.position


class Die:
    def __init__(self):
        self.counter = 0
        self.rolls = 0

    def roll(self):
        self.counter += 1
        self.counter %= 10
        self.rolls += 1
        return self.counter


def play(starting_spaces):
    players = [Player(starting_spaces[0]), Player(starting_spaces[1])]
    die = Die()
    for player_no in cycle([0, 1]):
        player = players[player_no]
        player.move(die.roll() + die.roll() + die.roll())
        if player.score >= 1000:
            other_player = players[1 - player_no]
            return other_player.score, die.rolls


MULTIROLLS = [(3, 1), (4, 3), (5, 6), (6, 7), (7, 6), (8, 3), (9, 1)]


# state = ((player1_pos, player1_score), (player2_pos, player2_score))

def multimove(multistate, player_no):
    new_multistate = defaultdict(int)
    for state, unicount in multistate.items():
        if state[0] == 'win':
            new_multistate[state] += unicount
            continue
        for roll, spread in MULTIROLLS:
            new_state = list(state)
            new_unicount = unicount * spread
            pos, score = state[player_no]
            new_pos = add_pos(pos, roll)
            new_score = score + new_pos
            if new_score >= 21:
                new_state = ('win', player_no)
            else:
                new_state[player_no] = (new_pos, new_score)
                new_state = tuple(new_state)
            new_multistate[new_state] += new_unicount
    return new_multistate


def multiplay(multistate):
    for player_no in cycle([0, 1]):
        multistate = multimove(multistate, player_no)
        if len(multistate) == 2:
            return max(multistate.values())


def main():
    score, rolls = play([7, 3])
    print(score * rolls)
    print(multiplay({((7, 0), (3, 0)): 1}))


if __name__ == '__main__':
    main()
