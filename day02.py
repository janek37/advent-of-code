import sys
from enum import Enum
from typing import Iterable, Tuple, Generator


class Outcome(Enum):
    WIN = 6
    DRAW = 3
    LOSS = 0

    @classmethod
    def from_str(cls, s):
        if s == 'X':
            return cls.LOSS
        if s == 'Y':
            return cls.DRAW
        if s == 'Z':
            return cls.WIN


class RPS(Enum):
    ROCK = 1
    PAPER = 2
    SCISSORS = 3

    @classmethod
    def winning_pairs(cls):
        return [
            (cls.ROCK, cls.SCISSORS),
            (cls.PAPER, cls.ROCK),
            (cls.SCISSORS, cls.PAPER),
        ]

    def outcome(self, other: "RPS") -> Outcome:
        if self == other:
            return Outcome.DRAW
        elif (self, other) in RPS.winning_pairs():
            return Outcome.WIN
        else:
            return Outcome.LOSS

    def play(self, outcome: Outcome):
        if outcome == Outcome.DRAW:
            return self
        elif outcome == Outcome.LOSS:
            return dict(RPS.winning_pairs())[self]
        elif outcome == Outcome.WIN:
            return {b: a for a, b in RPS.winning_pairs()}[self]

    def score(self, other: "RPS") -> int:
        return self.outcome(other).value + self.value

    @classmethod
    def from_str(cls, s: str):
        if s in 'AX':
            return cls.ROCK
        elif s in 'BY':
            return cls.PAPER
        elif s in 'CZ':
            return cls.SCISSORS


def parse_input(lines: Iterable[str]) -> Generator[Tuple[RPS, str], None, None]:
    for line in lines:
        they, me = line.split()
        yield RPS.from_str(they), me


def simple_plays(code: Iterable[Tuple[RPS, str]]) -> Generator[Tuple[RPS, RPS], None, None]:
    for they, me in code:
        yield they, RPS.from_str(me)


def advanced_plays(code: Iterable[Tuple[RPS, str]]) -> Generator[Tuple[RPS, RPS], None, None]:
    for they, me in code:
        yield they, they.play(Outcome.from_str(me))


def scores(plays: Iterable[Tuple[RPS, RPS]]) -> Generator[int, None, None]:
    for they, me in plays:
        yield me.score(they)


def main():
    code = list(parse_input(sys.stdin))
    print(sum(scores(simple_plays(code))))
    print(sum(scores(advanced_plays(code))))


if __name__ == '__main__':
    main()
