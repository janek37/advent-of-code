import sys
from collections.abc import Iterator
from typing import Iterable, NamedTuple


class Card(NamedTuple):
    id: int
    match_count: int

    @classmethod
    def from_numbers(cls, card_id: int, winning_numbers: set[int], numbers: set[int]) -> "Card":
        return cls(card_id, len(winning_numbers & numbers))


def parse_input(lines: Iterable[str]) -> Iterator[Card]:
    for line in lines:
        card_id = int(line.split(':')[0].split()[1])
        part1, part2 = line.split(': ')[1].split(' | ')
        winning_numbers = set(int(n) for n in part1.split())
        numbers = set(int(n) for n in part2.split())
        yield Card.from_numbers(card_id, winning_numbers, numbers)


def get_card_counts(cards: list[Card]) -> dict[int, int]:
    counts = {card.id: 1 for card in cards}
    for card in cards:
        for card_id in range(card.id + 1, card.id + card.match_count + 1):
            counts[card_id] += counts[card.id]
    return counts


def main():
    cards = list(parse_input(line.rstrip('\n') for line in sys.stdin))
    print(sum(1 << card.match_count >> 1 for card in cards))
    print(sum(get_card_counts(cards).values()))


if __name__ == '__main__':
    main()
