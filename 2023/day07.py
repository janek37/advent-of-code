import sys
from collections import Counter
from collections.abc import Iterator
from typing import Iterable

Hand = tuple[int, int, int, int, int]

CARD_VALUES = {
    'A': 14,
    'K': 13,
    'Q': 12,
    'J': 11,
    'T': 10,
    '9': 9,
    '8': 8,
    '7': 7,
    '6': 6,
    '5': 5,
    '4': 4,
    '3': 3,
    '2': 2,
}


def parse_input(lines: Iterable[str]) -> Iterator[tuple[Hand, int]]:
    for line in lines:
        hand, bid = line.split()
        yield tuple(CARD_VALUES.get(char) for char in hand), int(bid)


def hand_strength(hand: Hand) -> tuple[list[int], Hand]:
    counts = [count for card, count in Counter(hand).most_common()]
    return counts, hand


def joker_hand_strength(hand: Hand) -> tuple[list[int], Hand]:
    counter = Counter(hand)
    joker = CARD_VALUES['J']
    if joker not in counter:
        return sorted(counter.values(), reverse=True), hand
    else:
        new_hand: Hand = tuple(0 if card == joker else card for card in hand)  # type: ignore
        best_cards = [card for card, count in counter.most_common()]
        if best_cards[0] == joker:
            joker_value = best_cards[1] if len(best_cards) > 1 else 14
        else:
            joker_value = best_cards[0]
        joker_hand = tuple(joker_value if card == joker else card for card in hand)
        joker_counts = [count for card, count in Counter(joker_hand).most_common()]
        return joker_counts, new_hand


def main():
    hands_bids = list(parse_input(line.rstrip('\n') for line in sys.stdin))
    sorted_hands = sorted(hands_bids, key=lambda hand_bid: hand_strength(hand_bid[0]))
    print(sum(bid * i for i, (hand, bid) in enumerate(sorted_hands, start=1)))
    sorted_joker_hands = sorted(hands_bids, key=lambda hand_bid: joker_hand_strength(hand_bid[0]))
    print(sum(bid * i for i, (hand, bid) in enumerate(sorted_joker_hands, start=1)))


if __name__ == '__main__':
    main()
