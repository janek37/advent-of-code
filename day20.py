import sys
from typing import Iterable


def parse_input(lines: Iterable[str]) -> Iterable[int]:
    for line in lines:
        yield int(line)


def mix(f: list[int]) -> list[int]:
    # the boolean indicates whether the value was already moved
    temp_f = [(n, False) for n in f]
    i = 0
    while i < len(temp_f):
        if not temp_f[i][1]:
            value, _ = temp_f.pop(i)
            new_i = (i + value) % len(temp_f)
            temp_f.insert(new_i, (value, True))
        else:
            i += 1
    return [v for v, moved in temp_f]


def mix_with_indices(orig: list[tuple[int, int]], f: list[tuple[int, int]]):
    for orig_item in orig:
        i = f.index(orig_item)
        del f[i]
        new_i = (i + orig_item[1]) % len(f)
        f.insert(new_i, orig_item)


def get_coordinates(f: list[int]):
    zero_index = f.index(0)
    return sum(f[(zero_index + i) % len(f)] for i in [1000, 2000, 3000])


def main():
    encrypted = list(parse_input(line.strip() for line in sys.stdin))
    print(get_coordinates(mix(encrypted)))
    key = 811589153
    new_encrypted = list(enumerate(v * key for v in encrypted))
    orig_new_encrypted = list(new_encrypted)
    for i in range(10):
        mix_with_indices(orig_new_encrypted, new_encrypted)
    decrypted = [v for i, v in new_encrypted]
    print(get_coordinates(decrypted))


if __name__ == '__main__':
    main()
