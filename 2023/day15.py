import sys
from collections.abc import Iterator


def parse_input(lines: Iterator[str]) -> list[str]:
    line = next(lines)
    return line.split(',')


def get_hash(s: str) -> int:
    current_value = 0
    for char in s:
        current_value += ord(char)
        current_value = (current_value * 17) % 256
    return current_value


def process_step(step: str, boxes: list[list[tuple[str, int]]]) -> None:
    label = step[:-1] if '-' in step else step[:-2]
    box_no = get_hash(label)
    if '-' in step:
        remove_lens(label, boxes[box_no])
    else:
        insert_lens(int(step[-1]), label, boxes[box_no])


def remove_lens(label: str, box: list[tuple[str, int]]) -> None:
    for i, (lens_label, lens) in enumerate(box):
        if lens_label == label:
            del box[i]
            return


def insert_lens(lens: int, label: str, box: list[tuple[str, int]]) -> None:
    for i, (lens_label, _lens) in enumerate(box):
        if lens_label == label:
            box[i] = (label, lens)
            break
    else:
        box.append((label, lens))


def get_focusing_power(boxes: list[list[tuple[str, int]]]) -> int:
    return sum(
        sum(i * j * lens for j, (_label, lens) in enumerate(box, start=1))
        for i, box in enumerate(boxes, start=1)
    )


def main():
    steps = parse_input(line.rstrip('\n') for line in sys.stdin)
    print(sum(get_hash(step) for step in steps))
    boxes = [[] for _ in range(256)]
    for step in steps:
        process_step(step, boxes)
    print(get_focusing_power(boxes))


if __name__ == '__main__':
    main()
