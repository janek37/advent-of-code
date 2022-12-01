import sys


def parse_input(lines):
    elves = []
    current_elf = []
    for line in lines:
        line = line.strip()
        if not line:
            elves.append(current_elf)
            current_elf = []
        else:
            current_elf.append(int(line))
    elves.append(current_elf)
    return elves


def most_calories(elves):
    return max(sum(elf) for elf in elves)


def top_three(elves):
    sorted_elves = sorted((sum(elf) for elf in elves), reverse=True)
    return sum(sorted_elves[:3])


if __name__ == '__main__':
    elves = parse_input(sys.stdin)
    print(most_calories(elves))
    print(top_three(elves))
