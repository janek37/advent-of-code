import sys

Platform = list[list[str]]


def tilt_north(platform: Platform) -> Platform:
    new_platform = [list(platform[0])]
    for row in platform[1:]:
        new_row = []
        for i, char in enumerate(row):
            if char != 'O' or new_platform[-1][i] != '.':
                new_row.append(char)
            else:
                new_row.append('.')
                for j in range(len(new_platform) - 1, -1, -1):
                    if new_platform[j][i] != '.':
                        new_platform[j + 1][i] = 'O'
                        break
                    if j == 0:
                        new_platform[j][i] = 'O'
        new_platform.append(new_row)
    return new_platform


def rotate_clockwise(platform: Platform) -> Platform:
    return [[row[i] for row in reversed(platform)] for i in range(len(platform[0]))]


def spin_cycle(platform: Platform) -> Platform:
    new_platform = platform
    for _ in range(4):
        new_platform = rotate_clockwise(tilt_north(new_platform))
    return new_platform


def get_total_load(platform: Platform) -> int:
    return sum(row.count('O') * (len(platform) - i) for i, row in enumerate(platform))


def main():
    platform = [list(line.rstrip('\n')) for line in sys.stdin]
    print(get_total_load(tilt_north(platform)))
    spinned_platform = platform
    spinned_platform_ahead = platform
    period = None
    for i in range(1, 300):
        spinned_platform = spin_cycle(spinned_platform)
        if period is None:
            spinned_platform_ahead = spin_cycle(spin_cycle(spinned_platform_ahead))
            if spinned_platform == spinned_platform_ahead:
                period = i
        elif (1_000_000_000 - i) % period == 0:
            print(get_total_load(spinned_platform))
            break


if __name__ == '__main__':
    main()
