import sys
from functools import cache
from itertools import count

Platform = list[str]


def tilt_east(platform: Platform) -> Platform:
    return [tilt_row_east(row) for row in platform]


def tilt_north(platform: Platform) -> Platform:
    return rotate_counterclockwise(tilt_east(rotate_clockwise(platform)))


@cache
def tilt_row_east(row: str) -> str:
    return '#'.join(''.join(sorted(part)) for part in ''.join(row).split('#'))


def rotate_clockwise(platform: Platform) -> Platform:
    return [''.join(row[i] for row in reversed(platform)) for i in range(len(platform[0]))]


def rotate_counterclockwise(platform: Platform) -> Platform:
    return [''.join(row[i] for row in platform) for i in range(len(platform[0]) - 1, -1, -1)]


def spin_cycle(platform: Platform) -> Platform:
    new_platform = platform
    for _ in range(4):
        new_platform = tilt_east(rotate_clockwise(new_platform))
    return new_platform


def get_total_load(platform: Platform) -> int:
    return sum(row.count('O') * (len(platform) - i) for i, row in enumerate(platform))


def get_total_load_after_n_cycles(platform: Platform, n: int) -> int:
    spinned_platform = platform
    spinned_platform_ahead = platform
    period = None
    for i in count(1):
        spinned_platform = spin_cycle(spinned_platform)
        if period is None:
            spinned_platform_ahead = spin_cycle(spin_cycle(spinned_platform_ahead))
            if spinned_platform == spinned_platform_ahead:
                period = i
        elif (n - i) % period == 0:
            return get_total_load(spinned_platform)


def main():
    platform = [line.rstrip('\n') for line in sys.stdin]
    print(get_total_load(tilt_north(platform)))
    print(get_total_load_after_n_cycles(platform, 1_000_000_000))


if __name__ == '__main__':
    main()
