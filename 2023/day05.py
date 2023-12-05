import sys
from typing import NamedTuple, Iterator


class Range(NamedTuple):
    start: int
    length: int

    @property
    def last(self):
        return self.start + self.length - 1

    def __contains__(self, item: int) -> bool:
        return self.start <= item < self.start + self.length


class RangeMap(NamedTuple):
    source_range: Range
    offset: int

    def __contains__(self, item: int) -> bool:
        return item in self.source_range

    def __getitem__(self, item: int) -> int | None:
        if item in self:
            return item + self.offset
        else:
            return item

    def map_range(self, range_: Range) -> tuple[Range, Range | None]:
        if range_.start not in self:
            if self.source_range.start in range_:
                unmapped_length = self.source_range.start - range_.start
                return (
                    Range(range_.start, unmapped_length),
                    Range(self.source_range.start, range_.length - unmapped_length),
                )
            else:
                return range_, None
        elif range_.last in self:
            return Range(self[range_.start], range_.length), None
        else:
            last_mapped = self.source_range.last
            mapped_length = last_mapped - range_.start + 1
            mapped_range = Range(self[range_.start], mapped_length)
            return mapped_range, Range(last_mapped + 1, range_.length - mapped_length)

    @classmethod
    def from_values(cls, source_start: int, destination_start: int, length: int) -> "RangeMap":
        return cls(source_range=Range(source_start, length), offset=destination_start - source_start)


def parse_input(lines: Iterator[str]) -> tuple[list[int], list[list[RangeMap]]]:
    line = next(lines)
    seeds = [int(part) for part in line.split(': ')[1].split()]
    next(lines)  # skip the empty line
    next(lines)  # skip the initial header
    maps = []
    current_map = []
    for line in lines:
        if not line:
            continue
        if line[0].isdigit():
            destination_start, source_start, range_length = (int(part) for part in line.split())
            current_map.append(
                RangeMap.from_values(
                    source_start=source_start,
                    destination_start=destination_start,
                    length=range_length
                )
            )
        else:
            maps.append(current_map)
            current_map = []
    maps.append(current_map)
    return seeds, maps


class Map(NamedTuple):
    range_maps: list[RangeMap]

    def map_value(self, value: int) -> int:
        range_map = self._find_range_map(value)
        return range_map[value] if range_map else value

    def map_range(self, range_: Range) -> list[Range]:
        mapped_ranges = []
        current_range = range_
        while current_range:
            range_map = self._find_range_map(current_range.start)
            if not range_map:
                mapped_ranges.append(current_range)
                break
            mapped_range, current_range = range_map.map_range(current_range)
            mapped_ranges.append(mapped_range)
        return mapped_ranges

    def _find_range_map(self, value: int) -> RangeMap | None:
        for range_map in self.range_maps:
            if range_map.source_range.last >= value:
                return range_map


def optimize_map(range_maps: list[RangeMap]) -> Map:
    return Map(sorted(range_maps, key=lambda r: r.source_range))


def get_location(seed: int, maps: list[Map]) -> int:
    current_value = seed
    for map_ in maps:
        current_value = map_.map_value(current_value)
    return current_value


def get_location_ranges(seed_range: Range, maps: list[Map]) -> list[Range]:
    current_ranges = [seed_range]
    for map_ in maps:
        new_ranges = []
        for range_ in current_ranges:
            new_ranges += map_.map_range(range_)
        current_ranges = new_ranges
    return current_ranges


def main():
    seeds, maps = parse_input(line.rstrip('\n') for line in sys.stdin)
    optimized_maps = [optimize_map(range_maps) for range_maps in maps]
    print(min(get_location(seed, optimized_maps) for seed in seeds))
    seed_ranges = [Range(seeds[i], seeds[i + 1]) for i in range(0, len(seeds), 2)]
    print(min(min(r.start for r in get_location_ranges(seed_range, optimized_maps)) for seed_range in seed_ranges))


if __name__ == '__main__':
    main()
