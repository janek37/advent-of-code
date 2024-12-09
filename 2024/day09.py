import sys


def decompress(disk_map: list[int]) -> list[int | None]:
    decompressed = []
    for i, size in enumerate(disk_map):
        value = i // 2 if i % 2 == 0 else None
        decompressed += [value] * size
    return decompressed


def compact(disk_dump: list[int | None]) -> None:
    front_index = 0
    back_index = len(disk_dump) - 1
    while front_index < back_index:
        if disk_dump[back_index] is None:
            back_index -= 1
            continue
        if disk_dump[front_index] is not None:
            front_index += 1
            continue
        disk_dump[front_index] = disk_dump[back_index]
        disk_dump[back_index] = None
        front_index += 1
        back_index -= 1


def check_sum(disk_dump: list[int]) -> int:
    return sum(i * file_id for i, file_id in enumerate(disk_dump) if file_id is not None)


def compact_smart(disk_map: list[int]) -> list[tuple[int, int, int]]:
    file_positions = []
    gap_positions = []
    current_position = 0
    for i, size in enumerate(disk_map):
        if i % 2 == 0:
            file_positions.append((i // 2, size, current_position))
        elif size > 0:
            gap_positions.append((size, current_position))
        current_position += size

    moved_files = []
    for file_id, size, position in reversed(file_positions):
        for i, (gap_size, gap_position) in enumerate(gap_positions):
            if gap_position > position:
                moved_files.append((file_id, size, position))
                break
            if gap_size < size:
                continue
            moved_files.append((file_id, size, gap_position))
            gap_positions[i] = (gap_size - size, gap_position + size)
            break
        else:
            moved_files.append((file_id, size, position))
    return moved_files


def smart_checksum(file_positions: list[tuple[int, int, int]]) -> int:
    return sum(file_id * (position * size + size * (size - 1) // 2) for file_id, size, position in file_positions)


def main():
    disk_map = list(map(int, sys.stdin.readline().strip()))
    disk_dump = decompress(disk_map)
    compact(disk_dump)
    print(check_sum(disk_dump))
    print(smart_checksum(compact_smart(disk_map)))


if __name__ == '__main__':
    main()
