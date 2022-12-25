def get_neighbors(location):
    neighbors = [location-10, location+10]
    if location % 10 > 0:
        neighbors += [location-1, location-11, location+9]
    if location % 10 < 9:
        neighbors += [location+1, location-9, location+11]
    return [n for n in neighbors if 0 <= n < 100]


def next_step(energies):
    new_energies = [energy+1 for energy in energies]
    flashes = set()
    new_flashes = [i for i, e in enumerate(new_energies) if e > 9]
    while new_flashes:
        current_flashes = new_flashes
        new_flashes = []
        for flash_location in current_flashes:
            if flash_location not in flashes:
                for neighbor in get_neighbors(flash_location):
                    new_energies[neighbor] += 1
                    if new_energies[neighbor] > 9:
                        new_flashes.append(neighbor)
            flashes.add(flash_location)
    for flash_location in flashes:
        new_energies[flash_location] = 0
    return new_energies, len(flashes)


def get_total_flash_count(energies, step_count):
    total_flash_count = 0
    for step in range(step_count):
        energies, flash_count = next_step(energies)
        total_flash_count += flash_count
    return total_flash_count


def get_synchronized_flash(energies):
    step = 1
    while True:
        energies, flash_count = next_step(energies)
        if flash_count == 100:
            return step
        step += 1


def main():
    import sys
    energies = [int(ch) for ch in sys.stdin.read() if ch in '0123456789']
    print(get_total_flash_count(energies, 100))
    print(get_synchronized_flash(energies))


if __name__ == '__main__':
    main()
