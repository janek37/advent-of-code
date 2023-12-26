MIN_OVERLAP = 12

ROTATIONS = [
    lambda x, y, z: (x, y, z),
    lambda x, y, z: (x, -y, -z),
    lambda x, y, z: (x, -z, y),
    lambda x, y, z: (x, z, -y),
    lambda x, y, z: (-x, -y, z),
    lambda x, y, z: (-x, y, -z),
    lambda x, y, z: (-x, z, y),
    lambda x, y, z: (-x, -z, -y),
    lambda x, y, z: (y, z, x),
    lambda x, y, z: (y, -z, -x),
    lambda x, y, z: (y, -x, z),
    lambda x, y, z: (y, x, -z),
    lambda x, y, z: (-y, -z, x),
    lambda x, y, z: (-y, z, -x),
    lambda x, y, z: (-y, x, z),
    lambda x, y, z: (-y, -x, -z),
    lambda x, y, z: (z, x, y),
    lambda x, y, z: (z, -x, -y),
    lambda x, y, z: (z, -y, x),
    lambda x, y, z: (z, y, -x),
    lambda x, y, z: (-z, -x, y),
    lambda x, y, z: (-z, x, -y),
    lambda x, y, z: (-z, y, x),
    lambda x, y, z: (-z, -y, -x),
]


def parse_input(lines):
    scanners = []
    for line in lines:
        if line.startswith('---'):
            scanner = []
            scanners.append(scanner)
        elif line.strip():
            coord = tuple(int(s) for s in line.strip().split(','))
            scanner.append(coord)
    return scanners


def get_vector(pos1, pos2):
    x1, y1, z1 = pos1
    x2, y2, z2 = pos2
    return x2-x1, y2-y1, z2-z1


def add_vectors(v1, v2):
    x1, y1, z1 = v1
    x2, y2, z2 = v2
    return x1+x2, y1+y2, z1+z2


def adjust_beacon(beacon, rotation, translation):
    return add_vectors(rotation(*beacon), translation)


def prepare_scanner(scanner):
    new_scanner = []
    for beacon in scanner:
        vectors = [get_vector(beacon, beacon2) for beacon2 in scanner]
        new_scanner.append((beacon, vectors))
    return new_scanner


def adjust_beacons(scanner1, scanner2):
    for beacon1, vectors1 in scanner1[:-MIN_OVERLAP+1]:
        vectors1_set = set(vectors1)
        for beacon2, vectors2 in scanner2:
            for rot_ind, rotation in enumerate(ROTATIONS):
                rotated_vectors2 = [rotation(*vector) for vector in vectors2]
                if len(vectors1_set & set(rotated_vectors2)) >= MIN_OVERLAP:
                    translation = get_vector(rotation(*beacon2), beacon1)
                    adjusted = []
                    for beacon2a, vectors2a in scanner2:
                        new_beacon = adjust_beacon(beacon2a, rotation, translation)
                        new_vectors = [
                            rotation(*vector)
                            for vector in vectors2a
                        ]
                        adjusted.append((new_beacon, new_vectors))
                    return True, adjusted, translation
    return False, None, None


def adjust_scanners(scanners):
    adjusted = [scanners[0]]
    not_adjusted = scanners[1:]
    current_ind = 0
    locations = [(0, 0, 0)]
    while not_adjusted:
        current = adjusted[current_ind]
        new_not_adjusted = []
        for scanner in not_adjusted:
            success, new_scanner, location = adjust_beacons(current, scanner)
            if success:
                adjusted.append(new_scanner)
                locations.append(location)
            else:
                new_not_adjusted.append(scanner)
        not_adjusted = new_not_adjusted
        current_ind += 1
    return adjusted, locations


def get_all_beacons(scanners):
    return set(beacon for scanner in scanners for beacon, vectors in scanner)


def distances(locations):
    for x1, y1, z1 in locations:
        for x2, y2, z2 in locations:
            yield abs(x1 - x2) + abs(y1 - y2) + abs(z1 - z2)


def main():
    import sys
    scanners = [prepare_scanner(s) for s in parse_input(sys.stdin)]
    adjusted, locations = adjust_scanners(list(scanners))
    print(len(get_all_beacons(adjusted)))
    print(max(distances(locations)))


if __name__ == '__main__':
    main()
