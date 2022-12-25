import re


def parse_input(lines):
    for line in lines:
        groups = re.findall(r'(on|off) x=(.*)\.\.(.*),y=(.*)\.\.(.*),z=(.*)\.\.(.*)', line)[0]
        state = groups[0]
        x0, x1, y0, y1, z0, z1 = (int(s) for s in groups[1:])
        yield state, ((x0, x1), (y0, y1), (z0, z1))


def check_range_overlap(r1, r2):
    return r1[0] <= r2[1] and r1[1] >= r2[0]


def check_overlap(cuboid, other):
    return all(check_range_overlap(r1, r2) for r1, r2 in zip(cuboid, other))


def filter_steps(steps):
    for state, ranges in steps:
        if all(check_range_overlap(r, (-50, 50)) for r in ranges):
            yield state, ranges


def split_cuboid(cuboid, other):
    cx, cy, cz = cuboid
    ox, oy, oz = other
    if cx[0] < ox[0]:
        yield (cx[0], ox[0]-1), cy, cz
        cx = (ox[0], cx[1])
    if ox[1] < cx[1]:
        yield (ox[1]+1, cx[1]), cy, cz
        cx = (cx[0], ox[1])
    if cy[0] < oy[0]:
        yield cx, (cy[0], oy[0]-1), cz
        cy = (oy[0], cy[1])
    if oy[1] < cy[1]:
        yield cx, (oy[1]+1, cy[1]), cz
        cy = (cy[0], oy[1])
    if cz[0] < oz[0]:
        yield cx, cy, (cz[0], oz[0]-1)
        # cz = (oz[0], cz[1])
    if oz[1] < cz[1]:
        yield cx, cy, (oz[1]+1, cz[1])
        # cz = (cz[0], oz[1])


def follow_step(cuboids, step):
    state, cuboid = step
    for c in cuboids:
        if check_overlap(c, cuboid):
            yield from split_cuboid(c, cuboid)
        else:
            yield c
    if state == 'on':
        yield cuboid


def follow_steps(steps):
    cuboids = []
    for step in steps:
        cuboids = follow_step(cuboids, step)
    return cuboids


def volume(cuboid):
    (x0, x1), (y0, y1), (z0, z1) = cuboid
    return (x1-x0+1) * (y1-y0+1) * (z1-z0+1)


def main():
    import sys
    steps = list(parse_input(sys.stdin))
    filtered_steps = filter_steps(steps)
    cuboids = follow_steps(filtered_steps)
    print(sum(volume(c) for c in cuboids))
    cuboids = follow_steps(steps)
    print(sum(volume(c) for c in cuboids))


if __name__ == '__main__':
    main()
