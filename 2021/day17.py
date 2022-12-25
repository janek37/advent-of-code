import re

def parse_input(s):
    results = re.findall(r'target area: x=(\d+)..(\d+), y=(-\d+)..(-\d+)', s)
    x0, x1, y0, y1 = (int(x) for x in results[0])
    return (x0, x1), (y0, y1)


def highest_point(y0):
    return y0*(y0+1) // 2


def good_velocities(x, y):
    x0, x1 = x
    y0, y1 = y
    for vx_start in range(x1+1):
        for vy_start in range(y0, -y0):
            x_pos = 0
            y_pos = 0
            vx = vx_start
            vy = vy_start
            while x_pos <= x1 and y_pos >= y0:
                if x0 <= x_pos <= x1 and y0 <= y_pos <= y1:
                    yield vx_start, vy_start
                    break
                if vx == 0 and x_pos < x0:
                    break
                x_pos += vx
                vx = max(vx-1, 0)
                y_pos += vy
                vy -= 1


def main():
    import sys
    x, y = parse_input(sys.stdin.read())
    print(highest_point(y[0]))
    print(sum(1 for _ in good_velocities(x, y)))


if __name__ == '__main__':
    main()
