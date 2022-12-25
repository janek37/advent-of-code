def analyze_sonar(depths):
    return sum(int(d2 > d1) for d1, d2 in zip(depths, depths[1:]))


def analyze_sonar_with_sliding_window(depths):
    return sum(int(d2 > d1) for d1, d2 in zip(depths, depths[3:]))


def main():
    import sys
    depths = [int(line.strip()) for line in sys.stdin]
    print(analyze_sonar(depths))
    print(analyze_sonar_with_sliding_window(depths))


if __name__ == '__main__':
    main()
