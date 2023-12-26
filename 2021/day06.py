def next_day(population):
    p0, p1, p2, p3, p4, p5, p6, p7, p8 = population
    return p1, p2, p3, p4, p5, p6, p0+p7, p8, p0


def after_days(population, days):
    pop = population
    for i in range(days):
        pop = next_day(pop)
    return pop


def parse_input(lines):
    for line in lines:
        timers = [int(s) for s in line.strip().split(',')]
        break
    population = tuple(timers.count(i) for i in range(9))
    return population


def main():
    import sys
    population = parse_input(sys.stdin)
    print(sum(after_days(population, 80)))
    print(sum(after_days(population, 256)))


if __name__ == '__main__':
    main()
