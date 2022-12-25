def median(l):
    return sorted(l)[len(l)//2]


def mean(l):
    return round(sum(l)/len(l))


def total_fuel(l, position):
    return sum(abs(crab - position) for crab in l)


def total_fuel2(l, position):
    distances = (abs(crab - position) for crab in l)
    return sum(d*(d+1)/2 for d in distances)


def main():
    import sys
    crabs = [int(s) for s in sys.stdin.read().strip().split(',')]
    position = median(crabs)
    print(total_fuel(crabs, position))
    position = mean(crabs)
    print(total_fuel2(crabs, position))
    print(total_fuel2(crabs, position-1))
    print(total_fuel2(crabs, position-2))


if __name__ == '__main__':
    main()
