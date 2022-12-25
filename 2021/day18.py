def parse_sn(s, start=0):
    if '0' <= s[start] <= '9':
        return int(s[start]), start+1
    else:
        assert s[start] == '['
        left, ind = parse_sn(s, start+1)
        assert s[ind] == ','
        right, ind = parse_sn(s, ind+1)
        assert s[ind] == ']'
        return [left, right], ind+1


def parse_input(lines):
    for line in lines:
        yield(parse_sn(line)[0])


def add_sn(sn1, sn2):
    return reduce_sn([sn1, sn2])


def reduce_sn(sn):
    while True:
        while True:
            exploded, _left, _right, sn = explode(sn)
            if not exploded:
                break
        splitted, sn = split(sn)
        if not splitted:
            break
    return sn


def explode(sn, depth=0):
    if isinstance(sn, int):
        return False, 0, 0, sn
    if depth >= 4 and isinstance(sn[0], int) and isinstance(sn[1], int):
        return True, sn[0], sn[1], 0
    left_exploded, left, right, new = explode(sn[0], depth+1)
    if left_exploded:
        return True, left, 0, [new, propagate_right(right, sn[1])]
    right_exploded, left, right, new = explode(sn[1], depth+1)
    if right_exploded:
        return True, 0, right, [propagate_left(left, sn[0]), new]
    return False, 0, 0, sn


def propagate_right(n, sn):
    if isinstance(sn, int):
        return sn + n
    else:
        return [propagate_right(n, sn[0]), sn[1]]


def propagate_left(n, sn):
    if isinstance(sn, int):
        return sn + n
    else:
        return [sn[0], propagate_left(n, sn[1])]


def split(sn):
    if isinstance(sn, int):
        if sn >= 10:
            return True, [sn//2, (sn+1)//2]
        else:
            return False, sn
    else:
        left_splitted, new = split(sn[0])
        if left_splitted:
            return True, [new, sn[1]]
        right_splitted, new = split(sn[1])
        if right_splitted:
            return True, [sn[0], new]
        return False, sn


def magnitude(sn):
    if isinstance(sn, int):
        return sn
    else:
        return 3*magnitude(sn[0]) + 2*magnitude(sn[1])


def pair_sum_magnitudes(sns):
    for i, sn1 in enumerate(sns):
        for sn2 in sns[i+1:]:
            yield magnitude(add_sn(sn1, sn2))
            yield magnitude(add_sn(sn2, sn1))


def main():
    import sys
    sns = list(parse_input(sys.stdin))
    sn_sum = sns[0]
    for sn in sns[1:]:
        sn_sum = add_sn(sn_sum, sn)
    print(magnitude(sn_sum))
    print(max(pair_sum_magnitudes(sns)))


if __name__ == '__main__':
    main()
