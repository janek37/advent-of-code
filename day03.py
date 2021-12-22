def count_ones(binary_numbers):
    n = len(binary_numbers[0])
    counts = [0] * n
    for binary_number in binary_numbers:
        for i, bit in enumerate(binary_number):
            if bit == '1':
                counts[i] += 1
    return counts


def get_gamma_epsilon(binary_numbers):
    l = len(binary_numbers)
    counts = count_ones(binary_numbers)
    gamma_str = ''.join('1' if c > l/2 else '0' for c in counts)
    epsilon_str = ''.join('1' if c <= l/2 else '0' for c in counts)
    return int(gamma_str, 2), int(epsilon_str, 2)


def filter_numbers(binary_numbers, position, prefer_most, tie_breaker):
    l = len(binary_numbers)
    one_count = sum(1 for bn in binary_numbers if bn[position] == '1')
    if one_count == l/2:
        bit = tie_breaker
    else:
        if prefer_most:
            bit = '1' if one_count > l/2 else '0'
        else:
            bit = '0' if l > one_count > l/2 else '1'
    return [bn for bn in binary_numbers if bn[position] == bit]


def choose_number(binary_numbers, prefer_most, tie_breaker):
    nums = binary_numbers
    for position in range(len(binary_numbers[0])):
        if len(nums) == 1:
            break
        nums = filter_numbers(nums, position, prefer_most, tie_breaker)
    return nums[0]


def main():
    import sys
    binary_numbers = [line.strip() for line in sys.stdin]
    gamma, epsilon = get_gamma_epsilon(binary_numbers)
    print(gamma * epsilon)
    o2_rating = choose_number(binary_numbers, prefer_most=True, tie_breaker='1')
    co2_rating = choose_number(binary_numbers, prefer_most=False, tie_breaker='0')
    print(int(o2_rating, 2) * int(co2_rating, 2))


if __name__ == '__main__':
    main()
