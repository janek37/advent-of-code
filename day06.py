import sys


def find_marker(data: str, length: int) -> int:
    for i in range(len(data)):
        maybe_marker = data[i:i+length]
        if len(set(maybe_marker)) == length:
            return i + length


def main():
    data = sys.stdin.readline().strip()
    print(find_marker(data, 4))
    print(find_marker(data, 14))


if __name__ == '__main__':
    main()
