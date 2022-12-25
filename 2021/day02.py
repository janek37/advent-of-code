def compute_course(course):
    position = 0
    depth = 0
    for command, arg in course:
        if command == 'forward':
            position += arg
        elif command == 'down':
            depth += arg
        elif command == 'up':
            depth -= arg
    return position, depth


def compute_course_with_aim(course):
    aim = 0
    position = 0
    depth = 0
    for command, arg in course:
        if command == 'forward':
            position += arg
            depth += arg * aim
        elif command == 'down':
            aim += arg
        elif command == 'up':
            aim -= arg
    return position, depth


def parse_course(lines):
    return (
        (command, int(arg))
        for command, arg in (line.split() for line in lines)
    )


def main():
    import sys
    course = list(parse_course(sys.stdin))
    pos, depth = compute_course(course)
    print(pos, depth, pos * depth)
    pos, depth = compute_course_with_aim(course)
    print(pos, depth, pos * depth)


if __name__ == '__main__':
    main()
