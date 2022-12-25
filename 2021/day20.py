def parse_input(lines):
    algorithm = lines[0]
    image = [line.strip() for line in lines[2:]]
    return algorithm, image


def get_pixel(image, x, y, outside):
    if 0 <= y < len(image) and 0 <= x < len(image[0]):
        return 1 if image[y][x] == '#' else 0
    else:
        return outside


def get_index(image, x, y, outside):
    index = 0
    for y1 in [y-1, y, y+1]:
        for x1 in [x-1, x, x+1]:
            index = index*2 + get_pixel(image, x1, y1, outside)
    return index


def enhance(algorithm, image, outside=0):
    new_image = []
    for y in range(-1, len(image)+1):
        new_row = []
        for x in range(-1, len(image[0])+1):
            index = get_index(image, x, y, outside)
            new_row.append(algorithm[index])
        new_image.append(new_row)
    if outside == 0:
        outside_ind = 0
    else:
        outside_ind = 511
    return new_image, 1 if algorithm[outside_ind] == '#' else 0


def count_pixels(image):
    return sum(1 if pixel == '#' else 0 for row in image for pixel in row)


def print_image(image):
    for row in image:
        print(''.join(row))


def main():
    import sys
    algorithm, image = parse_input(list(sys.stdin))
    outside = 0
    for i in range(50):
        image, outside = enhance(algorithm, image, outside)
        if i == 1:
            print(count_pixels(image))
    print(count_pixels(image))


if __name__ == '__main__':
    main()
