SIZE = 5


def parse_input(lines):
    drawn_numbers = [int(n) for n in lines[0].strip().split(",")]
    boards = []
    for line_no in range(2, len(lines), SIZE+1):
        board = []
        for board_line_no in range(SIZE):
            board += [int(n) for n in lines[line_no+board_line_no].split()]
        assert len(board) == SIZE*SIZE
        boards.append(board)
    return drawn_numbers, boards


def get_fives(board):
    fives = []
    for row in range(SIZE):
        fives.append(set(board[row*SIZE:row*SIZE+SIZE]))
    for column in range(SIZE):
        fives.append(set(board[column:SIZE*SIZE:SIZE]))
    return fives


def filter_fives(drawn_numbers, fives):
    return [(five, board_no) for five, board_no in fives if five.issubset(drawn_numbers)]


def bisect_bingo(drawn_numbers, fives, start=5, end=None):
    if end is None:
        end = len(drawn_numbers)
    if start == end:
        return fives[0][1], start
    current = (start + end) // 2
    filtered_fives = filter_fives(set(drawn_numbers[:current]), fives)
    if len(filtered_fives) == 0:
        return bisect_bingo(drawn_numbers, fives, current+1, end)
    else:
        return bisect_bingo(drawn_numbers, filtered_fives, start, current)


def bisect_bingo_last(drawn_numbers, board_count, fives, start=5, end=None):
    if end is None:
        end = len(drawn_numbers)
    if start == end:
        return start
    current = (start + end) // 2
    filtered_fives = filter_fives(set(drawn_numbers[:current]), fives)
    win_count = len(set(board_no for five, board_no in filtered_fives))
    if win_count < board_count:
        return bisect_bingo_last(drawn_numbers, board_count, fives, current+1, end)
    else:
        return bisect_bingo_last(drawn_numbers, board_count, fives, start, current)


def find_board(drawn_numbers, boards):
    fives = []
    for i, board in enumerate(boards):
        board_fives = get_fives(board)
        fives += [(five, i) for five in board_fives]
    board_no, count = bisect_bingo(drawn_numbers, fives)
    return boards[board_no], count


def find_last_board(drawn_numbers, boards):
    fives = []
    for i, board in enumerate(boards):
        board_fives = get_fives(board)
        fives += [(five, i) for five in board_fives]
    count = bisect_bingo_last(drawn_numbers, len(boards), fives)
    filtered_fives = filter_fives(set(drawn_numbers[:count-1]), fives)
    win_boards = set(board_no for five, board_no in filtered_fives)
    no_win_boards = set(range(len(boards))) - win_boards
    last_board_no = list(no_win_boards)[0]
    return boards[last_board_no], count


def get_score(board, drawn_numbers):
    unmarked = [num for num in board if num not in drawn_numbers]
    unmarked_sum = sum(unmarked)
    return unmarked_sum * drawn_numbers[-1]


def main():
    import sys
    drawn_numbers, boards = parse_input(sys.stdin.readlines())
    board, count = find_board(drawn_numbers, boards)
    score = get_score(board, drawn_numbers[:count])
    print(score)
    board, count = find_last_board(drawn_numbers, boards)
    score = get_score(board, drawn_numbers[:count])
    print(score)


if __name__ == '__main__':
    main()
