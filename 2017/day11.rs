use std::cmp::max;
use std::io;

pub fn main() {
    let steps = parse_input();
    println!("{}", total_distance(&steps));
    let max_distance = (1..=steps.len()).map(|i| total_distance(&steps[..i])).max().unwrap();
    println!("{}", max_distance);
}

fn total_distance(steps: &[Direction]) -> i32 {
    let (x, y) = total_offset(steps);
    x.abs() + max(y.abs() - x.abs(), 0) / 2
}

fn total_offset(steps: &[Direction]) -> (i32, i32) {
    steps.iter().map(|d| direction_to_offsets(d)).reduce(
        |(x1, y1), (|x2, y2)| (x1 + x2, y1 + y2)
    ).unwrap()
}

enum Direction {
    N,
    NE,
    SE,
    S,
    SW,
    NW,
}

fn direction_to_offsets(d: &Direction) -> (i32, i32) {
    match *d {
        Direction::N => (0, 2),
        Direction::NE => (1, 1),
        Direction::SE => (1, -1),
        Direction::S => (0, -2),
        Direction::SW => (-1, -1),
        Direction::NW => (-1, 1),
    }
}

fn direction_from_str(s: &str) -> Direction {
    match s {
        "n" => Direction::N,
        "ne" => Direction::NE,
        "se" => Direction::SE,
        "s" => Direction::S,
        "sw" => Direction::SW,
        "nw" => Direction::NW,
        _ => unreachable!(),
    }
}

fn parse_input() -> Vec<Direction> {
    let mut buffer = String::new();
    io::stdin().read_line(&mut buffer).unwrap();
    buffer.trim_end().split(',').map(direction_from_str).collect()
}
