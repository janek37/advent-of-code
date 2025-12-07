use std::collections::{HashMap, HashSet};
use std::io;

type Coord = usize;
type Pos = (Coord, Coord);

pub fn main() {
    let (start, height, splitters) = parse_input();
    println!("{}", count_splits(start, height, &splitters));
    println!("{}", count_quantum_splits(start, height, &splitters));
}

fn count_splits(start: Pos, height: Coord, splitters: &HashSet<Pos>) -> u32 {
    let mut count = 0;
    let (x, y0) = start;
    let mut beams: HashSet<Coord> = HashSet::from([x; 1]);
    for y in y0+1..height {
        let mut new_beams: HashSet<Coord> = HashSet::new();
        for x in beams {
            if splitters.contains(&(x, y)) {
                count += 1;
                new_beams.insert(x - 1);
                new_beams.insert(x + 1);
            } else {
                new_beams.insert(x);
            }
        }
        beams = new_beams;
    }
    count
}

fn count_quantum_splits(start: Pos, height: Coord, splitters: &HashSet<Pos>) -> u64 {
    let (x, y0) = start;
    let mut beams: HashMap<Coord, u64> = HashMap::from([(x, 1); 1]);
    for y in y0+1..height {
        let mut new_beams: HashMap<Coord, u64> = HashMap::new();
        for (x, c) in beams {
            if splitters.contains(&(x, y)) {
                *new_beams.entry(x - 1).or_insert(0) += c;
                *new_beams.entry(x + 1).or_insert(0) += c;
            } else {
                *new_beams.entry(x).or_insert(0) += c;
            }
        }
        beams = new_beams;
    }
    beams.values().sum()
}

fn parse_input() -> (Pos, Coord, HashSet<Pos>) {
    let mut start: Pos = (0, 0);
    let mut splitters: HashSet<Pos> = HashSet::new();
    let mut height = 0;
    for (i, line) in io::stdin().lines().enumerate() {
        let raw_line = line.unwrap();
        for (j, ch) in raw_line.chars().enumerate() {
            if ch == 'S' {
                start = (j, i);
            } else if ch == '^' {
                splitters.insert((j, i));
            }
        }
        height = i + 1;
    }
    (start, height, splitters)
}