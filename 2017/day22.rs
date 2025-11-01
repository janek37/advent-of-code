use std::collections::{HashMap, HashSet};
use std::io;

pub fn main() {
    let (start, orig_infected) = parse_input();
    let mut infected = orig_infected.clone();
    let mut state = (start, Direction::North);
    let mut count = 0;
    for _ in 0..10000 {
        let i;
        (state, i) = burst(state, &mut infected);
        count += i;
    }
    println!("{}", count);
    count = 0;
    state = (start, Direction::North);
    let mut node_states: HashMap<(i32, i32), NodeState> = orig_infected
        .iter()
        .map(|&coord| (coord, NodeState::Infected)).collect();
    for _ in 0..10_000_000 {
        let i;
        (state, i) = burst_evolved(state, &mut node_states);
        count += i;
    }
    println!("{}", count);
}

fn burst((position, facing): ((i32, i32), Direction), infected: &mut HashSet<(i32, i32)>) -> (((i32, i32), Direction), i32) {
    let c: i32;
    let new_direction = if infected.contains(&position) {
        infected.remove(&position);
        c = 0;
        facing.turn_right()
    } else {
        infected.insert(position);
        c = 1;
        facing.turn_left()
    };
    let new_position = new_direction.step(position);
    ((new_position, new_direction), c)
}

fn burst_evolved((position, facing): ((i32, i32), Direction), node_states: &mut HashMap<(i32, i32), NodeState>) -> (((i32, i32), Direction), i32) {
    let c: i32;
    let new_direction = match node_states.get(&position) {
        None => {
            node_states.insert(position, NodeState::Weakened);
            c = 0;
            facing.turn_left()
        },
        Some(NodeState::Weakened) => {
            node_states.insert(position, NodeState::Infected);
            c = 1;
            facing
        },
        Some(NodeState::Infected) => {
            node_states.insert(position, NodeState::Flagged);
            c = 0;
            facing.turn_right()
        },
        Some(NodeState::Flagged) => {
            node_states.remove(&position);
            c = 0;
            facing.turn_left().turn_left()
        }
    };
    let new_position = new_direction.step(position);
    ((new_position, new_direction), c)
}

enum Direction {
    North,
    South,
    West,
    East,
}

impl From<NodeState> for char {
    fn from(node_state: NodeState) -> char {
        match node_state {
            NodeState::Weakened => 'W',
            NodeState::Infected => '#',
            NodeState::Flagged => 'F',
        }
    }
}

#[derive(Clone, Copy)]
enum NodeState {
    Weakened,
    Infected,
    Flagged,
}

impl Direction {
    fn turn_right(&self) -> Self {
        match self {
            Direction::North => Direction::East,
            Direction::East => Direction::South,
            Direction::South => Direction::West,
            Direction::West => Direction::North,
        }
    }

    fn turn_left(&self) -> Self {
        match self {
            Direction::North => Direction::West,
            Direction::East => Direction::North,
            Direction::South => Direction::East,
            Direction::West => Direction::South,
        }
    }

    fn step(&self, pos: (i32, i32)) -> (i32, i32) {
        let offset = match self {
            Direction::North => (0, -1),
            Direction::South => (0, 1),
            Direction::West => (-1, 0),
            Direction::East => (1, 0),
        };
        (pos.0 + offset.0, pos.1 + offset.1)
    }
}

fn parse_input() -> ((i32, i32), HashSet<(i32, i32)>) {
    let lines: Vec<_> = io::stdin().lines().collect();
    let size = lines.len() as i32; // assume square
    let center = ((size - 1) / 2, (size - 1) / 2);
    let coords = lines
        .iter()
        .enumerate()
        .flat_map(|(i, line)|
             line.as_ref()
                 .unwrap()
                 .chars()
                 .enumerate()
                 .filter_map(move |(j, ch)| if ch == '#' { Some((j as i32, i as i32)) } else { None })).collect();
    (center, coords)
}
