use std::collections::HashSet;
use std::io;

pub fn main() {
    let (steps, mut machine) = parse_input();
    for _ in 0..steps {
        machine.step();
    }
    println!("{}", machine.checksum());
}

struct TuringMachine {
    state: usize,
    states: Vec<(StateAction, StateAction)>,
    tape: HashSet<i32>,
    cursor: i32,
}

impl TuringMachine {
    fn step(&mut self) {
        let symbol = self.tape.contains(&self.cursor);
        let state_action = if symbol {
            &self.states[self.state].1
        } else {
            &self.states[self.state].0
        };
        if state_action.write {
            self.tape.insert(self.cursor);
        } else {
            self.tape.remove(&self.cursor);
        }
        match state_action.direction {
            Direction::Left => { self.cursor -= 1 },
            Direction::Right => { self.cursor += 1 },
        }
        self.state = state_action.next_state;
    }

    fn checksum(&self) -> usize {
        self.tape.len()
    }
}

struct StateAction {
    write: bool,
    direction: Direction,
    next_state: usize,
}

fn parse_input() -> (u32, TuringMachine) {
    let mut buf = String::new();
    let start_state = state_id_to_index(
        read_line_part(&mut buf, 3).chars().next().unwrap()
    );
    let steps: u32 = read_line_part(&mut buf, 5).parse().unwrap();
    let mut states = Vec::new();
    loop {
        io::stdin().read_line(&mut buf).unwrap(); // skip empty line
        let line_len = io::stdin().read_line(&mut buf).unwrap();  // skip state id
        if line_len == 0 { break; }
        let zero_action = parse_state_action(&mut buf);
        let one_action = parse_state_action(&mut buf);
        states.push((zero_action, one_action));
    }
    (steps, TuringMachine { state: start_state, states, tape: HashSet::new(), cursor: 0 })
}

fn state_id_to_index(state_id: char) -> usize {
    (state_id as usize) - ('A' as usize)
}

enum Direction {
    Left,
    Right,
}

fn parse_state_action(buf: &mut String) -> StateAction {
    io::stdin().read_line(buf).unwrap(); // If the current value is X:
    let write = read_line_part(buf, 4).starts_with('1');
    let direction = if read_line_part(buf, 6).starts_with('l') {
        Direction::Left
    } else {
        Direction::Right
    };
    let next_state = state_id_to_index(read_line_part(buf, 4).chars().next().unwrap());
    StateAction { write, direction, next_state }
}

fn read_line_part(buf: &mut String, idx: usize) -> &str {
    buf.clear();
    io::stdin().read_line(buf).unwrap();
    buf.split_whitespace().nth(idx).unwrap()
}
