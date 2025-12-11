use std::io;

pub fn main() {
    let machines = parse_input();
    println!("{}", machines.iter().map(fewest_presses).sum::<usize>());
    //println!("{}", machines.iter().map(fewest_joltage_presses).sum::<usize>());
}

fn fewest_presses(machine: &Machine) -> usize {
    let binary_buttons: Vec<u32> = machine.buttons
        .iter()
        .map(|b| b.iter().map(|n| (1 << n) as u32).sum())
        .collect();
    for subset in subsets(&binary_buttons) {
        if subset.iter().copied().fold(0, |a, b| a ^ b) == machine.lights {
            return subset.len()
        }
    }
    unreachable!()
}

// fn fewest_joltage_presses(machine: &Machine) -> usize {
//     let matrix = buttons_to_matrix(&machine.buttons, machine.joltages.len());
//     0
// }
//
// fn buttons_to_matrix(buttons: &[Vec<u32>], size: usize) -> Vec<Vec<f64>> {
//     buttons.iter().map(|b| (0..size).map(|i| if b.contains(&(i as u32)) { 1. } else { 0. }).chain([-1.]).collect()).collect()
//     // (0..=max_pos)
//     //     .map(|i| buttons.iter().map(|b| b.contains(&i) as u32).collect())
//     //     .collect()
// }

struct Machine {
    lights: u32,
    buttons: Vec<Vec<u32>>,
    joltages: Vec<u32>,
}

fn subsets<T>(set: &[T]) -> Vec<Vec<&T>> {
    let mut subsets: Vec<Vec<&T>> = Vec::new();
    for count in 0..=set.len() {
        subsets.extend(get_combinations(set, count));
    }
    subsets
}

fn get_combinations<T>(set: &[T], count: usize) -> Vec<Vec<&T>> {
    if count == 0 {
        vec![Vec::new()]
    } else {
        set[..set.len() - count + 1]
            .iter()
            .enumerate()
            .flat_map(
                |(i, t)|
                get_combinations(&set[i+1..], count - 1)
                    .iter()
                    .map(|c| { let mut c1 = c.clone(); c1.push(t); c1 })
                    .collect::<Vec<Vec<&T>>>()
            ).collect()
    }
}

fn parse_input() -> Vec<Machine> {
    io::stdin().lines().map(|l| parse_line(&l.unwrap())).collect()
}

fn parse_line(s: &str) -> Machine {
    let mut state: u32 = 0;
    let mut buttons: Vec<Vec<u32>> = Vec::new();
    let mut joltages: Vec<u32> = Vec::new();
    for part in s.split_whitespace() {
        let first_char = part.chars().next().unwrap();
        let middle = &part[1..part.len()-1];
        match first_char {
            '[' => {
                state = middle
                    .chars()
                    .enumerate()
                    .filter(|&(_, ch)| ch == '#')
                    .map(|(i, _)| 1 << i)
                    .sum();
            },
            '(' => { buttons.push(parse_nums(middle)); },
            '{' => { joltages = parse_nums(middle) },
            _ => unreachable!(),
        }
    }
    Machine { lights: state, buttons, joltages }
}

fn parse_nums(s: &str) -> Vec<u32> {
    s.split(',').map(|p| p.parse().unwrap()).collect()
}
