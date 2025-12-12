use std::io;

pub fn main() {
    let machines = parse_input();
    println!("{}", machines.iter().map(fewest_presses).sum::<usize>());
    println!("{}", machines.iter().map(fewest_joltage_presses).sum::<usize>());
}

fn fewest_presses(machine: &Machine) -> usize {
    let binary_buttons = get_binary_buttons(&machine.buttons);
    for subset in subsets(&binary_buttons) {
        if subset.iter().fold(0, |a, &b| a ^ b) == machine.lights {
            return subset.len()
        }
    }
    unreachable!()
}

fn get_binary_buttons(buttons: &[Vec<u32>]) -> Vec<u32> {
    buttons
        .iter()
        .map(|b| b.iter().map(|n| 1u32 << n).sum())
        .collect()
}

// I gave up, went to Reddit and found this hint:
// https://www.reddit.com/r/adventofcode/comments/1pk87hl/2025_day_10_part_2_bifurcate_your_way_to_victory/
// > find all possible sets of buttons you can push so that the remaining voltages are even, and divide by 2 and recurse.
fn fewest_joltage_presses(machine: &Machine) -> usize {
    let binary_buttons = get_binary_buttons(&machine.buttons);
    let subset_xors: Vec<_> = subsets(&binary_buttons)
        .iter()
        .map(|subset| (subset.to_owned(), subset.iter().fold(0, |a, &b| a ^ b)))
        .collect();
    fewest_joltage_presses_recur(&subset_xors, &machine.joltages).unwrap()
}

fn fewest_joltage_presses_recur(subset_xors: &[(Vec<u32>, u32)], joltages: &[i32]) -> Option<usize> {
    if joltages.iter().all(|&j| j == 0) {
        return Some(0);
    }
    let binary_joltages = get_binary_joltages(joltages);
    let mut best = None;
    for (subset, xor) in subset_xors {
        if *xor == binary_joltages {
            let new_joltages = get_new_joltages(joltages, &subset);
            if new_joltages.iter().all(|&j| j >= 0) {
                let press_count = fewest_joltage_presses_recur(
                    subset_xors, &new_joltages
                ).map(|c| subset.len() + 2 * c);
                best = best.min(press_count).or(best).or(press_count);
            }
        }
    }
    best
}

fn get_new_joltages(joltages: &[i32], subset: &[u32]) -> Vec<i32> {
    let mut new_joltages = Vec::new();
    let mut mask = 1;
    for &joltage in joltages {
        new_joltages.push((joltage - subset.iter().filter(|&b| b & mask != 0).count() as i32) / 2);
        mask <<= 1;
    }
    new_joltages
}

fn get_binary_joltages(joltages: &[i32]) -> u32 {
    joltages
        .iter()
        .enumerate()
        .map(|(i, j)| ((1 << i) * (j % 2)) as u32)
        .sum()
}

struct Machine {
    lights: u32,
    buttons: Vec<Vec<u32>>,
    joltages: Vec<i32>,
}

fn subsets<T: Copy>(set: &[T]) -> Vec<Vec<T>> {
    let mut subsets: Vec<Vec<T>> = Vec::new();
    for count in 0..=set.len() {
        subsets.extend(get_combinations(set, count));
    }
    subsets
}

fn get_combinations<T: Copy>(set: &[T], count: usize) -> Vec<Vec<T>> {
    if count == 0 {
        vec![Vec::new()]
    } else {
        set[..set.len() - count + 1]
            .iter()
            .enumerate()
            .flat_map(
                |(i, &t)|
                get_combinations(&set[i+1..], count - 1)
                    .iter()
                    .map(|c| { let mut c1 = c.clone(); c1.push(t); c1 })
                    .collect::<Vec<Vec<T>>>()
            ).collect()
    }
}

fn parse_input() -> Vec<Machine> {
    io::stdin().lines().map(|l| parse_line(&l.unwrap())).collect()
}

fn parse_line(s: &str) -> Machine {
    let mut state: u32 = 0;
    let mut buttons: Vec<Vec<u32>> = Vec::new();
    let mut joltages: Vec<i32> = Vec::new();
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
            '{' => { joltages = parse_nums(middle).iter().map(|&j| j as i32).collect() },
            _ => unreachable!(),
        }
    }
    Machine { lights: state, buttons, joltages }
}

fn parse_nums(s: &str) -> Vec<u32> {
    s.split(',').map(|p| p.parse().unwrap()).collect()
}
