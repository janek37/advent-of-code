use std::io;
use std::collections::HashMap;

pub fn main() {
    let banks = parse_input();
    let (count, loop_length) = find_repeated(&banks);
    println!("{}", count);
    println!("{}", loop_length);
}

fn find_repeated(banks: &[usize]) -> (usize, usize) {
    let mut count = 0;
    let mut new_banks = banks.to_vec();
    let mut seen: HashMap<Vec<usize>, usize> = HashMap::new();
    let loop_length;
    loop {
        let new_new_banks = &redistribute(&new_banks);
        if seen.contains_key(&new_banks) {
            loop_length = count - seen.get(&new_banks).unwrap();
            break
        }
        seen.insert(new_banks, count);
        new_banks = new_new_banks.clone();
        count += 1;
    }
    (count, loop_length)
}

fn redistribute(banks: &[usize]) -> Vec<usize> {
    let (idx, blocks) = banks
        .iter()
        .enumerate()
        .max_by_key(|&(i, x)| (x, -(i as isize)))
        .unwrap();
    let num_banks = banks.len();
    let small_increase = blocks / num_banks;
    let remainder = blocks % num_banks;
    banks
        .iter()
        .enumerate()
        .map(|(i, x)| (if i == idx { 0 } else { *x }) + small_increase + (if (num_banks + i - idx - 1) % num_banks < remainder { 1 } else { 0 }))
        .collect()
}

fn parse_input() -> Vec<usize> {
    let mut line = String::new();
    let _ = io::stdin().read_line(&mut line);
    line.split_whitespace().map(|x| x.parse().unwrap()).collect()
}
