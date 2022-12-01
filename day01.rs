use std::io;

fn main() {
    let mut elves = parse_input();
    elves.sort_unstable_by(|a, b| b.cmp(a));
    println!("{}", elves[0]);
    println!("{}", elves[0..3].iter().sum::<i32>());
}

fn parse_input() -> Vec<i32> {
    let lines = io::stdin().lines();

    let mut elves = vec![];
    let mut current_elf = 0;
    for line in lines {
        let line = line.unwrap();
        let line = line.trim();
        if line.is_empty() {
            elves.push(current_elf);
            current_elf = 0
        } else {
            current_elf += line.parse::<i32>().unwrap()
        }
    }
    elves.push(current_elf);
    elves
}
