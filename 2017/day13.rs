use std::io;

pub fn main() {
    let layers = parse_input();
    println!("{}", compute_severity(&layers));
    let mut delay = 0;
    loop {
        if !is_caught(&layers, delay) {
            break;
        }
        delay += 1;
    }
    println!("{}", delay);
}

fn compute_severity(layers: &[(u32, u32)]) -> u32 {
    layers.iter().map(|&(depth, range)| if depth % (2*range - 2) == 0 { depth * range } else { 0 }).sum()
}

fn is_caught(layers: &[(u32, u32)], delay: u32) -> bool {
    layers.iter().any(|&(depth, range)| (depth + delay) % (2*range - 2) == 0)
}

fn parse_input() -> Vec<(u32, u32)> {
    io::stdin().lines().map(|line| parse_line(&line.unwrap())).collect()
}

fn parse_line(line: &str) -> (u32, u32) {
    let parsed: Vec<u32> = line.split(": ").map(|p| p.parse().unwrap()).collect();
    (parsed[0], parsed[1])
}
