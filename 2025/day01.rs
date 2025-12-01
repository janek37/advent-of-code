use std::io;

pub fn main() {
    let rotations = parse_input();
    println!("{}", count_zeros(&rotations, 50));
    println!("{}", count_zero_clicks(&rotations, 50));
}

fn count_zeros(rotations: &[i16], position: i16) -> u16 {
    let mut dial = position;
    let mut zero_count = 0;
    for &r in rotations.iter() {
        dial = (dial + r).rem_euclid(100);
        if dial == 0 {
            zero_count += 1;
        }
    }
    zero_count
}

fn count_zero_clicks(rotations: &[i16], position: i16) -> i16 {
    let mut dial = position;
    let mut count = 0;
    for &r in rotations.iter() {
        if r > 0 {
            count += (dial + r) / 100;
        } else {
            count += ((if dial == 0 { 0 } else { 100 - dial }) - r) / 100;
        }
        dial = (dial + r).rem_euclid(100);
    }
    count
}

fn parse_input() -> Vec<i16> {
    let mut rotations = Vec::new();
    for line in io::stdin().lines() {
        let raw_line = line.unwrap();
        let trimmed = raw_line.trim_end();
        let direction = trimmed.chars().next().unwrap();
        let distance: i16 = trimmed[1..].parse().unwrap();
        rotations.push(if direction == 'R' { distance } else { -distance })
    }
    rotations
}
