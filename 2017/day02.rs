use std::io;

pub fn main() {
    let rows = parse_input();
    let checksum: u32 = rows.iter().map(|row| row.iter().max().unwrap() - row.iter().min().unwrap()).sum();
    println!("{}", checksum);
    let result: u32 = rows.iter().map(|row| find_division(row).unwrap()).sum();
    println!("{}", result);
}

fn parse_input() -> Vec<Vec<u32>> {
    let lines = io::stdin().lines();
    let mut rows: Vec<Vec<u32>> = Vec::new();
    for line in lines {
        rows.push(line.unwrap().split_whitespace().map(|x| x.parse::<u32>().unwrap()).collect());
    }
    rows
}

fn find_division(row: &[u32]) -> Option<u32> {
    for i in 0..row.len() {
        for j in i + 1..row.len() {
            if row[i].is_multiple_of(row[j]) {
                return Some(row[i] / row[j]);
            }
            if row[j].is_multiple_of(row[i]) {
                return Some(row[j] / row[i]);
            }
        }
    }
    None
}
