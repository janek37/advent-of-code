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

fn find_division(row: &Vec<u32>) -> Option<u32> {
    for i in 0..row.len() {
        for j in i + 1..row.len() {
            if row[i] % row[j] == 0 {
                return Some(row[i] / row[j]);
            }
            if row[j] % row[i] == 0 {
                return Some(row[j] / row[i]);
            }
        }
    }
    None
}
