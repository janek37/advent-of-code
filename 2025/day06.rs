use std::io;

pub fn main() {
    let (rows, columns, ops) = parse_input();
    println!("{}", compute(&transpose(&rows), &ops));
    println!("{}", compute(&columns, &ops));
}

enum Op {
    Plus,
    Times,
}

fn compute(columns: &[Vec<u64>], ops: &[Op]) -> u64 {
    columns
        .iter()
        .zip(ops)
        .map(|(col, op)| {
            match op {
                Op::Plus => col.iter().sum::<u64>(),
                Op::Times => col.iter().product(),
            }
        }).sum()
}

fn transpose(rows: &[Vec<u64>]) -> Vec<Vec<u64>> {
    (0..rows[0].len())
        .map(|col| rows.iter().map(|row| row[col]).collect())
        .collect()
}

fn transpose_str(rows: &[String]) -> Vec<String> {
    (0..rows[0].len())
        .map(|col| rows.iter().map(|row| row.chars().nth(col).unwrap()).collect())
        .collect()
}

fn parse_input() -> (Vec<Vec<u64>>, Vec<Vec<u64>>, Vec<Op>) {
    let mut raw_rows: Vec<String> = Vec::new();
    let mut rows: Vec<Vec<u64>> = Vec::new();
    let mut ops: Vec<Op> = Vec::new();
    for line in io::stdin().lines() {
        let raw_line = line.unwrap();
        if raw_line.contains('+') || raw_line.contains('*') {
            ops = raw_line
                .split_whitespace()
                .map(|s| if s.starts_with('+') { Op::Plus } else { Op::Times })
                .collect();
        } else {
            raw_rows.push(raw_line.to_owned());
            let row = raw_line.split_whitespace().map(|s| s.parse().unwrap()).collect();
            rows.push(row);
        }
    }
    let mut columns: Vec<Vec<u64>> = Vec::new();
    let mut current_column: Vec<u64> = Vec::new();
    for raw_column in transpose_str(&raw_rows) {
        let trimmed_column = raw_column.trim();
        if trimmed_column.is_empty() {
            columns.push(current_column);
            current_column = Vec::new();
        } else {
            current_column.push(trimmed_column.parse().unwrap());
        }
    }
    if !current_column.is_empty() {
        columns.push(current_column);
    }
    (rows, columns, ops)
}
