use std::collections::HashSet;
use std::io;
use crate::day10::KnotHash;

pub fn main() {
    let mut key = String::new();
    io::stdin().read_line(&mut key).unwrap();
    let trimmed_key = key.trim_end();
    let grid: Vec<Vec<u8>> = (0..128).map(|i| KnotHash::digest_from_str(&format!("{}-{}", trimmed_key, i))).collect();
    let count: u32 = grid.iter().map(|digest| digest.iter().copied().map(|b| count_ones(b) as u32).sum::<u32>()).sum();
    println!("{}", count);
    println!("{}", Disk { grid }.count_regions());
}

fn count_ones(b: u8) -> u8 {
    if b == 0 { 0 } else { b % 2 + count_ones(b / 2) }
}

struct Disk {
    grid: Vec<Vec<u8>>,
}

impl Disk {
    fn get_state(&self, pos: (u8, u8)) -> bool {
        (self.grid[pos.0 as usize][(pos.1 / 8) as usize] >> (7 - pos.1 % 8)) % 2 == 1
    }

    fn count_regions(&self) -> u32 {
        let mut count = 0;
        let mut visited: HashSet<(u8, u8)> = HashSet::new();
        for row in 0..128 {
            for col in 0..128 {
                let pos = (row, col);
                if self.get_state(pos) && !visited.contains(&pos) {
                    visited.extend(&self.flood_fill(pos));
                    count += 1;
                }
            }
        }
        count
    }

    fn flood_fill(&self, start: (u8, u8)) -> HashSet<(u8, u8)> {
        let mut visited: HashSet<(u8, u8)> = HashSet::new();
        let mut stack: Vec<(u8, u8)> = vec![start];
        while let Some(square) = stack.pop() {
            if visited.contains(&square) {
                continue
            }
            visited.insert(square);
            for (offrow, offcol) in [(0, -1), (0, 1), (-1, 0), (1, 0)] {
                let new_row = offrow + (square.0 as i32);
                let new_col = offcol + (square.1 as i32);
                if (0..128).contains(&new_row) && (0..128).contains(&new_col) {
                    let new_pos = (new_row as u8, new_col as u8);
                    if self.get_state(new_pos) {
                        stack.push(new_pos);
                    }
                }
            }
        }
        visited
    }
}
