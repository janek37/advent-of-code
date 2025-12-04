use std::collections::HashSet;
use std::io;

pub fn main() {
    let map = parse_input();
    let roll_locations = get_roll_locations(&map);
    println!("{}", count_accessible(&roll_locations));
    println!("{}", count_iter_accessible(&roll_locations));
}

fn count_accessible(roll_locations: &HashSet<(i32, i32)>) -> usize {
    roll_locations.iter().filter(|&&loc| is_accessible(loc, roll_locations)).count()
}

fn count_iter_accessible(roll_locations: &HashSet<(i32, i32)>) -> usize {
    let mut count = 0;
    let mut remaining = roll_locations.clone();
    loop {
        let new_count = count_accessible(&remaining);
        if new_count == 0 { break };
        count += new_count;
        remaining = remove_accessible(&remaining);
    }
    count
}

fn remove_accessible(roll_locations: &HashSet<(i32, i32)>) -> HashSet<(i32,i32)> {
    roll_locations
        .iter()
        .filter(|&&loc| !is_accessible(loc, roll_locations))
        .copied()
        .collect()
}

const OFFSETS: [(i32, i32); 8] = [(-1, -1), (-1, 0), (-1, 1), (0, -1), (0, 1), (1, -1), (1, 0), (1, 1)];

fn is_accessible((x, y): (i32, i32), roll_locations: &HashSet<(i32, i32)>) -> bool {
    OFFSETS.map(|(dx, dy)| roll_locations.contains(&(x+dx, y+dy)) as u32)
        .iter()
        .sum::<u32>() < 4
}

fn get_roll_locations(map: &[Vec<bool>]) -> HashSet<(i32, i32)> {
    let mut set = HashSet::new();
    for (y, map_row) in map.iter().enumerate() {
        for (x, &is_roll) in map_row.iter().enumerate() {
            if is_roll {
                set.insert((x as i32, y as i32));
            }
        }
    }
    set
}

fn parse_input() -> Vec<Vec<bool>> {
    io::stdin().lines()
        .map(|l| l.unwrap().trim_end().chars().map(|ch| ch == '@').collect())
        .collect()
}
