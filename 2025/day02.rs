use std::io;

pub fn main() {
    let ranges = parse_input();
    println!("{}", ranges.iter().map(|r| sum_invalid_ids(r, 2)).sum::<u64>());
    println!("{}", ranges.iter().map(sum_all_invalid_ids).sum::<u64>())
}

struct IDRange {
    start: String,
    end: String,
}

fn sum_invalid_ids(range: &IDRange, repetitions: usize) -> u64 {
    let lower_root = find_next_invalid_root(&range.start, repetitions);
    let upper_root = find_previous_invalid_root(&range.end, repetitions);
    if lower_root > upper_root {
        return 0;
    }
    assert_eq!(count_digits(lower_root), count_digits(upper_root)); // simplifying assumption
    (lower_root..=upper_root).sum::<u64>() * multiplier(count_digits(lower_root), repetitions)
}

fn sum_all_invalid_ids(range: &IDRange) -> u64 {
    // assumption: no numbers above 10 digits
    sum_invalid_ids(range, 2)
        + sum_invalid_ids(range, 3)
        + sum_invalid_ids(range, 5)
        - sum_invalid_ids(range, 6)
        + sum_invalid_ids(range, 7)
        - sum_invalid_ids(range, 10)
}

fn find_next_invalid_root(id: &str, repetitions: usize) -> u64 {
    let parsed_id: u64 = id.parse().unwrap();
    let root_len = id.len().div_ceil(repetitions);
    if id.len().is_multiple_of(repetitions) {
        let root: u64 = id[0..root_len].parse().unwrap();
        if root * multiplier(root_len as u32, repetitions) < parsed_id {
            root + 1
        } else {
            root
        }
    } else {
        10u64.pow(root_len as u32 - 1)
    }
}

fn find_previous_invalid_root(id: &str, repetitions: usize) -> u64 {
    let parsed_id: u64 = id.parse().unwrap();
    let root_len = id.len() / repetitions;
    if id.len().is_multiple_of(repetitions) {
        let root: u64 = id[0..root_len].parse().unwrap();
        if root * multiplier(root_len as u32, repetitions) > parsed_id {
            root - 1
        } else {
            root
        }
    } else {
        10u64.pow(root_len as u32) - 1
    }
}

fn count_digits(n: u64) -> u32 {
    n.ilog10() + 1
}

fn multiplier(root_len: u32, repetitions: usize) -> u64 {
    (0..repetitions).map(|i| 10u64.pow(root_len * (i as u32))).sum()
}

fn parse_input() -> Vec<IDRange> {
    let mut buffer = String::new();
    io::stdin().read_line(&mut buffer).unwrap();
    buffer.trim_end().split(',').map(parse_range).collect()
}

fn parse_range(s: &str) -> IDRange {
    let nums: Vec<&str> = s.split('-').take(2).collect();
    IDRange { start: nums[0].to_string(), end: nums[1].to_string() }
}


#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_next_root() {
        assert_eq!(find_next_invalid_root("100000", 2), 100);
        assert_eq!(find_next_invalid_root("123456", 2), 124);
        assert_eq!(find_next_invalid_root("9999", 2), 99);
        assert_eq!(find_next_invalid_root("123", 2), 10);
    }

    #[test]
    fn test_previous_root() {
        assert_eq!(find_previous_invalid_root("100000", 2), 99);
        assert_eq!(find_previous_invalid_root("123456", 2), 123);
        assert_eq!(find_previous_invalid_root("123000", 2), 122);
        assert_eq!(find_previous_invalid_root("123", 2), 9);
    }
}
