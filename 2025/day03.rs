use std::io;

pub fn main() {
    let banks = parse_input();
    println!("{}", banks.iter().map(|b| max_joltage(b, 2, 0)).sum::<u64>());
    println!("{}", banks.iter().map(|b| max_joltage(b, 12, 0)).sum::<u64>());
}

fn max_joltage(bank: &[u32], digits: usize, acc: u64) -> u64 {
    if digits == 1 {
        return acc * 10 + *bank.iter().max().unwrap() as u64;
    }
    let (i, first) = bank[..bank.len() - digits + 1]
        .iter()
        .enumerate()
        .max_by_key(|&(i, j)| (j, -(i as i32)))
        .unwrap();
    max_joltage(&bank[i+1..], digits - 1, acc * 10 + (*first as u64))
}

fn parse_input() -> Vec<Vec<u32>> {
    io::stdin()
        .lines()
        .map(
            |l|
            l.unwrap()
            .chars()
            .map(|ch| ch.to_digit(10).unwrap())
            .collect()
        )
        .collect()
}


#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_max_joltage() {
        assert_eq!(max_joltage(&[9,8,7,6,5,4,3,2,1,1,1,1,1,1,1], 2, 0), 98);
        assert_eq!(max_joltage(&[8,1,1,1,1,1,1,1,1,1,1,1,1,1,9], 2, 0), 89);
        assert_eq!(max_joltage(&[2,3,4,2,3,4,2,3,4,2,3,4,2,7,8], 2, 0), 78);
        assert_eq!(max_joltage(&[8,1,8,1,8,1,9,1,1,1,1,2,1,1,1], 2, 0), 92);
        assert_eq!(max_joltage(&[9,8,7,6,5,4,3,2,1,1,1,1,1,1,1], 12, 0), 987654321111);
        assert_eq!(max_joltage(&[8,1,1,1,1,1,1,1,1,1,1,1,1,1,9], 12, 0), 811111111119);
        assert_eq!(max_joltage(&[2,3,4,2,3,4,2,3,4,2,3,4,2,7,8], 12, 0), 434234234278);
        assert_eq!(max_joltage(&[8,1,8,1,8,1,9,1,1,1,1,2,1,1,1], 12, 0), 888911112111);
    }
}
