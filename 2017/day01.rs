use std::io;

pub fn main() {
    let mut line = String::new();
    let _ = io::stdin().read_line(&mut line);
    let digits = line.trim_end();
    let mut sum = 0;
    for (digit, next_digit) in digits.chars().zip(digits.chars().cycle().skip(1)) {
        if digit == next_digit {
            sum += digit.to_digit(10).unwrap();
        }
    }
    println!("{}", sum);
    let mut sum2 = 0;
    for (digit, next_digit) in digits.chars().zip(digits.chars().cycle().skip(digits.len() / 2)) {
        if digit == next_digit {
            sum2 += digit.to_digit(10).unwrap();
        }
    }
    println!("{}", sum2);
}
