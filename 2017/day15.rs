use std::io;

pub fn main() {
    let (a_start, b_start) = parse_input();
    println!("{}", count_matches(a_start, b_start));
    println!("{}", count_matches_2(a_start, b_start));
}

fn count_matches(a_start: u64, b_start: u64) -> u32 {
    let mut count = 0;
    let mut a = a_start;
    let mut b = b_start;
    for _ in 0..40_000_000 {
        a = (a * 16807) % 2147483647;
        b = (b * 48271) % 2147483647;
        if a & 0xffff == b & 0xffff {
            count += 1;
        }
    }
    count
}

fn count_matches_2(a_start: u64, b_start: u64) -> u32 {
    let mut count = 0;
    let mut a = a_start;
    let mut b = b_start;
    for _ in 0..5_000_000 {
        loop {
            a = (a * 16807) % 2147483647;
            if a & 3 == 0 {
                break
            }
        }
        loop {
            b = (b * 48271) % 2147483647;
            if b & 7 == 0 {
                break
            }
        }
        if a & 0xffff == b & 0xffff {
            count += 1;
        }
    }
    count
}

fn parse_input() -> (u64, u64) {
    (parse_line(), parse_line())
}

fn parse_line() -> u64 {
    let mut buf = String::new();
    io::stdin().read_line(&mut buf).unwrap();
    // Generator X starts with n
    buf[24..].trim_end().parse().unwrap()
}
