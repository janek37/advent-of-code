use std::io;

pub fn main() {
    let (a_start, b_start) = parse_input();
    println!("{}", count_matches(a_start, b_start));
    println!("{}", count_matches_2(a_start, b_start));
}

fn count_matches(a_start: u32, b_start: u32) -> u32 {
    let mut count = 0;
    let mut a = a_start;
    let mut b = b_start;
    for _ in 0..40_000_000 {
        a = next_a(a);
        b = next_b(b);
        if a & 0xffff == b & 0xffff {
            count += 1;
        }
    }
    count
}

fn count_matches_2(a_start: u32, b_start: u32) -> u32 {
    let mut count = 0;
    let mut a = a_start;
    let mut b = b_start;
    for _ in 0..5_000_000 {
        loop {
            a = next_a(a);
            if a & 3 == 0 { break }
        }
        loop {
            b = next_b(b);
            if b & 7 == 0 { break }
        }
        if a & 0xffff == b & 0xffff {
            count += 1;
        }
    }
    count
}

#[inline(always)]
fn next_a(a: u32) -> u32 {
    mod_7fff_ffff((a as u64) * 16807)
}

#[inline(always)]
fn next_b(b: u32) -> u32 {
    mod_7fff_ffff((b as u64) * 48271)
}

#[inline(always)]
fn mod_7fff_ffff(n: u64) -> u32 {
    let modulus = 0x7fff_ffff;
    let mut m = (n & modulus) + (n >> 31);
    if m >= modulus { m -= modulus; }
    m as u32
}

fn parse_input() -> (u32, u32) {
    (parse_line(), parse_line())
}

fn parse_line() -> u32 {
    let mut buf = String::new();
    io::stdin().read_line(&mut buf).unwrap();
    // Generator X starts with n
    buf[24..].trim_end().parse().unwrap()
}
