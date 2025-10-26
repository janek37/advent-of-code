use std::io;

pub fn main() {
    let steps = read_num();
    println!("{}", value_after_n(steps, 2017));
    println!("{}", value_after_zero(steps, 50_000_000));
}

fn value_after_n(steps: usize, n: usize) -> usize {
    let mut buffer: Vec<usize> = vec![0];
    let mut pos = 0;
    for i in 1..=n {
        pos = (pos + steps) % i + 1;
        buffer.insert(pos, i);
    }
    buffer[(pos + 1) % (n + 1)]
}

fn value_after_zero(steps: usize, n: usize) -> usize {
    let mut pos = 0;
    let mut value = 0;
    for i in 1..=n {
        pos = (pos + steps) % i + 1;
        if pos == 1 {
            value = i;
        }
    }
    value
}

fn read_num() -> usize {
    let mut input = String::new();
    io::stdin().read_line(&mut input).unwrap();
    input.trim().parse().unwrap()
}
