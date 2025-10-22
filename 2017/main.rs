use std::env;

mod day01;
mod day02;
mod day03;
mod day04;
mod day05;
mod day06;
mod day07;
mod day08;
mod day09;
mod day10;
mod day11;
mod day12;
mod day13;

fn main() {
    let day_arg = env::args().nth(1).expect("Day argument required");
    let day = day_arg.parse::<usize>().expect(&format!("Not a valid number: {}", day_arg));
    let days: &[fn()] = &[
        day01::main,
        day02::main,
        day03::main,
        day04::main,
        day05::main,
        day06::main,
        day07::main,
        day08::main,
        day09::main,
        day10::main,
        day11::main,
        day12::main,
        day13::main,
    ];

    if let Some(func) = days.get(day - 1) {
        func();
    } else {
        panic!("Day {} not found", day);
    }
}
