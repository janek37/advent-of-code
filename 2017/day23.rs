use std::io;
use crate::day23::Value::{Literal, Register};
use crate::day23::Instruction::{Set, Mul, Sub, Jnz, Pri, Nop};

pub fn main() {
    let program = parse_input();
    let mut context = Context::new();
    println!("{}", context.run_program(&program));
    context = Context::new();
    context.run_instruction(&Set('a', Literal(1)));
    context.run_program(&optimize(&program));
    println!("{}", context.eval(&Register('h')));
}

static PRIME_TEST: [Instruction; 16] = [
    Set('f', Literal(1)),
    Set('d', Literal(2)),
    Set('e', Literal(2)),
    Set('g', Register('d')),
    Mul('g', Register('e')),
    Sub('g', Register('b')),
    Jnz(Register('g'), Literal(2)),
    Set('f', Literal(0)),
    Sub('e', Literal(-1)),
    Set('g', Register('e')),
    Sub('g', Register('b')),
    Jnz(Register('g'), Literal(-8)),
    Sub('d', Literal(-1)),
    Set('g', Register('d')),
    Sub('g', Register('b')),
    Jnz(Register('g'), Literal(-13)),
];

fn optimize(program: &[Instruction]) -> Vec<Instruction> {
    let mut new_program: Vec<Instruction> = vec![];
    // a very targeted and well-informed optimization
    for i in 0..program.len() - 16 {
        if program[i..i+16].iter().zip(PRIME_TEST).all(|(&i1, i2)| i1 == i2) {
            new_program.push(Pri('f', Register('b')));
            new_program.extend(vec![Nop; 15]);
            new_program.extend(&program[i+16..]);
            break;
        }
        new_program.push(program[i]);
    }
    new_program
}

#[derive(Clone, Copy, PartialEq)]
enum Value {
    Literal(i32),
    Register(char),
}

#[derive(Clone, Copy, PartialEq)]
enum Instruction {
    Set(char, Value),
    Sub(char, Value),
    Mul(char, Value),
    Jnz(Value, Value),
    Pri(char, Value),
    Nop,
}

struct Context {
    values: Vec<i32>,
}

impl Context {
    fn new() -> Self {
        Self { values: vec![0; 256] }
    }

    fn run_program(&mut self, program: &[Instruction]) -> i32 {
        let mut i = 0;
        let mut mul_count = 0;
        while (0..program.len() as isize).contains(&i) {
            if let Mul(_, _) = program[i as usize] {
                mul_count += 1;
            }
            i += self.run_instruction(&program[i as usize]);
        }
        mul_count
    }

    fn run_instruction(&mut self, i: &Instruction) -> isize {
        match i {
            Set(r, v) => {
                self.values[*r as usize] = self.eval(v);
            },
            Sub(r, v) => {
                self.values[*r as usize] -= self.eval(v);
            },
            Mul(r, v) => {
                self.values[*r as usize] *= self.eval(v);
            },
            Jnz(v, offset) => {
                if self.eval(v) != 0 {
                    return self.eval(offset) as isize
                };
            },
            Pri(r, v) => {
                self.values[*r as usize] = is_prime(self.eval(v)) as i32;
            },
            Nop => {},
        };
        1
    }

    fn eval(&self, v: &Value) -> i32 {
        match v {
            Literal(n) => *n,
            Register(r) => self.values[*r as usize],
        }
    }
}

fn is_prime(n: i32) -> bool {
    if n < 2 { return false }
    for d in 2..=(n as f64).sqrt().floor() as i32 {
        if n % d == 0 {
            return false;
        }
    }
    true
}

fn parse_input() -> Vec<Instruction> {
    io::stdin().lines().map(|line| parse_line(&line.unwrap())).collect()
}

fn parse_line(line: &str) -> Instruction {
    let parts: Vec<_> = line.split_whitespace().collect();
    match parts[0] {
        "set" => Set(parse_register(parts[1]), parse_value(parts[2])),
        "sub" => Sub(parse_register(parts[1]), parse_value(parts[2])),
        "mul" => Mul(parse_register(parts[1]), parse_value(parts[2])),
        "jnz" => Jnz(parse_value(parts[1]), parse_value(parts[2])),
        _ => unreachable!(),
    }
}

fn parse_value(s: &str) -> Value {
    let maybe_num: Result<i32, _> = s.parse();
    if let Ok(int) = maybe_num {
        Literal(int)
    } else {
        Register(parse_register(s))
    }
}

fn parse_register(s: &str) -> char {
    s.chars().next().unwrap()
}
