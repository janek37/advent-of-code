use std::collections::LinkedList;
use std::io;

pub fn main() {
    let program = parse_input();
    let mut context = Context::new();
    println!("{}", context.run_program(&program));
    println!("{}", run_duet(&program));
}

fn run_duet(program: &[Instruction]) -> i32 {
    let mut p0 = Context2::new(program.to_owned(), 0);
    let mut p1 = Context2::new(program.to_owned(), 1);
    let mut count = 0;
    loop {
        loop {
            let result = p0.next();
            match result {
                Some(Some(v)) => p1.buffer.push_back(v),
                _ => break,
            }
        }
        loop {
            let result = p1.next();
            match result {
                Some(Some(v)) => {
                    p0.buffer.push_back(v);
                    count += 1;
                },
                _ => break,
            }
        }
        if !p0.is_alive() || !p1.is_alive() || p0.buffer.is_empty() {
            break;
        }
    }
    count
}

#[derive(Clone, Copy)]
enum Value {
    Literal(i64),
    Register(char),
}

#[derive(Clone, Copy)]
enum Instruction {
    Send(Value),
    Receive(char),
    Set(char, Value),
    Add(char, Value),
    Mul(char, Value),
    Mod(char, Value),
    Jgz(Value, Value),
}

struct Context {
    values: Vec<i64>,
    sent: Option<i64>,
    received: Option<i64>,
}

impl Context {
    fn new() -> Self {
        Self { values: [0; 256].to_vec(), sent: None, received: None }
    }

    fn run_program(&mut self, program: &[Instruction]) -> i64 {
        let mut i = 0;
        while (0..program.len() as isize).contains(&i) {
            i += self.run_instruction(&program[i as usize]);
            if let Some(int) = self.received {
                return int;
            }
        }
        unreachable!()
    }

    fn run_instruction(&mut self, i: &Instruction) -> isize {
        match i {
            Instruction::Send(v) => {
                self.sent = Some(self.eval(v));
            },
            Instruction::Receive(r) => {
                if self.eval(&Value::Register(*r)) != 0 {
                    self.received = self.sent;
                }
            }
            Instruction::Set(r, v) => {
                self.values[*r as usize] = self.eval(v);
            },
            Instruction::Add(r, v) => {
                self.values[*r as usize] += self.eval(v);
            },
            Instruction::Mul(r, v) => {
                self.values[*r as usize] *= self.eval(v);
            },
            Instruction::Mod(r, v) => {
                self.values[*r as usize] %= self.eval(v);
            }
            Instruction::Jgz(v, offset) => {
                if self.eval(v) > 0 {
                    return self.eval(offset) as isize
                };
            }
        };
        1
    }

    fn eval(&self, v: &Value) -> i64 {
        match v {
            Value::Literal(n) => *n,
            Value::Register(r) => self.values[*r as usize],
        }
    }
}

struct Context2 {
    values: Vec<i64>,
    buffer: LinkedList<i64>,
    program: Vec<Instruction>,
    ip: usize,
}

impl Context2 {
    fn new(program: Vec<Instruction>, pid: i64) -> Self {
        let mut values = [0; 256].to_vec();
        values['p' as usize] = pid;
        Self { values, buffer: LinkedList::new(), program, ip: 0}
    }

    fn run_instruction(&mut self) -> (Option<i64>, bool) {
        let mut ip_offset = 1;
        let mut sent = None;
        let mut waiting = false;
        match self.program[self.ip] {
            Instruction::Send(v) => {
                sent = Some(self.eval(v));
            },
            Instruction::Receive(r) => {
                if let Some(v) = self.buffer.pop_front() {
                    self.values[r as usize] = v;
                } else {
                    ip_offset = 0;
                    waiting = true;
                }
            },
            Instruction::Set(r, v) => {
                self.values[r as usize] = self.eval(v);
            },
            Instruction::Add(r, v) => {
                self.values[r as usize] += self.eval(v);
            },
            Instruction::Mul(r, v) => {
                self.values[r as usize] *= self.eval(v);
            },
            Instruction::Mod(r, v) => {
                self.values[r as usize] %= self.eval(v);
            }
            Instruction::Jgz(v, offset) => {
                if self.eval(v) > 0 {
                    ip_offset = self.eval(offset) as isize;
                };
            }
        };
        self.ip = (self.ip as isize + ip_offset) as usize;
        (sent, waiting)
    }

    fn eval(&self, v: Value) -> i64 {
        match v {
            Value::Literal(n) => n,
            Value::Register(r) => self.values[r as usize],
        }
    }

    fn is_alive(&self) -> bool {
        (0..self.program.len()).contains(&self.ip)
    }
}

impl Iterator for Context2 {
    type Item = Option<i64>;

    fn next(&mut self) -> Option<Self::Item> {
        let mut sent;
        let mut waiting;
        while self.is_alive() {
            (sent, waiting) = self.run_instruction();
            if sent.is_some() || waiting {
                return Some(sent);
            }
        }
        None
    }
}

fn parse_input() -> Vec<Instruction> {
    io::stdin().lines().map(|line| parse_line(&line.unwrap())).collect()
}

fn parse_line(line: &str) -> Instruction {
    let parts: Vec<_> = line.split_whitespace().collect();
    match parts[0] {
        "snd" => Instruction::Send(parse_value(parts[1])),
        "rcv" => Instruction::Receive(parse_register(parts[1])),
        "set" => Instruction::Set(parse_register(parts[1]), parse_value(parts[2])),
        "add" => Instruction::Add(parse_register(parts[1]), parse_value(parts[2])),
        "mul" => Instruction::Mul(parse_register(parts[1]), parse_value(parts[2])),
        "mod" => Instruction::Mod(parse_register(parts[1]), parse_value(parts[2])),
        "jgz" => Instruction::Jgz(parse_value(parts[1]), parse_value(parts[2])),
        _ => unreachable!(),
    }
}

fn parse_value(s: &str) -> Value {
    let maybe_num: Result<i64, _> = s.parse();
    if let Ok(int) = maybe_num {
        Value::Literal(int)
    } else {
        Value::Register(parse_register(s))
    }
}

fn parse_register(s: &str) -> char {
    s.chars().next().unwrap()
}
