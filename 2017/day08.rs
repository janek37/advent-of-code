//! ```cargo
//! [dependencies]
//! regex = "1"
//! ```
use std::io;
use std::collections::HashMap;
use regex::Regex;

fn main() {
    let instructions = parse_input();
    let mut context: HashMap<String, i32> = HashMap::new();
    let mut max_intermediate_value = 0;
    for instruction in instructions {
        let new_value = run_instruction(&mut context, &instruction);
        if new_value > max_intermediate_value {
            max_intermediate_value = new_value
        }
    }
    let max_value = context.values().max().unwrap();
    println!("{}", max_value);
    println!("{}", max_intermediate_value);
}

fn run_instruction(context: &mut HashMap<String, i32>, i: &Instruction) -> i32 {
    let register_value = match context.get(&i.condition_register) {Some(v) => *v, None => 0};
    let comparison_result = match i.condition_operator {
        Comparison::Equal => register_value == i.condition_value,
        Comparison::Inequal => register_value != i.condition_value,
        Comparison::Less => register_value < i.condition_value,
        Comparison::Greater => register_value > i.condition_value,
        Comparison::LessOrEqual => register_value <= i.condition_value,
        Comparison::GreaterOrEqual => register_value >= i.condition_value,
    };
    if comparison_result {
        let affected_register_value = match context.get(&i.affected_register) {
            Some(v) => *v,
            None => 0
        };
        context.insert(i.affected_register.clone(), affected_register_value + i.summand);
        affected_register_value + i.summand
    } else {
        0
    }
}

enum Comparison {
    Equal,
    Inequal,
    Less,
    Greater,
    LessOrEqual,
    GreaterOrEqual,
}

struct Instruction {
    affected_register: String,
    summand: i32,
    condition_register: String,
    condition_operator: Comparison,
    condition_value: i32,
}

impl Instruction {
    fn from(s: &String) -> Instruction {
        let re = Regex::new(r"(.*) (inc|dec) (-?\d+) if (.*) (<|>|==|!=|<=|>=) (-?\d+)").unwrap();
        let (_, [ar, op, s, cr, cop, cv]) = re.captures(s).unwrap().extract();
        Instruction {
            affected_register: ar.to_string(),
            summand: s.parse::<i32>().unwrap() * (if op == "inc" {1} else {-1}),
            condition_register: cr.to_string(),
            condition_operator:
            if cop == "==" {
                Comparison::Equal
            } else if cop == "!=" {
                Comparison::Inequal
            } else if cop == "<" {
                Comparison::Less
            } else if cop == ">" {
                Comparison::Greater
            } else if cop == "<=" {
                Comparison::LessOrEqual
            } else { // if cop == ">=" {
                Comparison::GreaterOrEqual
            },
            condition_value: cv.parse::<i32>().unwrap(),
        }
    }
}

fn parse_input() -> Vec<Instruction> {
    io::stdin()
        .lines()
        .map(|line| Instruction::from(&line.unwrap()))
        .collect()
}
