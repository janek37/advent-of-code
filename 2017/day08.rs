use std::io;
use std::collections::HashMap;

pub fn main() {
    let instructions = parse_input();
    let mut context: HashMap<String, i32> = HashMap::new();
    let mut max_intermediate_value = 0;
    for instruction in instructions {
        if let Some(v) = run_instruction(&mut context, &instruction) && v > max_intermediate_value {
            max_intermediate_value = v
        }
    }
    let max_value = context.values().max().unwrap();
    println!("{}", max_value);
    println!("{}", max_intermediate_value);
}

fn run_instruction(context: &mut HashMap<String, i32>, i: &Instruction) -> Option<i32> {
    let register_value = *context.get(&i.condition_register).unwrap_or(&0);
    let ok = match i.condition_operator {
        Comparison::Equal => register_value == i.condition_value,
        Comparison::Inequal => register_value != i.condition_value,
        Comparison::Less => register_value < i.condition_value,
        Comparison::Greater => register_value > i.condition_value,
        Comparison::LessOrEqual => register_value <= i.condition_value,
        Comparison::GreaterOrEqual => register_value >= i.condition_value,
    };
    if ok {
        let ptr = context.entry(i.affected_register.clone()).or_insert(0);
        *ptr += i.summand;
        Some(*ptr)
    } else {
        None
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
    fn from(s: &str) -> Instruction {
        let parts: Vec<_> = s.split_whitespace().collect();
        //Regex::new(r"(.*) (inc|dec) (-?\d+) if (.*) (<|>|==|!=|<=|>=) (-?\d+)").unwrap();
        let ar = parts[0];
        let op = parts[1];
        let val = parts[2].parse::<i32>().unwrap();
        // parts[3] == "if"
        let cr = parts[4];
        let cop = parts[5];
        let cv = parts[6].parse::<i32>().unwrap();

        let condition_operator = match cop {
            "==" => Comparison::Equal,
            "!=" => Comparison::Inequal,
            "<" => Comparison::Less,
            ">" => Comparison::Greater,
            "<=" => Comparison::LessOrEqual,
            ">=" => Comparison::GreaterOrEqual,
            _ => unreachable!(),
        };

        Instruction {
            affected_register: ar.to_string(),
            summand: if op == "inc" { val } else { -val },
            condition_register: cr.to_string(),
            condition_operator,
            condition_value: cv,
        }
    }
}

fn parse_input() -> Vec<Instruction> {
    io::stdin()
        .lines()
        .map(|line| Instruction::from(&line.unwrap()))
        .collect()
}
