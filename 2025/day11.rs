use std::collections::HashMap;
use std::io;

pub fn main() {
    let machines = parse_input();
    println!("{}", count_paths(&machines, "you", "out"));
    println!("{}", count_paths2(&machines, "svr", "out", "dac", "fft"));
}

fn count_paths(machines: &HashMap<String, Vec<String>>, start: &str, end: &str) -> usize {
    let mut stack = vec![start];
    let mut path_counts: HashMap<&str, usize> = HashMap::from([(end, 1)]);
    while let Some(machine) = stack.pop() {
        if !path_counts.contains_key(machine) {
            stack.push(machine);
            let mut sum = Some(0);
            for output in machines[machine].iter() {
                if let Some(count) = path_counts.get(output as &str) {
                    sum = sum.map(|s| s + count);
                } else {
                    sum = None;
                    stack.push(output);
                }
            }
            if let Some(s) = sum {
                path_counts.insert(machine, s);
            }
        }
    }
    path_counts[start]
}

fn count_paths2(machines: &HashMap<String, Vec<String>>, start: &str, end: &str, via1: &str, via2: &str) -> usize {
    let mut stack = vec![(start, 0)];
    let mut path_counts: HashMap<(&str, u8), usize> = HashMap::from(
        [((end, 2), 1), ((end, 1), 0), ((end, 0), 0)]
    );
    while let Some((machine, c)) = stack.pop() {
        if !path_counts.contains_key(&(machine, c)) {
            stack.push((machine, c));
            let new_c = if machine == via1 || machine == via2 { c + 1 } else { c };
            let mut sum = Some(0);
            for output in machines[machine].iter() {
                let key = (output as &str, new_c);
                if let Some(count) = path_counts.get(&key) {
                    sum = sum.map(|s| s + count);
                } else {
                    sum = None;
                    stack.push(key);
                }
            }
            if let Some(s) = sum {
                path_counts.insert((machine, c), s);
            }
        }
    }
    path_counts[&(start, 0)]
}

fn parse_input() -> HashMap<String, Vec<String>> {
    io::stdin().lines().map(|line| parse_line(&line.unwrap())).collect()
}

fn parse_line(line: &str) -> (String, Vec<String>) {
    let parts: Vec<&str> = line.split_whitespace().collect();
    (parts[0].trim_end_matches(':').to_owned(), parts[1..].iter().map(|s| s.to_string()).collect())
}
