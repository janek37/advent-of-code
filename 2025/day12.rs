use std::io;

pub fn main() {
    let (shapes, cases) = parse_input();
    let mut count = 0;
    for case in cases {
        if is_trivially_possible(&case) {
            count += 1;
        } else {
            assert!(is_trivially_impossible(&shapes, &case), "actually non-trivial case!");
        }
    }
    println!("{}", count)
}

struct Case {
    region: (u32, u32),
    shape_counts: Vec<u32>,
}

fn is_trivially_possible(case: &Case) -> bool {
    // every shape fits into a 3x3 box
    let num_shapes = case.shape_counts.iter().sum();
    (case.region.0 / 3) * (case.region.1 / 3) >= num_shapes
}

fn is_trivially_impossible(shapes: &[u32], case: &Case) -> bool {
    let total_shape_area = case.shape_counts.iter().enumerate().map(|(i, &num)| shapes[i] * num).sum();
    case.region.0 * case.region.1 < total_shape_area
}

fn parse_input() -> (Vec<u32>, Vec<Case>) {
    let mut shape_sizes = Vec::new();
    let mut shape_lines = 0;
    let mut shape_size = 0;
    let mut cases = Vec::new();
    for line in io::stdin().lines() {
        let raw_line = line.unwrap();
        if raw_line.contains('x') {
            cases.push(parse_case(&raw_line));
        } else if raw_line.contains('.') || raw_line.contains('#') {
            // ignore the shape, just count the squares
            shape_size += raw_line.chars().filter(|&ch| ch == '#').count();
            shape_lines += 1;
            if shape_lines == 3 {
                shape_sizes.push(shape_size as u32);
                shape_size = 0;
                shape_lines = 0;
            }
        }
    }
    (shape_sizes, cases)
}

fn parse_case(line: &str) -> Case {
    let parts: Vec<&str> = line.split_whitespace().collect();
    let [width, height] = parts[0]
        .trim_end_matches(':')
        .split('x')
        .map(|s| s.parse().unwrap())
        .collect::<Vec<u32>>()
        .try_into()
        .unwrap();
    Case { region: (width, height), shape_counts: parts[1..].iter().map(|s| s.parse().unwrap()).collect() }
}
