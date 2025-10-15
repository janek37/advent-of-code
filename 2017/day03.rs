use std::io;
use std::collections::HashMap;

fn main() {
    let input = read_num();
    let xy = get_coord(input);
    println!("{}", xy.0.abs() + xy.1.abs());
    println!("{}", get_first_larger(input).unwrap());
}

fn get_coord(id: i32) -> (i32, i32) {
    let root = ((id as f32).sqrt().floor() as i32 + 1) / 2;
    let near_odd_square = (2*root-1)*(2*root-1);
    let offset = id - near_odd_square;
    if offset == 0 {
        (root - 1, root - 1)
    } else if offset <= 2 * root - 1 {
        (root, root - offset)
    } else if offset <= 4 * root - 1 {
        (3 * root - offset, -root)
    } else if offset <= 6 * root - 1 {
        (-root, offset - 5 * root)
    } else {
        (offset - 7 * root, root)
    }
}

struct Values {
    map: HashMap<(i32, i32), i32>,
    current: i32
}

impl Iterator for Values {
    type Item = i32;

    fn next(&mut self) -> Option<Self::Item> {
        let offsets = vec![(-1, -1), (-1, 0), (-1, 1), (0, -1), (0, 1), (1, -1), (1, 0), (1, 1)];
        let mut sum = if self.current == 1 { 1 } else { 0 };
        let xy = get_coord(self.current);
        for (dx, dy) in offsets.iter() {
            let neighbor = &(xy.0+dx, xy.1+dy);
            if self.map.contains_key(neighbor) {
                sum += self.map.get(neighbor).unwrap()
            }
        }
        self.map.insert(xy, sum);
        self.current += 1;
        Some(sum)
    }
}

fn get_first_larger(threshold: i32) -> Option<i32> {
    let values = Values { map: HashMap::new(), current: 1 };
    for value in values {
        if value > threshold {
            return Some(value)
        }
    }
    None
}

fn read_num() -> i32 {
    let mut input = String::new();
    io::stdin().read_line(&mut input).unwrap();
    input.trim().parse().unwrap()
}
