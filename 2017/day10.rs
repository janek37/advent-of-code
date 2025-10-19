use std::io;

const SIZE: u32 = 256;
const SUFFIX: [u32; 5] = [17, 31, 73, 47, 23];

fn main() {
    let input = get_input();
    let lengths = parse_input(&input);
    let mut circular_list = CircularList::new(SIZE);
    circular_list.apply_lengths(&lengths, 1);
    println!("{}", circular_list.fingerprint());

    let new_lengths: Vec<u32> = input
        .chars()
        .map(|ch| ch as u32)
        .chain(SUFFIX.iter().map(|n| *n))
        .collect();
    let mut new_circular_list = CircularList::new(SIZE);
    new_circular_list.apply_lengths(&new_lengths, 64);
    println!("{}", new_circular_list.hex_digest());
}

struct CircularList {
    list: Vec<u32>
}

impl CircularList {
    fn new(size: u32) -> CircularList {
        CircularList {
            list: (0..size).collect()
        }
    }

    fn fingerprint(&self) -> u32 {
        self.list[0] * self.list[1]
    }

    fn hex_digest(&self) -> String {
        self.digest().iter().map(|n| format!("{:02x}", n)).collect::<Vec<_>>().join("")
    }

    fn digest(&self) -> Vec<u8> {
        (0..16)
            .map(
                |i|
                self.list[16*i..16*(i+1)]
                    .iter()
                    .map(|n| *n)
                    .reduce(|a, b| a ^ b)
                    .unwrap() as u8
            )
            .collect()
    }

    fn apply_lengths(&mut self, lengths: &Vec<u32>, rounds: u32) {
        let mut current_pos = 0;
        let mut skip = 0;
        for _ in 0..rounds {
            for length in lengths.iter() {
                if *length > SIZE {
                    panic!("Invalid length");
                }
                self.reverse(current_pos, *length);
                current_pos += length + skip;
                current_pos %= SIZE;
                skip += 1;
            }
            }
    }

    fn reverse(&mut self, from: u32, length: u32) {
        self.list = (0..SIZE)
            .map(
                |i|
                if (i < from && i + SIZE >= from + length) || i >= from + length {
                    self.list[i as usize]
                } else {
                    let source_index = (2*from + length - 1 - i) % SIZE;
                    self.list[source_index as usize]
                }
            )
            .collect()
    }
}

fn parse_input(input: &String) -> Vec<u32> {
    input.split(",").map(|s| s.parse().unwrap()).collect()
}

fn get_input() -> String {
    let mut buffer = String::new();
    io::stdin().read_line(&mut buffer).unwrap();
    buffer.trim_end().to_string()
}
