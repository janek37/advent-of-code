use std::io;

const SUFFIX: [u8; 5] = [17, 31, 73, 47, 23];

pub fn main() {
    let input = get_input();
    let lengths = parse_input(&input);
    let mut hash = KnotHash::new();
    hash.apply_lengths(&lengths, 1);
    println!("{}", hash.fingerprint());
    println!("{}", KnotHash::hex_digest_from_str(&input));
}

pub struct KnotHash {
    list: Vec<u8>
}

impl KnotHash {
    fn new() -> Self {
        Self { list: (0..=255).collect() }
    }

    fn fingerprint(&self) -> u32 {
        (self.list[0] as u32) * (self.list[1] as u32)
    }

    fn hex_digest_from_str(s: &str) -> String {
        let digest = KnotHash::digest_from_str(s);
        digest.iter().map(|n| format!("{:02x}", n)).collect::<Vec<_>>().join("")
    }

    fn digest(&self) -> Vec<u8> {
        self.list
            .chunks(16)
            .map(|chunk| chunk.iter().copied().reduce(|a, b| a ^ b).unwrap())
            .collect()
    }

    pub fn digest_from_str(s: &str) -> Vec<u8> {
        let mut hash = KnotHash::new();
        hash.apply_lengths(&KnotHash::lengths_from_str(s), 64);
        hash.digest()
    }

    fn lengths_from_str(s: &str) -> Vec<u8> {
        s.as_bytes()
            .iter()
            .copied()
            .chain(SUFFIX)
            .collect::<Vec<_>>()
    }

    fn apply_lengths(&mut self, lengths: &[u8], rounds: u32) {
        let mut current_pos = 0;
        let mut skip = 0;
        for _ in 0..rounds {
            for &length in lengths {
                self.reverse(current_pos, length);
                current_pos = current_pos.wrapping_add(length).wrapping_add(skip);
                skip = skip.wrapping_add(1);
            }
        }
    }

    fn reverse(&mut self, from: u8, length: u8) {
        for i in 0..length / 2 {
            let a = from.wrapping_add(i);
            let b = from.wrapping_add(length.wrapping_sub(1)).wrapping_sub(i);
            self.list.swap(a as usize, b as usize);
        }
    }
}

fn parse_input(input: &str) -> Vec<u8> {
    input.split(',').map(|s| s.parse().unwrap()).collect()
}

fn get_input() -> String {
    let mut buffer = String::new();
    io::stdin().read_line(&mut buffer).unwrap();
    buffer.trim_end().to_string()
}
