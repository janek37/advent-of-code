use std::io;

pub fn main() {
    let moves = parse_input();
    let chars: Vec<char> = "abcdefghijklmnop".chars().collect();
    let permutation = DancePermutation::from_moves(16, &moves);
    println!("{}", permutation.apply(&chars).iter().collect::<String>());
    let power_permutation = permutation.power(1_000_000_000);
    println!("{}", power_permutation.apply(&chars).iter().collect::<String>());
}

enum DanceMove {
    Spin(usize),
    Exchange(usize, usize),
    Partner(char, char),
}

struct DancePermutation {
    size: usize,
    positions: Vec<usize>,
    translations: Vec<usize>,
}

impl DancePermutation {
    fn new(size: usize) -> Self {
        Self { size, positions: (0..size).collect(), translations: (0..size).collect() }
    }

    fn from_moves(size: usize, moves: &[DanceMove]) -> Self {
        moves.iter().fold(DancePermutation::new(size), |p, m| p.apply_move(m))
    }

    fn from_move(size: usize, move_: &DanceMove) -> Self {
        let mut perm = DancePermutation::new(size);
        match move_ {
            DanceMove::Spin(n) => {
                perm.positions.rotate_right(*n);
            },
            DanceMove::Exchange(i, j) => {
                perm.positions.swap(*i, *j);
            },
            DanceMove::Partner(a, b) => {
                let i = *a as usize - ('a' as usize);
                let j = *b as usize - ('a' as usize);
                perm.translations.swap(i, j);
            },
        };
        perm
    }

    fn apply<T: Copy>(&self, sequence: &[T]) -> Vec<T> {
        (0..self.size).map(|i| sequence[self.translations[self.positions[i]]]).collect()
    }

    fn apply_move(&self, move_: &DanceMove) -> Self {
        let move_perm = &DancePermutation::from_move(self.size, move_);
        Self {
            size: self.size,
            positions: apply_perm(&move_perm.positions, &self.positions),
            translations: apply_perm(&self.translations, &move_perm.translations),
        }
    }

    fn power(&self, n: usize) -> Self {
        Self {
            size: self.size,
            positions: cycles_to_perm(perm_to_cycles(&self.positions), n),
            translations: cycles_to_perm(perm_to_cycles(&self.translations), n),
        }
    }
}

fn apply_perm(perm: &[usize], sequence: &[usize]) -> Vec<usize> {
    (0..perm.len()).map(|i| sequence[perm[i]]).collect()
}

fn perm_to_cycles(perm: &[usize]) -> Vec<Vec<usize>> {
    let mut visited: Vec<bool> = perm.iter().map(|_| false).collect();
    let mut current = 0;
    let mut cycles = Vec::new();
    while current < perm.len() {
        if visited[current] { current += 1; continue }
        let mut cycle = vec![current];
        loop {
            let last = *cycle.last().unwrap();
            visited[last] = true;
            let next = perm[last];
            if next == current { break }
            cycle.push(next);
        }
        cycles.push(cycle);
    }
    cycles
}

fn cycles_to_perm(cycles: Vec<Vec<usize>>, step: usize) -> Vec<usize> {
    let size = cycles.iter().map(|c| c.len()).sum();
    let mut perm: Vec<usize> = (0..size).collect();
    for cycle in cycles {
        for (a, b) in cycle.iter().zip(cycle.iter().cycle().skip(step % cycle.len())) {
            perm[*a] = *b;
        }
    }
    perm
}

fn parse_input() -> Vec<DanceMove> {
    let mut buf = String::new();
    io::stdin().read_line(&mut buf).unwrap();
    buf.trim_end().split(',').map(parse_move).collect()
}

fn parse_move(s: &str) -> DanceMove {
    let first = s.chars().next().unwrap();
    match first {
        's' => DanceMove::Spin(s[1..].parse().unwrap()),
        'x' => {
            let pp: Vec<&str> =  s[1..].split('/').take(2).collect();
            let parts: Vec<usize> = pp.iter().map(|p| p.parse().unwrap()).collect();
            DanceMove::Exchange(parts[0], parts[1])
        },
        'p' => DanceMove::Partner(s.chars().nth(1).unwrap(), s.chars().nth(3).unwrap()),
        _ => unreachable!(),
    }
}
