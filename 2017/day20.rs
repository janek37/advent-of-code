use std::io;

pub fn main() {
    let particles = parse_input();
    let (idx, _) = particles.iter().enumerate()
        .min_by_key(|&(_, p)| (norm(p.acceleration), norm(p.velocity))) // wrong in general but works for me
        .unwrap();
    println!("{}", idx);
    println!("{}", simulate_collisions(&particles));
}

fn simulate_collisions(initial_particles: &[Particle]) -> usize {
    let mut particles = initial_particles.to_vec();
    for _ in 0..100 { // yes, I know
        particles = particles.iter().map(|p| p.step()).collect();
        particles = remove_collisions(&particles);
    }
    particles.len()
}

fn remove_collisions(particles: &[Particle]) -> Vec<Particle> {
    let mut collisions = vec![false; particles.len()];
    let mut sorted = particles.to_vec();
    sorted.sort_by_key(|p| norm(p.position));
    for (i, p1) in sorted.iter().enumerate() {
        for (j, p2) in sorted.iter().enumerate().skip(i + 1) {
            if p1.position == p2.position {
                collisions[i] = true;
                collisions[j] = true;
                break;
            }
            if norm(p1.position) != norm(p2.position) {
                break;
            }
        }
    }
    let mut remaining: Vec<Particle> = Vec::new();
    for (particle, collided) in sorted.iter().copied().zip(collisions) {
        if !collided { remaining.push(particle) }
    }
    remaining
}

fn norm(x: (i32, i32, i32)) -> i32 {
    x.0.abs() + x.1.abs() + x.2.abs()
}

fn add(x: (i32, i32, i32), y: (i32, i32, i32)) -> (i32, i32, i32) {
    (x.0 + y.0, x.1 + y.1, x.2 + y.2)
}

#[derive(Copy, Clone)]
struct Particle {
    position: (i32, i32, i32),
    velocity: (i32, i32, i32),
    acceleration: (i32, i32, i32),
}

impl Particle {
    fn step(&self) -> Self {
        let velocity = add(self.velocity, self.acceleration);
        let position = add(self.position, velocity);
        Self {
            position, velocity, acceleration: self.acceleration
        }
    }
}

fn parse_input() -> Vec<Particle> {
    io::stdin().lines().map(|line| parse_line(&line.unwrap())).collect()
}

fn parse_line(line: &str) -> Particle {
    let parts: Vec<_> = line.split_whitespace().collect();
    Particle {
        position: parse_tuple(parts[0]),
        velocity: parse_tuple(parts[1]),
        acceleration: parse_tuple(parts[2]),
    }
}

fn parse_tuple(s: &str) -> (i32, i32, i32) {
    // x=<-4019,1982,4446>,
    let trimmed = s.trim_end_matches(',');
    let nums: Vec<i32> = trimmed[3..trimmed.len() - 1].split(',').map(|x| x.parse().unwrap()).collect();
    (nums[0], nums[1], nums[2])
}
