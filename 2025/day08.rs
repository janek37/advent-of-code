use std::collections::HashMap;
use std::io;

type Point = (i32, i32, i32);

pub fn main() {
    let junctions = parse_input();
    let pairs = get_sorted_pairs(&junctions);
    let sizes = get_circuit_sizes(&junctions, &pairs[..1000]);
    println!("{}", sizes[0] * sizes[1] * sizes[2]);
    let mut disjoint_sets = DisjointSets::new(&junctions);
    println!("{}", make_connections(&pairs, &mut disjoint_sets));
}

fn get_circuit_sizes(junctions: &[Point], pairs: &[(Point, Point)]) -> Vec<usize> {
    let mut disjoint_sets = DisjointSets::new(&junctions);
    make_connections(pairs, &mut disjoint_sets);
    let mut circuit_sizes: Vec<usize> = junctions
        .iter()
        .filter(|&&j| disjoint_sets.is_root(j))
        .map(|&j| disjoint_sets.get_count(j)).collect();
    circuit_sizes.sort_by_key(|&n| -(n as isize));
    circuit_sizes
}

struct DisjointSets<T> {
    parents: HashMap<T, T>,
    counts: HashMap<T, usize>,
}

impl DisjointSets<Point> {
    fn new(points: &[Point]) -> Self {
        Self {
            parents: HashMap::new(),
            counts: points.iter().map(|&p| (p, 1)).collect(),
        }
    }

    fn find(&mut self, p: Point) -> Point {
        let line = self.get_line(p);
        let ancestor = line[line.len() - 1];
        for &descendant in line[..line.len().saturating_sub(2)].iter() {
            self.parents.insert(descendant, ancestor);
        }
        ancestor
    }

    fn get_line(&self, p: Point) -> Vec<Point> {
        let mut line = Vec::new();
        let mut ancestor = p;
        loop {
            line.push(ancestor);
            match self.parents.get(&ancestor) {
                Some(&p) => { ancestor = p; },
                None => { break; }
            }
        }
        line
    }

    fn is_root(&self, p: Point) -> bool {
        !self.parents.contains_key(&p)
    }

    fn are_connected(&mut self, p1: Point, p2: Point) -> bool {
        self.find(p1) == self.find(p2)
    }

    fn union(&mut self, p1: Point, p2: Point) {
        let ancestor1 = self.find(p1);
        let ancestor2 = self.find(p2);
        self.parents.insert(ancestor1, ancestor2);
        self.counts.insert(ancestor2, self.get_count(ancestor1) + self.get_count(ancestor2));
        assert!(!self.parents.contains_key(&ancestor2));
    }

    fn get_count(&self, p: Point) -> usize {
        *self.counts.get(&p).unwrap()
    }
}

fn make_connections(pairs: &[(Point, Point)], disjoint_sets: &mut DisjointSets<Point>) -> i32 {
    let mut last_connection = pairs[0];
    let mut connection_count = 0;
    let max_connections = disjoint_sets.counts.len() - 1;
    for &(j1, j2) in pairs {
        if !disjoint_sets.are_connected(j1, j2) {
            disjoint_sets.union(j1, j2);
            last_connection = (j1, j2);
            connection_count += 1;
            if connection_count == max_connections {
                break;
            }
        }
    }
    last_connection.0.0 * last_connection.1.0
}

fn get_sorted_pairs(junctions: &[Point]) -> Vec<(Point, Point)> {
    let mut connections: Vec<_> = junctions
        .iter()
        .enumerate()
        .flat_map(|(i, &j1)| junctions[i+1..].iter().map(move |&j2| (j1, j2)))
        .collect();
    connections.sort_by_key(|&(j1, j2)| distance(j1, j2));
    connections
}

fn distance(p1: Point, p2: Point) -> i64 {
    let d0 = (p1.0 - p2.0) as i64;
    let d1 = (p1.1 - p2.1) as i64;
    let d2 = (p1.2 - p2.2) as i64;
    d0 * d0 + d1 * d1 + d2 * d2
}

fn parse_input() -> Vec<Point> {
    io::stdin()
        .lines()
        .map(|line| parse_point(line.unwrap().trim_end()))
        .collect()
}

fn parse_point(s: &str) -> Point {
    let [x, y, z] = s.split(',')
        .map(|p| p.parse().unwrap())
        .collect::<Vec<_>>()
        .try_into()
        .unwrap();
    (x, y, z)
}
