use std::collections::HashSet;
use std::io;

pub fn main() {
    let graph = parse_input();
    println!("{}", flood_fill(&graph, 0).len());
    println!("{}", count_groups(&graph));
}

fn flood_fill(graph: &Vec<Vec<usize>>, start: usize) -> HashSet<usize> {
    let mut visited: HashSet<usize> = HashSet::new();
    let mut stack: Vec<usize> = vec![start];
    while let Some(node) = stack.pop() {
        if visited.contains(&node) {
            continue
        }
        visited.insert(node);
        let neighbors = &graph[node];
        stack.extend(neighbors);
    }
    visited
}

fn count_groups(graph: &Vec<Vec<usize>>) -> usize {
    let mut count = 0;
    let mut current = 0;
    let mut visited: HashSet<usize> = HashSet::new();
    while current < graph.len() {
        let group = flood_fill(graph, current);
        count += 1;
        visited.extend(&group);
        while visited.contains(&current) {
            current += 1;
        }
    }
    count
}

fn parse_input() -> Vec<Vec<usize>> {
    io::stdin().lines().map(|line| parse_line(&line.unwrap())).collect()
}

fn parse_line(line: &str) -> Vec<usize> {
    let parts: Vec<_> = line.split_whitespace().collect();
    parts[2..].iter().map(|p| p.trim_end_matches(',').parse().unwrap()).collect()
}
