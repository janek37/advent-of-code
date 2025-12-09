use std::cmp::{max, min};
use std::io;

type Tile = (u64, u64);

pub fn main() {
    let tiles = parse_input();
    println!("{}", largest_rectangle(&tiles));
    println!("{}", largest_red_green_rectangle(&tiles));
}

fn largest_rectangle(tiles: &[Tile]) -> u64 {
    tile_pairs(tiles)
        .iter()
        .map(|&(t1, t2)| area(t1, t2))
        .max()
        .unwrap()
}

// implemented after graphing the polygon - includes implicit assumptions about the shape
fn largest_red_green_rectangle(tiles: &[Tile]) -> u64 {
    let edges = get_edges(tiles);
    tile_pairs(tiles)
        .iter()
        .filter(|&&rect| edges.iter().all(|&edge| !edge_crosses_rectangle(edge, rect)))
        .map(|&(t1, t2)| area(t1, t2))
        .max()
        .unwrap()
}

fn area(t1: Tile, t2: Tile) -> u64 {
    (t1.0.abs_diff(t2.0) + 1) * (t1.1.abs_diff(t2.1) + 1)
}

fn edge_crosses_rectangle(edge: (Tile, Tile), rect: (Tile, Tile)) -> bool {
    if edge.0.0 == edge.1.0 {
        // vertical
        let x = edge.0.0;
        let y1 = edge.0.1;
        let y2 = edge.1.1;
        is_between(x, rect.0.0, rect.1.0) && is_overlap((y1, y2), (rect.0.1, rect.1.1))
    } else {
        // horizontal
        let y = edge.0.1;
        let x1 = edge.0.0;
        let x2 = edge.1.0;
        is_between(y, rect.0.1, rect.1.1) && is_overlap((x1, x2), (rect.0.0, rect.1.0))
    }
}

fn is_between(x: u64, e1: u64, e2: u64) -> bool {
    if e1 <= e2 {
        x > e1 && x < e2
    } else {
        x > e2 && x < e1
    }
}

fn is_overlap(range1: (u64, u64), range2: (u64, u64)) -> bool {
    let r1s = min(range1.0, range1.1);
    let r1e = max(range1.0, range1.1);
    let r2s = min(range2.0, range2.1);
    let r2e = max(range2.0, range2.1);
    if r1s <= r2s {
        r1e > r2s
    } else {
        r2e > r1s
    }
}

fn tile_pairs(tiles: &[Tile]) -> Vec<(Tile, Tile)> {
    tiles
        .iter()
        .enumerate()
        .flat_map(|(i, &t1)| tiles[i+1..].iter().map(move |&t2| (t1, t2)))
        .collect()
}

fn get_edges(tiles: &[Tile]) -> Vec<(Tile, Tile)> {
    tiles.iter().enumerate().map(|(i, &t)| (t, tiles[(i + 1) % tiles.len()])).collect()
}

fn parse_input() -> Vec<Tile> {
    io::stdin().lines().map(|line| parse_tile(&line.unwrap())).collect()
}

fn parse_tile(s: &str) -> Tile {
    let [x, y] = s.split(',')
        .map(|p| p.parse().unwrap())
        .collect::<Vec<_>>()
        .try_into()
        .unwrap();
    (x, y)
}
