use std::io;

pub fn main() {
    let components = parse_input();
    println!("{}", make_bridges(&components).iter().map(|(_, s)| s).max().unwrap());
    println!("{}", make_bridges(&components).iter().max().unwrap().1);
}

fn make_bridges(compoments: &[(u32, u32)]) -> Vec<(u32, u32)> {
    make_bridges_from(0, compoments, &vec![true; compoments.len()])
}

fn make_bridges_from(start: u32, components: &[(u32, u32)], available: &[bool]) -> Vec<(u32, u32)> {
    let mut bridges: Vec<(u32, u32)> = vec![];
    for (i, (&component, &is_available)) in components.iter().zip(available.iter()).enumerate() {
        if !is_available { continue; }
        let new_start;
        if component.0 == start {
            new_start = component.1;
        } else if component.1 == start {
            new_start = component.0
        } else {
            continue;
        }
        let mut new_available = available.to_owned();
        new_available[i] = false;
        bridges.extend(
            make_bridges_from(new_start, components, &new_available)
                .iter().map(|(l, s)| (l + 1, s + component.0 + component.1))
        );
    }
    if bridges.is_empty() {
        bridges.push((0, 0))
    }
    bridges
}

fn parse_input() -> Vec<(u32, u32)> {
    io::stdin()
        .lines()
        .map(|line| line.unwrap().trim_end().split('/').map(|s| s.to_string()).collect::<Vec<_>>())
        .map(|split| (split[0].parse().unwrap(), split[1].parse().unwrap()))
        .collect()
}
