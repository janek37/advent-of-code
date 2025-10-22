use std::io;
use std::collections::HashMap;
use std::collections::HashSet;

pub fn main() {
    let nodes = parse_input();
    println!("{}", find_root(&nodes));
    let tree = Tree::from(&nodes);
    println!("{}", find_bad_weight(&tree).unwrap())
}

fn find_root(nodes: &Vec<Node>) -> String {
    let mut node_set: HashSet<&String> = HashSet::new();
    for node in nodes {
        for child_name in node.child_names.iter() {
            node_set.insert(child_name);
        }
    }
    for node in nodes {
        if !node_set.contains(&node.name) {
            return node.name.clone();
        }
    }
    "".to_string()
}

fn find_bad_weight(tree: &Tree) -> Option<u32> {
    if tree.children.len() == 0 {
        None
    } else if tree.children.len() == 1 {
        find_bad_weight(&tree.children[0])
    } else if tree.children.len() == 2 {
        if let Some(weight) = find_bad_weight(&tree.children[0]) {
            Some(weight)
        } else {
            find_bad_weight(&tree.children[1])
        }
    } else {
        let total_weights = tree.children.iter().map(|t| t.total_weight()).collect();
        let maybe_odd_index = find_odd_one_out(&total_weights);
        if let Some(odd_index) = maybe_odd_index {
            if let Some(weight) = find_bad_weight(&tree.children[odd_index]) {
                return Some(weight)
            }
            let good_index;
            if odd_index == 0 {
                good_index = 1
            } else {
                good_index = 0
            }
            Some(tree.children[odd_index].weight + total_weights[good_index] - total_weights[odd_index])
        } else {
            None
        }
    }
}

fn find_odd_one_out(vec: &Vec<u32>) -> Option<usize> {
    vec
        .iter()
        .enumerate()
        .find(|(_, x)| vec.iter().filter(|y| y == x).count() == 1)
        .map(|(i, _)| i)
}

struct Tree {
    weight: u32,
    children: Vec<Tree>
}

impl Tree {
    fn total_weight(&self) -> u32 {
        self.weight + self.children.iter().map(|subtree| subtree.total_weight()).sum::<u32>()
    }

    fn from(nodes: &Vec<Node>) -> Tree {
        let mut node_map = HashMap::new();
        for node in nodes.iter() {
            node_map.insert(&node.name, node);
        }
        let root = find_root(nodes);
        Tree::from_inner(&node_map, &root)
    }

    fn from_inner(node_map: &HashMap<&String, &Node>, root: &String) -> Tree {
        let node = node_map.get(root).unwrap();
        Tree {
            weight: node.weight,
            children: node.child_names.iter().map(|child_name| Tree::from_inner(node_map, child_name)).collect(),
        }
    }
}

struct Node {
    name: String,
    weight: u32,
    child_names: Vec<String>
}

impl Node {
    fn from(line: String) -> Node {
        let parts: Vec<_> = line.split_whitespace().collect();
        let name = parts[0].to_string();
        let weight: u32 = parts[1][1 .. parts[1].len()-1].parse().unwrap();
        let child_names: Vec<String>;
        if parts.len() <= 2 {
            child_names = Vec::new();
        }
        else {
            child_names = parts[3..]
                .iter()
                .map(|s| s.trim_end_matches(",").to_string())
                .collect();
        }
        Node {
            name,
            weight,
            child_names
        }
    }
}

fn parse_input() -> Vec<Node> {
    io::stdin().lines().map(|line| Node::from(line.unwrap())).collect()
}
