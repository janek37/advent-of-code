use std::collections::HashMap;
use std::io;

pub fn main() {
    let rules = parse_input();
    let orig_image = Image {
        size: 3,
        content: ".#...####".to_string(),
    };
    let mut image = orig_image.clone();
    for _ in 0..5 { image = image.enhance(&rules) }
    println!("{}", image.pixel_count());
    println!("{}", count_pixels_after(6, &orig_image, &rules));
}

fn count_pixels_after(n: usize, image: &Image, rules: &HashMap<String, String>) -> usize {
    let start_key = find_key(image, rules);
    let (start, transitions) = compute_transitions(rules, &start_key);
    let mut counts = vec![0; transitions.len()];
    counts[start] = 1;
    for _ in 0..n {
        let mut new_counts = vec![0; transitions.len()];
        for (i, count) in counts.iter().enumerate() {
            for &t in transitions[i].1.iter() {
                new_counts[t] += count;
            }
        }
        counts = new_counts;
    }
    counts.iter().enumerate().map(|(i, c)| transitions[i].0 * c).sum()
}

fn compute_transitions(rules: &HashMap<String, String>, start: &str) -> (usize, Vec<(usize, Vec<usize>)>) {
    let rules3: Vec<String> = rules.keys().filter(|k| k.len() == 9).cloned().collect();
    let enhanced = rules3.iter()
        .cloned()
        .map(|content| Image { size: 3, content })
        .map(|image| (image.pixel_count(), image
            .enhance(rules).enhance(rules).enhance(rules)
            .get_blocks(3)
            .iter()
            .map(|b| find_key(b, rules))
            .map(|k| rules3.iter().enumerate().find(|&(_, s)| *s == k).unwrap().0)
            .collect::<Vec<usize>>()
        )).collect();
    (rules3.iter().enumerate().find(|&(_, s)| s == start).unwrap().0, enhanced)
}

#[derive(Clone)]
struct Image {
    size: usize,
    content: String,
}

impl Image {
    fn get_idx(&self, x: usize, y: usize) -> usize {
        x + y * self.size
    }

    fn get_pixel(&self, x: usize, y: usize) -> char {
        self.content.chars().nth(self.get_idx(x, y)).unwrap()
    }

    fn pixel_count(&self) -> usize {
        self.content.chars().filter(|ch| *ch == '#').count()
    }

    fn enhance(&self, rules: &HashMap<String, String>) -> Self {
        let rule_size = if self.size.is_multiple_of(2) { 2 } else { 3 };
        let blocks = self.get_blocks(rule_size);
        let new_blocks: Vec<Image> = blocks.iter().map(|b| apply_rules(b, rules)).collect();
        let new_size = self.size / rule_size * (rule_size + 1);
        let grid_size = self.size / rule_size;
        let new_content: String = (0..new_size)
            .map(|i| (0..new_size).map(|j|
                new_blocks[i / (rule_size + 1) * grid_size + j / (rule_size + 1)]
                    .get_pixel(j % (rule_size + 1), i % (rule_size + 1))
            ).collect::<String>()).collect();
        Self { content: new_content, size: new_size }
    }

    fn get_blocks(&self, block_size: usize) -> Vec<Self> {
        let mut blocks: Vec<Image> = Vec::new();
        let grid_size = self.size / block_size;
        for i in 0..grid_size {
            for j in 0..grid_size {
                blocks.push(self.get_block(j, i, block_size));
            }
        }
        blocks
    }

    fn get_block(&self, x0: usize, y0: usize, size: usize) -> Self {
        let content = (y0*size..(y0+1)*size)
            .map(|y| &self.content[self.get_idx(x0*size, y)..self.get_idx((x0+1)*size, y)])
            .collect();
        Self { content, size }
    }
}

fn apply_rules(block: &Image, rules: &HashMap<String, String>) -> Image {
    let key = find_key(block, rules);
    Image { content: rules.get(&key).unwrap().clone(), size: block.size + 1 }
}

fn find_key(block: &Image, rules: &HashMap<String, String>) -> String {
    rotations(block).iter().find(|&r| rules.contains_key(r)).unwrap().clone()
}

fn rotations(block: &Image) -> Vec<String> {
    let mut result: Vec<String> = vec![
        block.content.clone(),
        block.content.chars().rev().collect(),
        (0..block.size)
            .map(|i| (0..block.size)
                .map(|j| block.get_pixel(i, block.size - 1 - j))
                .collect::<String>()
            )
            .collect()
    ];
    result.push(result[2].chars().rev().collect());
    if block.size > 2 {
        result.push(
            (0..block.size)
                .map(|i| (0..block.size)
                    .map(|j| block.get_pixel(i, j))
                    .collect::<String>()
                )
                .collect()
        );
        result.push(result[4].chars().rev().collect());
        result.push(
            (0..block.size)
                .map(|i| (0..block.size)
                    .map(|j| block.get_pixel(block.size - 1 - j, i))
                    .collect::<String>()
                )
                .collect()
        );
        result.push(result[6].chars().rev().collect());
    }
    result
}

fn parse_input() -> HashMap<String, String> {
    io::stdin().lines().map(|line| parse_line(&line.unwrap())).collect()
}

fn parse_line(line: &str) -> (String, String) {
    let mut split = line.split(" => ");
    (
        split.next().unwrap().to_string().replace('/', ""),
        split.next().unwrap().to_string().replace('/', ""),
    )
}
