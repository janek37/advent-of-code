use std::io;

pub fn main() {
    let diagram: Vec<String> = io::stdin().lines().map(|l| l.unwrap()).collect();
    let (letters, steps) = track_line(&diagram);
    println!("{}", letters);
    println!("{}", steps);
}

fn track_line(diagram: &[String]) -> (String, u32) {
    let mut pos = (diagram[0].find('|').unwrap(), 0);
    let mut direction = (0, 1);
    let mut collected = String::new();
    let mut steps = 0;
    loop {
        let char = get_char(diagram, pos);
        if char == ' ' { break }
        steps += 1;
        if char.is_alphabetic() {
            collected.push(char);
        }
        if char == '+' {
            for new_offset in [-1, 1] {
                let new_direction = (
                    if direction.0 == 0 { new_offset } else { 0 },
                    if direction.1 == 0 { new_offset } else { 0 },
                );
                let new_pos = go_on(pos, new_direction);
                if get_char(diagram, new_pos) != ' ' {
                    pos = new_pos;
                    direction = new_direction;
                    break;
                }
            }
        } else {
            pos = go_on(pos, direction);
        }
    }
    (collected, steps)
}

fn get_char(diagram: &[String], pos: (usize, usize)) -> char {
    diagram[pos.1].chars().nth(pos.0).unwrap()
}

fn go_on(pos: (usize, usize), direction: (isize, isize)) -> (usize, usize) {
    (
        pos.0.wrapping_add_signed(direction.0),
        pos.1.wrapping_add_signed(direction.1),
    )
}
