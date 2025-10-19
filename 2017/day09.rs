use std::io;

pub fn main() {
    let mut stream = String::new();
    io::stdin().read_line(&mut stream).unwrap();
    let mut state = State { level: 0, in_garbage: false, after_bang: false };
    let mut total_score = 0;
    let mut garbage_count = 0;
    for char in stream.chars() {
        if state.after_bang {
            state.after_bang = false;
            continue
        }
        if char == '!' {
            state.after_bang = true;
            continue
        }
        if state.in_garbage {
            if char == '>' {
                state.in_garbage = false;
            } else {
                garbage_count += 1;
            }
            continue
        }
        match char {
            '{' => {
                state.level += 1;
                total_score += state.level;
            },
            '}' => {
                state.level -= 1;
            }
            '<' => {
                state.in_garbage = true;
            }
            _ => {}
        }
    }
    println!("{}", total_score);
    println!("{}", garbage_count);
}

struct State {
    level: u32,
    in_garbage: bool,
    after_bang: bool
}
