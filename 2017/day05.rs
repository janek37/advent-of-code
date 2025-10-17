use std::io;

fn main() {
    let orig_offsets = read_nums();
    let mut offsets = orig_offsets.clone();
    let mut pointer: i32 = 0;
    let mut count = 0;
    while 0 <= pointer && pointer < offsets.len() as i32 {
        let new_pointer = pointer + offsets[pointer as usize];
        offsets[pointer as usize] += 1;
        pointer = new_pointer;
        count += 1;
    }
    println!("{}", count);
    pointer = 0;
    count = 0;
    offsets = orig_offsets.clone();
    while 0 <= pointer && pointer < offsets.len() as i32 {
        let offset = offsets[pointer as usize];
        let offset_change;
        if offset >= 3 {
            offset_change = -1;
        } else {
            offset_change = 1;
        }
        offsets[pointer as usize] += offset_change;
        pointer = pointer + offset;
        count += 1;
    }
    println!("{}", count);
}

fn read_nums() -> Vec<i32> {
    io::stdin().lines().map(|line| line.unwrap().trim().parse().unwrap()).collect()
}
