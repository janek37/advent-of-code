use std::io;
use std::collections::HashSet;

pub fn main() {
    let passphrases = parse_input();
    let valid_count = passphrases.iter().filter(is_valid).count();
    println!("{}", valid_count);
    let valid_count2 = passphrases.iter().filter(is_valid2).count();
    println!("{}", valid_count2)
}

fn parse_input() -> Vec<Vec<String>> {
    io::stdin()
        .lines()
        .map(
            |line|
            line
                .unwrap()
                .split_whitespace()
                .map(|s| s.to_string())
                .collect()
        )
        .collect()
}

fn is_valid(passphrase: &&Vec<String>) -> bool {
    let mut words = HashSet::new();
    for word in passphrase.iter() {
        if !words.insert(word) {
            return false
        }
    }
    true
}

fn is_valid2(passphrase: &&Vec<String>) -> bool {
    let mut sorted_words = HashSet::new();
    for word in passphrase.iter() {
        let mut sorted_word: Vec<char> = word.chars().collect();
        sorted_word.sort();
        if !sorted_words.insert(sorted_word) {
            return false
        }
    }
    true
}
