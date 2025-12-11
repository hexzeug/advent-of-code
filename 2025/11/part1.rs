use std::{array, collections::VecDeque};

const DIGIT_SIZE: usize = (b'z' - b'a' + 1) as usize;
const NAME_SIZE: usize = DIGIT_SIZE * DIGIT_SIZE * DIGIT_SIZE;

fn name_to_vertex(name: &str) -> u16 {
        name
        .chars()
        .map(|c| (c as u8 - b'a') as usize)
        .fold(0, |acc, x| acc * DIGIT_SIZE + x) as u16
}

fn main() {
    let input = {
        let mut name = file!();
        name = name.strip_suffix("part1.rs").unwrap_or(name);
        name = name.strip_suffix("part2.rs").unwrap_or(name);
        std::fs::read_to_string(name.to_owned() + "input.txt").unwrap()
    };
    let source = name_to_vertex("you");
    let target = name_to_vertex("out");
    let mut adj: [_; NAME_SIZE] = array::from_fn(|_| Vec::new());
    for line in input.lines() {
        let (name, outputs) = line.split_once(": ").unwrap();
        let v = name_to_vertex(name) as usize;
        adj[v].extend(
            outputs.split(" ").map(name_to_vertex)
        );
    }

    let mut counter = 0;
    let mut queue = VecDeque::new();
    queue.push_back(source);
    while let Some(u) = queue.pop_front() {
        if u == target {
            counter += 1;
        } else {
            for &v in adj[u as usize].iter() {
                queue.push_back(v);
            }
        }
    }
    println!("{counter}");
}