use std::array;

use itertools::Itertools;

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

    let source = name_to_vertex("svr");
    let dac = name_to_vertex("dac");
    let fft = name_to_vertex("fft");
    let target = name_to_vertex("out");
    let mut adj: [_; NAME_SIZE] = array::from_fn(|_| Vec::new());
    for line in input.lines() {
        let (name, outputs) = line.split_once(": ").unwrap();
        let v = name_to_vertex(name) as usize;
        adj[v].extend(
            outputs.split(" ").map(name_to_vertex)
        );
    }

    let mut top = Vec::new();
    let mut disc = [false; NAME_SIZE];
    let mut stack = Vec::new();
    disc[source as usize] = true;
    stack.push((false, source));
    while let Some((finish, u)) = stack.pop() {
        if finish {
            top.push(u);
        } else {
            stack.push((true, u));
            for &v in adj[u as usize].iter() {
                if !disc[v as usize] {
                    disc[v as usize] = true;
                    stack.push((false, v));
                }
            }
        }
    }
    top.reverse();

    let mut adj_t: [_; NAME_SIZE] = array::from_fn(|_| Vec::new());
    for &u in top.iter() {
        for &v in adj[u as usize].iter() {
            adj_t[v as usize].push(u);
        }
    }

    let path: [u16; 4] = top
        .iter()
        .copied()
        .filter(|&v| v == source || v == dac || v == fft || v == target)
        .collect_array()
        .unwrap();

    let result = path
        .into_iter()
        .tuple_windows()
        .map(|(src, tag)| count_paths(&adj_t, &top, src, tag))
        .fold(1, |acc, x| acc * x);

    println!("{result}");
}

fn count_paths(adj_t: &[Vec<u16>; NAME_SIZE], top: &Vec<u16>, source: u16, target: u16) -> u64 {
    let mut counts = [0; NAME_SIZE];
    counts[source as usize] = 1;
    for u in top
        .iter()
        .copied()
        .skip_while(|&u| u != source)
        .skip(1)
        .take_while_inclusive(|&u| u != target)
    {
        counts[u as usize] = adj_t[u as usize]
            .iter()
            .copied()
            .map(|v| counts[v as usize])
            .sum();
    };
    assert!(counts[target as usize] > 0);
    counts[target as usize]
}