use std::{collections::{VecDeque}, iter::zip};

use itertools::{Itertools};
use rayon::prelude::*;

fn parse_line(line: &str) -> (Vec<u8>, Vec<Vec<u8>>) {
    fn unwrap(s: &str) -> &str {
        s.get(1..s.len()-1).unwrap()
    }
    let mut iter = line.split_whitespace();
    iter.next();
    let target = unwrap(iter.next_back().unwrap())
        .split(",")
        .map(str::parse)
        .map(Result::unwrap)
        .collect_vec();
    let n = target.len();
    let buttons = iter
        .map(unwrap)
        .map(|s| {
            let mut button = vec![0; n];
            for i in s
                .split(",")
                .map(str::parse::<usize>)
                .map(Result::unwrap)
            {
                button[i] = 1;
            }
            button
        })
        .collect_vec();
    return (target, buttons);
}

fn bfs((target, edges): (Vec<u8>, Vec<Vec<u8>>)) -> u64 {
    let n = target.len();
    let m = edges.len();
    let mut found = false;
    let mut dist = 0;
    let mut queue = VecDeque::new();
    queue.push_back((0, vec![0; n], 0));
    'bfs: while let Some((ei, u, d)) = queue.pop_front() {
        // if ei + 1 == m {
        //     if zip(edges[ei].iter(), u.iter()).any(|(e, u)| *e == 0 && *u != 1) {
        //         continue;
        //     }
        //     let mut iter = u.iter().copied().filter(|u| *u == 1);
        //     let first = iter.next().unwrap();
        //     if iter.any(|u| u != first) {
        //         continue;
        //     }
        //     found = true;
        //     dist = d + (first as u64) + 1;
        //     break;
        // }
        for (ei, e) in edges.iter().enumerate().skip(ei) {
            let v = zip(u.iter(), e.iter()).map(|(u, e)| u + e).collect_vec();
            if zip(v.iter(), target.iter()).any(|(v, t)| v > t) {
                continue;
            }
            if v == target {
                found = true;
                dist = d + 1;
                break 'bfs;
            }
            queue.push_back((ei, v, d + 1));
        }
    };
    if !found { panic!("unreachable") }
    return dist;
}

fn main() {
    let input = {
        let mut name = file!();
        name = name.strip_suffix("part1.rs").unwrap_or(name);
        name = name.strip_suffix("part2.rs").unwrap_or(name);
        std::fs::read_to_string(name.to_owned() + "input.txt").unwrap()
    };
    let result = input
        .lines()    
        .par_bridge()
        .map(parse_line)
        .inspect(|x| println!("{x:?}"))
        .map(bfs)
        .inspect(|x| println!("{x:?}"))
        .sum::<u64>();
    println!("{result}");
}