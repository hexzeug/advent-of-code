use std::collections::VecDeque;

use itertools::Itertools;

fn parse_line(line: &str) -> (usize, u16, Vec<u16>) {
    fn unwrap(s: &str) -> &str {
        s.get(1..s.len()-1).unwrap()
    }
    let mut iter = line.split_whitespace();
    iter.next_back();
    let first = unwrap(iter.next().unwrap());
    let n = first.len();
    let target = first
        .chars()
        .map(|c| if c == '#' { 1 } else { 0 })
        .rfold(0, |acc, x| acc << 1 | x);
    let buttons = iter
        .map(unwrap)
        .map(|s| {
            s
                .split(",")
                .map(str::parse::<u16>)
                .map(Result::unwrap)
                .map(|p| 1 << p)
                .fold(0, |acc, x| acc | x)
        })
        .collect_vec();
    return (n, target, buttons);
}

fn bfs((n, target, edges): (usize, u16, Vec<u16>)) -> u16 {
    let mut vis = vec![false; 1 << n];
    let mut dist = vec![0; 1 << n];
    let mut queue = VecDeque::new();
    queue.push_back(0);
    vis[0] = true;
    'bfs: while let Some(u) = queue.pop_front() {
        let d = dist[u as usize];
        for e in edges.iter() {
            let v = u ^ e;
            if !vis[v as usize] {
                vis[v as usize] = true;
                dist[v as usize] = d + 1;
                if v == target { break 'bfs; }
                queue.push_back(v);
            }
        }
    };
    if !vis[target as usize] { panic!("unreachable") }
    return dist[target as usize];
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
        .map(parse_line)
        .map(bfs)
        .sum::<u16>();
    println!("{result}");
}