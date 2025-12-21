use priority_queue::PriorityQueue;
use std::cmp::Reverse;

fn main() {
    let input = {
        let mut name = file!();
        name = name.strip_suffix("part1.rs").unwrap_or(name);
        name = name.strip_suffix("part2.rs").unwrap_or(name);
        std::fs::read_to_string(name.to_owned() + "input.txt").unwrap()
    };
    let grid: Vec<Vec<char>> = input.lines().map(|line| {
        line.trim().chars().collect::<Vec<char>>()
    }).collect();
    let h= grid.len();
    let w= grid[0].len();
    let mut degrees: Vec<Vec<i32>> = vec![vec![0; w + 2]; h + 2];
    for i in 0..h {
        for j in 0..w {
            if grid[i][j] == '@' {
                for k in i..(i + 3) {
                    for l in j..(j + 3) {
                        degrees[k][l] += 1;
                    }
                }
                degrees[i + 1][j + 1] -= 1;
            }
        }
    }
    let mut queue = PriorityQueue::new();
    for i in 0..h {
        for j in 0..w {
            if grid[i][j] == '@' {
                queue.push((i, j), Reverse(degrees[i + 1][j + 1]));
            }
        }
    }
    let mut count = 0;
    while !queue.is_empty() {
        let ((i, j), Reverse(deg)) = queue.pop().unwrap();
        if deg >= 4 { break; }
        count += 1;
        let r = if i > 0 {i - 1} else {0};
        let s = if j > 0 {j - 1} else {0};
        for k in r..=(i + 1) {
            for l in s..=(j + 1) {
                if let Some(Reverse(d)) = queue.get_priority(&(k, l)) {
                    queue.change_priority(&(k, l), Reverse(d - 1));
                }
            }
        }
    }
    println!("{count}");
}