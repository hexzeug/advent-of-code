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
    let h = grid.len();
    let w = grid[0].len();
    let mut counts: Vec<Vec<u32>> = vec![vec![0; w + 2]; h + 2];
    for i in 0..h {
        for j in 0..w {
            if grid[i][j] == '@' {
                for k in i..(i + 3) {
                    for l in j..(j + 3) {
                        counts[k][l] += 1;
                    }
                }
                counts[i + 1][j + 1] -= 1;
            }
        }
    }
    let mut count = 0;
    for i in 1..=h {
        for j in 1..=w {
            if grid[i - 1][j - 1] == '@' && counts[i][j] < 4 {
                count += 1;
            }
        }
    }
    println!("{count}");
}