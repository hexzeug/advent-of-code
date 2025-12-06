use std::iter::zip;

fn main() {
    let input = {
        let mut name = file!();
        name = name.strip_suffix("part1.rs").unwrap_or(name);
        name = name.strip_suffix("part2.rs").unwrap_or(name);
        std::fs::read_to_string(name.to_owned() + "input.txt").unwrap()
    };
    let lines = input.lines().collect::<Vec<_>>();
    let (&op_line, number_lines) = lines.split_last().unwrap();
    let operators = op_line
        .split_whitespace()
        .map(|c| if c == "+" { |a, b| a + b } else if c == "*" { |a, b| a * b } else { panic!() })
        .collect::<Vec<_>>();
    let rows = number_lines.iter().map(|s| s.chars()).map(Iterator::collect::<Vec<_>>).collect::<Vec<_>>();
    let cols = (0..(rows[0].len()))
        .map(|i| {
            rows
                .iter()
                .map(|row| row[i])
                .collect::<String>()
        })
        .collect::<Vec<_>>();
    let mut blocks = vec![vec![]];
    for col in cols {
        if col.chars().all(char::is_whitespace) {
            blocks.push(vec![]);
        } else {
            blocks.last_mut().unwrap().push(col);
        }
    }
    let result = zip(operators, blocks)
        .map(|(op, block)| {
            block
                .into_iter()
                .map(|col| col.trim().parse::<u64>().unwrap())
                .reduce(op)
                .unwrap()
        })
        .sum::<u64>();
    println!("{result}");
}