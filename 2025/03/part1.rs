fn main() {
    let input = {
        let mut name = file!();
        name = name.strip_suffix("part1.rs").unwrap_or(name);
        name = name.strip_suffix("part2.rs").unwrap_or(name);
        std::fs::read_to_string(name.to_owned() + "input.txt").unwrap()
    };
    let result: u32 = input
        .lines()
        .map(|line| line.chars().map(|c| c.to_digit(10).unwrap()).collect())
        .map(|bank: Vec<u32>| {
            let (i, a) = bank.iter().enumerate().rev().skip(1).max_by_key(|(_, x)| *x).unwrap();
            let b = bank[i + 1..bank.len()].iter().max().unwrap();
            a * 10 + b
        })
        .sum();
    println!("{result}");
}