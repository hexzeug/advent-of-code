fn main() {
    let input = {
        let mut name = file!();
        name = name.strip_suffix("part1.rs").unwrap_or(name);
        name = name.strip_suffix("part2.rs").unwrap_or(name);
        std::fs::read_to_string(name.to_owned() + "input.txt").unwrap()
    };
    let numbers = input
        .lines()
        .rev()
        .skip(1)
        .map(str::split_whitespace)
        .map(|x| x.map(|x| x.parse::<u64>().unwrap()))
        .map(|x| x.collect::<Vec<_>>())
        .collect::<Vec<_>>();
    let result = input
        .lines()
        .rev()
        .next()
        .unwrap()
        .split_whitespace()
        .map(|c| {
            if c == "+" {
                |a, b| a + b
            } else if c == "*" {
                |a, b| a * b
            } else { panic!() }
        })
        .enumerate()
        .map(|(i, op)| {
            numbers
                .iter()
                .map(|v| v[i])
                .reduce(op)
                .unwrap()
        })
        .sum::<u64>();
    println!("{result}");
}