use itertools::Itertools;

fn main() {
    let input = {
        let mut name = file!();
        name = name.strip_suffix("part1.rs").unwrap_or(name);
        name = name.strip_suffix("part2.rs").unwrap_or(name);
        std::fs::read_to_string(name.to_owned() + "input.txt").unwrap()
    };
    let result = input
        .lines()
        .map(|line| {
            line
                .split(",")
                .map(str::parse::<i64>)
                .map(Result::unwrap)
                .collect_tuple()
                .unwrap()
        })
        .tuple_combinations()
        .map(|((x1, y1), (x2, y2))| ((x2 - x1).abs() + 1) * ((y2 - y1).abs() + 1))
        .max()
        .unwrap();
    println!("{result}");
}