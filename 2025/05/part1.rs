fn main() {
    let input = {
        let mut name = file!();
        name = name.strip_suffix("part1.rs").unwrap_or(name);
        name = name.strip_suffix("part2.rs").unwrap_or(name);
        std::fs::read_to_string(name.to_owned() + "input.txt").unwrap()
    };
    let (interval_input, id_input) = input.split_once("\n\n").unwrap();
    let intervals: Vec<(u64, u64)> = interval_input
        .lines()
        .map(|line| {
            let (a, b) = line.split_once('-').unwrap();
            (a.parse().unwrap(), b.parse().unwrap())
        }).collect();
    let result = id_input
        .lines()
        .map(str::parse)
        .map(Result::unwrap)
        .filter(|id: &u64| {
            intervals.iter().find(|&(a, b)| a <= id && id <= b).is_some()
        }).count();
    println!("{result}");
}