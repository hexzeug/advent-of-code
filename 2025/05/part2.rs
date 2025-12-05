fn main() {
    let input = {
        let mut name = file!();
        name = name.strip_suffix("part1.rs").unwrap_or(name);
        name = name.strip_suffix("part2.rs").unwrap_or(name);
        std::fs::read_to_string(name.to_owned() + "input.txt").unwrap()
    };
    let (interval_input, _) = input.split_once("\n\n").unwrap();
    let mut intervals: Vec<(u64, u64)> = interval_input
        .lines()
        .map(|line| {
            let (a, b) = line.split_once('-').unwrap();
            (a.parse().unwrap(), b.parse().unwrap())
        }).collect();
    intervals.sort_unstable();
    intervals.dedup();
    let (mut start, mut end) = intervals[0];
    let mut result = 0;
    for (a, b) in intervals {
        if a <= end {
            end = std::cmp::max(end, b);
        } else {
            result += end - start + 1;
            start = a;
            end = b;
        }
    }
    result += end - start + 1;
    println!("{result}");
}