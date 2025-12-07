fn main() {
    let input = {
        let mut name = file!();
        name = name.strip_suffix("part1.rs").unwrap_or(name);
        name = name.strip_suffix("part2.rs").unwrap_or(name);
        std::fs::read_to_string(name.to_owned() + "input.txt").unwrap()
    };
    let mut beams = input
        .lines()
        .next()
        .unwrap()
        .chars()
        .map(|c| if c == 'S' {1} else {0})
        .collect::<Vec<_>>();
    for line in input.lines().skip(1) {
        for (i, c) in line.chars().enumerate() {
            if c == '^' {
                let b = beams[i];
                beams[i - 1] += b;
                beams[i + 1] += b;
                beams[i] = 0;
            }
        }
    }
    let result = beams.iter().sum::<u64>();
    println!("{result}");
}