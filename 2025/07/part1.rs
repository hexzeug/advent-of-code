fn main() {
    let input = {
        let mut name = file!();
        name = name.strip_suffix("part1.rs").unwrap_or(name);
        name = name.strip_suffix("part2.rs").unwrap_or(name);
        std::fs::read_to_string(name.to_owned() + "input.txt").unwrap()
    };
    let mut beams = vec![];
    let result = input
        .lines()
        .map(move |line| {
            let mut count = 0;
            if line.contains("S") {
                beams = line.chars().map(|c| c == 'S').collect();
                0
            } else {
                for (i, c) in line.chars().enumerate() {
                    if beams[i] && c == '^' {
                        beams[i - 1] = true;
                        beams[i + 1] = true;
                        beams[i] = false;
                        count += 1;
                    }
                }
                count
            }
        })
        .sum::<i32>();
    println!("{result}");
}