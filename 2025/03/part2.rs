fn main() {
    let input = {
        let mut name = file!();
        name = name.strip_suffix("part1.rs").unwrap_or(name);
        name = name.strip_suffix("part2.rs").unwrap_or(name);
        std::fs::read_to_string(name.to_owned() + "input.txt").unwrap()
    };
    let result: u64 = input.lines()
        .map(|line| line.chars().map(|c| c.to_digit(10).unwrap() as u64).collect())
        .map(|bank: Vec<u64>| {
            let mut i = 0;
            let mut a = 0;
            for k in (0..12).rev() {
                let b;
                (i, b) = bank.iter().enumerate().skip(i).rev().skip(k).max_by_key(|(_, x)| *x).unwrap();
                i += 1;
                a = a * 10 + b;
            }
            a
        })
        .sum();
    println!("{result}");
}