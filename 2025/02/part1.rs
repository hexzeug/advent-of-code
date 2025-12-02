fn main() {
    let input = {
        let mut name = file!();
        name = name.strip_suffix("part1.rs").unwrap_or(name);
        name = name.strip_suffix("part2.rs").unwrap_or(name);
        std::fs::read_to_string(name.to_owned() + "input.txt").unwrap()
    };
    let solution: u64 = input.trim().split(',')
        .flat_map(|s| {
            let (s, e) = s.split_once('-').unwrap();
            let sl = s.chars().count().try_into().unwrap();
            let el = e.chars().count().try_into().unwrap();
            (sl..=el)
                .filter(|l| l % 2 == 0)
                .map(move |l| {
                    (10u64.pow(l / 2),
                        (
                            if l == sl {s.parse().unwrap()} else {10u64.pow(l - 1)},
                            if l == el {e.parse().unwrap()} else {10u64.pow(l) - 1}
                        )
                    )
                })
        })
        .flat_map(|(h, (s, e))| {
            ((s / h)..=(e / h))
                .map(move |i| (h + 1) * i)
                .filter(move |i| s <= *i && *i <= e)
        })
        .sum();
    println!("{solution}");
}