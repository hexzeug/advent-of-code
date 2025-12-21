fn main() {
    let input = {
        let mut name = file!();
        name = name.strip_suffix("part1.rs").unwrap_or(name);
        name = name.strip_suffix("part2.rs").unwrap_or(name);
        std::fs::read_to_string(name.to_owned() + "input.txt").unwrap()
    };
    let mut ids: Vec<u64> = input.trim().split(',')
        .flat_map(|s| {
            let (s, e) = s.split_once('-').unwrap();
            let sl = s.chars().count().try_into().unwrap();
            let el = e.chars().count().try_into().unwrap();
            (sl..=el)
                .map(move |l| {
                    (l,
                        (
                            if l == sl {s.parse().unwrap()} else {10u64.pow(l - 1)},
                            if l == el {e.parse().unwrap()} else {10u64.pow(l) - 1}
                        )
                    )
                })
        })
        .flat_map(|(l, (s, e))| {
            (1..l)
                .filter(move |k| l % k == 0)
                .map(move |k| ((l, k), (s, e)))
        })
        .flat_map(|((l, k), (s, e))| {
            let n = l / k;
            let h = 10u64.pow(k);
            let rh = h.pow(n - 1);
            ((s / rh)..h)
                .map(move |i| {
                    (0..(l / k))
                        .fold(0, |x, _| x * 10u64.pow(k) + i)
                })
                .skip_while(move |i| *i < s)
                .take_while(move |i| *i <= e)
        })
        .collect::<Vec<u64>>();
    ids.sort();
    ids.dedup();
    let solution: u64 = ids.into_iter().sum();
    println!("{solution}");
}