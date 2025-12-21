use itertools::Itertools;
use petgraph::unionfind::UnionFind;

fn main() {
    let input = {
        let mut name = file!();
        name = name.strip_suffix("part1.rs").unwrap_or(name);
        name = name.strip_suffix("part2.rs").unwrap_or(name);
        std::fs::read_to_string(name.to_owned() + "input.txt").unwrap()
    };
    let vertices = input
        .lines()
        .map(|line| {
            line
                .split(",")
                .map(str::parse)
                .map(Result::unwrap)
                .collect_tuple::<(i64, i64, i64)>()
                .unwrap()
        })
        .collect_vec();
    let edges = vertices.iter()
        .enumerate()
        .tuple_combinations()
        .k_smallest_by_key(1000, |&((_, &(x1, y1, z1)), (_, &(x2, y2, z2)))| {
            let dx = x1 - x2;
            let dy = y1 - y2;
            let dz = z1 - z2;
            dx * dx + dy * dy + dz * dz
        })
        .map(|((u, _), (v, _))| (u, v));
    let mut union_find = UnionFind::<usize>::new(vertices.len());
    for (v, u) in edges {
        union_find.union(v, u);
    }
    let result = union_find
        .into_labeling()
        .iter()
        .counts()
        .values()
        .k_largest(3)
        .fold(1, |acc, &x| acc * x);
    println!("{result}");
}