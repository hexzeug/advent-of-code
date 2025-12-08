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
    let mut edges = vertices
        .iter()
        .enumerate()
        .tuple_combinations()
        .sorted_by_key(|&((_, &(x1, y1, z1)), (_, &(x2, y2, z2)))| {
            let dx = x1 - x2;
            let dy = y1 - y2;
            let dz = z1 - z2;
            dx * dx + dy * dy + dz * dz
        })
        .map(|((u, _), (v, _))| (u, v));
    let mut circuits = vertices.len();
    let mut union_find = UnionFind::<usize>::new(circuits);
    let (u, v) = loop {
        let (u, v) = edges.next().unwrap();
        if union_find.union(u, v) {
            circuits -= 1;
            if circuits == 1 {
                break (u, v);
            }
        }
    };
    let result = vertices[u].0 * vertices[v].0;
    println!("{result}");
}