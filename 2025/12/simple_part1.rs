use itertools::Itertools;

const PIECE_SIZE: usize = 3;

fn main() {
    let input = {
        let mut name = file!();
        name = name.strip_suffix("simple_part1.rs").unwrap_or(name);
        std::fs::read_to_string(name.to_owned() + "input.txt").unwrap()
    };
    let blocks = input.split("\n\n").collect_vec();
    let &problem_desc = blocks.last().unwrap();
    
    let result = problem_desc.lines().filter(|desc| {
        let (size_desc, vec_desc) = desc.split_once(": ").unwrap();
        let (width_desc, height_desc) = size_desc.split_once("x").unwrap();
        let width = width_desc.parse::<usize>().unwrap();
        let height = height_desc.parse::<usize>().unwrap();
        let required = vec_desc
            .split_whitespace()
            .map(str::parse::<usize>)
            .map(Result::unwrap)
            .sum::<usize>();
        required * PIECE_SIZE * PIECE_SIZE <= width * height
    }).count();
    println!("{}", result);
}
