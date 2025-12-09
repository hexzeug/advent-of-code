use std::{cmp::Reverse, fmt::Debug, str::FromStr};

use geo::{LineString, Polygon, Rect, Within, coord};
use itertools::{Itertools, traits::HomogeneousTuple};

fn parse_point<T>(s: &str) -> T
where
    T: HomogeneousTuple,
    T::Item: FromStr,
    <T::Item as FromStr>::Err: Debug
{
    s
        .split(",")
        .map(str::parse)
        .map(Result::unwrap)
        .collect_tuple::<T>()
        .unwrap()
}

trait SquareRect {
    fn square_area(&self) -> i64;
}

impl SquareRect for Rect<f64> {
    fn square_area(&self) -> i64 {
        (self.width() as i64 + 1) * (self.height() as i64 + 1)
    }
}

fn main() {
    let input = {
        let mut name = file!();
        name = name.strip_suffix("part1.rs").unwrap_or(name);
        name = name.strip_suffix("part2.rs").unwrap_or(name);
        std::fs::read_to_string(name.to_owned() + "input.txt").unwrap()
    };
    let vertices = input
        .lines()
        .map(parse_point::<(f64, f64)>)
        .collect_vec();
    let poly = Polygon::new(
        LineString::from_iter(vertices.iter().copied()),
        vec![]
    );
    let result = vertices
        .iter()
        .copied()
        .tuple_combinations()
        .map(|((x1, y1), (x2, y2))| {
            Rect::new(coord!{x: x1, y: y1}, coord!{x: x2, y: y2})
        })
        .sorted_by_key(|rect| Reverse(rect.square_area()))
        .find(|rect| rect.is_within(&poly))
        .unwrap()
        .square_area();
    println!("{result}");
}