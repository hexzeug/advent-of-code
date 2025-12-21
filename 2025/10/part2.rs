use itertools::{Itertools};
use good_lp::{Expression, ProblemVariables, Solution, SolverModel, default_solver, variable};

fn parse_line(line: &str) -> (Vec<i32>, Vec<Vec<i32>>) {
    fn unwrap(s: &str) -> &str {
        s.get(1..s.len()-1).unwrap()
    }
    let mut iter = line.split_whitespace();
    iter.next();
    let target = unwrap(iter.next_back().unwrap())
        .split(",")
        .map(str::parse)
        .map(Result::unwrap)
        .collect_vec();
    let n = target.len();
    let buttons = iter
        .map(unwrap)
        .map(|s| {
            let mut button = vec![0; n];
            for i in s
                .split(",")
                .map(str::parse::<usize>)
                .map(Result::unwrap)
            {
                button[i] = 1;
            }
            button
        })
        .collect_vec();
    return (target, buttons);
}

fn solve(
    (target_vector,
    source_vectors):
    (Vec<i32>,
    Vec<Vec<i32>>),
) -> i32 {
    let m = source_vectors.len();
    let mut vars = ProblemVariables::new();
    let a_vars: Vec<_> = (0..m)
        .map(|i| {
            vars.add(
                variable()
                    .integer()
                    .min(0)
                    .name(format!("a_{}", i)),
            )
        })
        .collect();

    let objective_expression: Expression = a_vars.iter().sum();
    let mut problem = vars.minimise(objective_expression.clone()).using(default_solver);

    let n = target_vector.len();
    
    if source_vectors.iter().any(|v| v.len() != n) {
        panic!("Source vectors must have the same dimension as the target vector.");
    }
    
    for j in 0..n {
        let lhs: Expression = a_vars
            .iter()
            .enumerate()
            .map(|(i, &a_i)| {
                let v_ij = source_vectors[i][j] as f64;
                a_i * v_ij
            })
            .sum();
        let t_j = target_vector[j] as f64;
        
        problem = problem.with(
            lhs.eq(t_j)
        );
    }
    let solution = problem.solve().unwrap();
    solution.eval(objective_expression) as i32
}

fn main() {
    let input = {
        let mut name = file!();
        name = name.strip_suffix("part1.rs").unwrap_or(name);
        name = name.strip_suffix("part2.rs").unwrap_or(name);
        std::fs::read_to_string(name.to_owned() + "input.txt").unwrap()
    };
    let result = input
        .lines()
        .map(parse_line)
        .map(solve)
        .sum::<i32>();
    println!("{result:?}");
}