use std::fmt::Display;

use itertools::Itertools;

fn debug_print_matrix<T: Display>(matrix: &Vec<Vec<T>>) {
    for row in matrix {
        for cell in row {
            print!("{} ", cell);
        }
        println!();
    }
}

fn build_constraints(pieces: &Vec<Piece>, width: usize, height: usize, target: &Vec<i8>) -> Vec<Vec<i8>> {
    assert!(pieces.len() == target.len());

    // debug
    let (pieces, target): (Vec<Piece>, Vec<i8>) = std::iter::zip(pieces, target).filter(|(_, t)| **t != 0).unzip();

    let n = pieces.len();
    let n_place_pos = (width - PIECE_SIZE + 1) * (height - PIECE_SIZE + 1);
    let n_vars = n_place_pos * pieces.iter().map(|p| p.n_perms).sum::<usize>();
    let mut constraints = vec![vec![0i8; n_vars + 1]; (width * height) + n];
    println!("n_vars: {n_vars}");

    // coefficients for usage constraints
    let mut var = 0;
    for (i, piece) in pieces.iter().enumerate() {
        let constr_i = (width * height) + i;
        for _ in 0..(n_place_pos * piece.n_perms) {
            assert!(constraints[constr_i][var] == 0);
            constraints[constr_i][var] = 1;
            var += 1;
        }
    }
    assert!(var == n_vars);

    // right hand side for usage constraints
    for (i, &target) in target.iter().enumerate() {
        let constr_i = (width * height) + i;
        constraints[constr_i][n_vars] = -target;
    }

    // coefficients for occupation constraints
    let mut var = 0;
    for piece in pieces {
        for anchor_i in 0..(height - PIECE_SIZE + 1) {
            for anchor_j in 0..(width - PIECE_SIZE + 1) {
                for perm_i in 0..piece.n_perms {
                    let perm = &piece.perms[perm_i];
                    for offset_i in 0..PIECE_SIZE {
                        for offset_j in 0..PIECE_SIZE {
                            let occupied = perm.grid[offset_i][offset_j];
                            if occupied {
                                let pos_i = anchor_i + offset_i;
                                let pos_j = anchor_j + offset_j;
                                let constr_i = pos_i * width + pos_j;
                                assert!(constraints[constr_i][var] == 0);
                                constraints[constr_i][var] = 1;
                            }
                        }
                    }
                    var += 1;
                }
            }
        }
    }
    assert!(var == n_vars);

    // right hand side for occupation constraints
    for i in 0..(width * height) {
        constraints[i][n_vars] = -1;
    }

    constraints
}

fn main() {
    let input = {
        let mut name = file!();
        name = name.strip_suffix("part1.rs").unwrap_or(name);
        name = name.strip_suffix("part2.rs").unwrap_or(name);
        std::fs::read_to_string(name.to_owned() + "input.txt").unwrap()
    };
    let blocks = input.split("\n\n").collect_vec();
    let (&problem_desc, piece_desc) = blocks.split_last().unwrap();
    
    let mut pieces = vec![];
    for &desc in piece_desc {
        let mut perm = PiecePermutation::default();
        for (i, row) in desc.lines().skip(1).enumerate() {
            for (j, c) in row.chars().enumerate() {
                perm.grid[i][j] = c == '#';
            }
        }
        let mut piece = Piece::default();
        for _ in 0..2 {
            for _ in 0..4 {
                if !piece.contains(&perm) {
                    piece.insert(perm.clone());
                }
                perm.rotate();
            }
            perm.flip();
        }
        pieces.push(piece);
    }

    let result = problem_desc.lines().map(|desc| {
        let (size_desc, vec_desc) = desc.split_once(": ").unwrap();
        let (width_desc, height_desc) = size_desc.split_once("x").unwrap();
        let vec = vec_desc
            .split_whitespace()
            .map(str::parse)
            .map(Result::unwrap)
            .collect_vec();
        let constraints = build_constraints(
            &pieces,
            width_desc.parse().unwrap(),
            height_desc.parse().unwrap(),
            &vec
        );
        debug_print_matrix(&constraints);
        0
    }).sum::<usize>();
    println!("{}", result);
}

const PIECE_SIZE: usize = 3;

#[derive(Debug, Default, Clone, Copy)]
struct Piece {
    n_perms: usize,
    perms: [PiecePermutation; 8],
}

impl Piece {
    fn contains(&self, perm: &PiecePermutation) -> bool {
        self.perms[..self.n_perms].contains(perm)
    }

    fn insert(&mut self, perm: PiecePermutation) {
        self.perms[self.n_perms] = perm;
        self.n_perms += 1;
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Default)]
struct PiecePermutation {
    grid: [[bool; PIECE_SIZE]; PIECE_SIZE],
}

impl PiecePermutation {
    fn rotate(&mut self) {
        self.flip();
        for i in 0..PIECE_SIZE {
            self.grid[i].reverse();
        };
    }

    fn flip(&mut self) {
        for i in 0..PIECE_SIZE {
            for j in i + 1..PIECE_SIZE {
                let tmp = self.grid[i][j];
                self.grid[i][j] = self.grid[j][i];
                self.grid[j][i] = tmp;
            }
        };
    }
}
