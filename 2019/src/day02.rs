use crate::intcode::Program;
use rayon::prelude::*;

pub const INPUT: &str = include_str!("../res/day02.txt");

pub fn part1(input: &str) -> i32 {
    run(12, 2, Program::parse(input))
}

pub fn part2(input: &str) -> Option<i32> {
    let program = Program::parse(input);
    (0..10000)
        .into_par_iter()
        .find_any(|i| 1969_07_20 == run(i / 100, i % 100, program.clone()))
}

fn run(noun: i32, verb: i32, mut program: Program) -> i32 {
    program.memory[1] = noun;
    program.memory[2] = verb;
    program.execute(vec![]).memory[0]
}

#[cfg(test)]
mod spec {
    use super::*;

    #[test]
    fn part1_my_input() {
        assert_eq!(part1(INPUT), 4570637);
    }

    #[test]
    fn part2_my_input() {
        assert_eq!(part2(INPUT), Some(5485));
    }
}
