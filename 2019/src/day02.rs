use crate::intcode;
use rayon::prelude::*;

pub const INPUT: &str = include_str!("../res/day02.txt");

pub fn part1(input: &str) -> i32 {
    run(12, 2, intcode::parse(input))
}

pub fn part2(input: &str) -> Option<i32> {
    let memory = intcode::parse(input);
    (0..10000)
        .into_par_iter()
        .find_any(|i| 1969_07_20 == run(i / 100, i % 100, memory.clone()))
}

fn run(noun: i32, verb: i32, mut memory: Vec<i32>) -> i32 {
    memory[1] = noun;
    memory[2] = verb;
    intcode::execute(&mut memory, 0);
    memory[0]
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
