use crate::intcode::Computer;
use rayon::prelude::*;

pub const INPUT: &str = include_str!("../res/day02.txt");

pub fn part1(input: &str) -> i64 {
    call(Computer::parse(input), 12, 2)
}

pub fn part2(input: &str) -> Option<i64> {
    let computer = Computer::parse(input);
    (0..10000_i64)
        .into_par_iter()
        .find_any(|i| 1969_07_20 == call(computer.clone(), i / 100, i % 100))
}

fn call(mut computer: Computer, noun: i64, verb: i64) -> i64 {
    computer.memory[1] = noun;
    computer.memory[2] = verb;
    computer.run();
    computer.memory[0]
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
