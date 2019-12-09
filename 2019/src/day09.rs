use crate::intcode::Program;

pub const INPUT: &str = include_str!("../res/day09.txt");

pub fn part1(input: &str) -> i64 {
    Program::parse(input).call(vec![1])
}

pub fn part2(input: &str) -> i64 {
    Program::parse(input).call(vec![2])
}

#[cfg(test)]
mod spec {
    use super::*;

    #[test]
    fn part1_my_input() {
        assert_eq!(part1(INPUT), 4234906522);
    }

    #[test]
    fn part2_my_input() {
        assert_eq!(part2(INPUT), 60962);
    }
}
