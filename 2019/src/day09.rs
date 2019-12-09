use crate::intcode::Program;

pub const INPUT: &str = include_str!("../res/day09.txt");

pub fn part1(input: &str) -> i64 {
    Program::parse(input)
        .execute(vec![1])
        .output
        .last()
        .cloned()
        .unwrap()
}

#[cfg(test)]
mod spec {
    use super::*;

    #[test]
    fn part1_my_input() {
        assert_eq!(part1(INPUT), 4234906522);
    }
}
