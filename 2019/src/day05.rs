use crate::intcode::Computer;

pub const INPUT: &str = include_str!("../res/day05.txt");

pub fn part1(input: &str) -> i64 {
    Computer::parse(input).call(vec![1])
}

pub fn part2(input: &str) -> i64 {
    Computer::parse(input).call(vec![5])
}

#[cfg(test)]
mod spec {
    use super::*;

    #[test]
    fn part1_my_input() {
        assert_eq!(part1(INPUT), 13346482);
    }

    #[test]
    fn part2_my_input() {
        assert_eq!(part2(INPUT), 12111395);
    }
}
