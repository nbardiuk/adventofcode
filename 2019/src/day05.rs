use crate::intcode;

pub const INPUT: &str = include_str!("../res/day05.txt");

pub fn part1(input: &str) -> i32 {
    intcode::execute(&mut intcode::parse(input), 1)
        .last()
        .cloned()
        .unwrap()
}

pub fn part2(input: &str) -> i32 {
    intcode::execute(&mut intcode::parse(input), 5)
        .last()
        .cloned()
        .unwrap()
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
