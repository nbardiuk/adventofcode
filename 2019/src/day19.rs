use crate::intcode::Computer;

pub const INPUT: &str = include_str!("../res/day19.txt");

pub fn part1(input: &str) -> i64 {
    let computer = Computer::parse(input);
    let mut sum = 0;
    for i in 0..50 {
        for j in 0..50 {
            sum += computer.clone().call(vec![i, j])
        }
    }
    sum
}

#[cfg(test)]
mod spec {
    use super::*;

    #[test]
    fn part1_my_input() {
        assert_eq!(part1(INPUT), 171);
    }
}
