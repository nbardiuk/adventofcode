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

pub fn part2(input: &str) -> i64 {
    let computer = Computer::parse(input);
    let mut first_j = 0_i64;
    for i in 20.. {
        let mut beem = false;
        let mut last_j = 0;
        for j in first_j.. {
            let v = computer.clone().call(vec![j, i]);
            if !beem && v == 1 {
                first_j = j;
                beem = true;
            }
            if beem && v == 0 {
                last_j = j - 1;
                break;
            }
        }
        if last_j - first_j > 100 {
            let corner_j = last_j - 99;
            if computer.clone().call(vec![corner_j, i + 99]) == 1 {
                return corner_j * 10000 + i;
            }
        }
    }
    0
}

#[cfg(test)]
mod spec {
    use super::*;

    #[test]
    fn part1_my_input() {
        assert_eq!(part1(INPUT), 171);
    }

    #[test]
    fn part2_my_input() {
        assert_eq!(part2(INPUT), 9741242);
    }
}
