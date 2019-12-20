use crate::intcode::Computer;

pub const INPUT: &str = include_str!("../res/day19.txt");

pub fn part1(input: &str) -> i64 {
    let computer = Computer::parse(input);
    let mut sum = 0;
    let mut j_start = 0_i64;
    for i in 0..50 {
        let mut inside_beam = false;
        for j in j_start..50 {
            let v = computer.clone().call(vec![j, i]);
            sum += v;
            if !inside_beam && v == 1 {
                j_start = j;
                inside_beam = true;
            }
            if inside_beam && v == 0 {
                break;
            }
        }
    }
    sum
}

pub fn part2(input: &str) -> i64 {
    let computer = Computer::parse(input);
    let mut j_start = 0_i64;
    let mut j_end = 0_i64;
    for i in 100.. {
        for j in j_start.. {
            if 1 == computer.clone().call(vec![j, i]) {
                j_start = j;
                break;
            }
        }
        j_end = j_start.max(j_end);
        for j in j_end.. {
            if 0 == computer.clone().call(vec![j, i]) {
                j_end = j - 1;
                break;
            }
        }
        if j_end - j_start > 100 {
            let corner_j = j_end - 99;
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
