use crate::intcode;
use itertools::Itertools;

pub const INPUT: &str = include_str!("../res/day07.txt");

pub fn part1(input: &str) -> i32 {
    let program = intcode::parse(input);
    (0..5)
        .permutations(5)
        .map(|settings| {
            settings.into_iter().fold(vec![0], |output, setting| {
                intcode::execute(&mut program.clone(), &[setting, output[0]])
            })[0]
        })
        .max()
        .unwrap()
}

#[cfg(test)]
mod spec {
    use super::*;

    #[test]
    fn part1_example_43210() {
        assert_eq!(
            part1("3,15,3,16,1002,16,10,16,1,16,15,15,4,15,99,0,0"),
            43210
        );
    }

    #[test]
    fn part1_example_01234() {
        assert_eq!(
            part1("3,23,3,24,1002,24,10,24,1002,23,-1,23,101,5,23,23,1,24,23,23,4,23,99,0,0"),
            54321
        );
    }

    #[test]
    fn part1_example_10432() {
        assert_eq!(
            part1("3,31,3,32,1002,32,10,32,1001,31,-2,31,1007,31,0,33,1002,33,7,33,1,33,31,31,1,32,31,31,4,31,99,0,0,0"),
            65210
        );
    }

    #[test]
    fn part1_my_input() {
        assert_eq!(part1(INPUT), 45730);
    }
}
