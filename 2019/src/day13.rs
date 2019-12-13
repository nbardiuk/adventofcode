use crate::intcode::Computer;
use std::collections::HashSet;

pub const INPUT: &str = include_str!("../res/day13.txt");

pub fn part1(input: &str) -> usize {
    Computer::parse(input)
        .iteration(&mut vec![])
        .flush_output()
        .chunks(3)
        .fold(HashSet::new(), |mut blocks, chunk| {
            match chunk[..] {
                [x, y, 2] => {
                    blocks.insert((x, y));
                }
                [x, y, _] => {
                    blocks.remove(&(x, y));
                }
                _ => {}
            };
            blocks
        })
        .len()
}

pub fn part2(input: &str) -> i64 {
    let mut computer = Computer::parse(input);
    computer.memory[0] = 2;

    let mut input = vec![];
    let mut ballx = 0;
    let mut paddlex = 0;
    let mut score = 0;
    loop {
        for chunk in computer.iteration(&mut input).flush_output().chunks(3) {
            match chunk[..] {
                [-1, 0, s] => score = s,
                [x, _, 3] => paddlex = x,
                [x, _, 4] => ballx = x,
                _ => {}
            }
        }
        input.push((ballx - paddlex).signum());
        if computer.has_terminated {
            return score;
        }
    }
}

#[cfg(test)]
mod spec {
    use super::*;

    #[test]
    fn part1_my_input() {
        assert_eq!(part1(INPUT), 200)
    }

    #[test]
    fn part2_my_input() {
        assert_eq!(part2(INPUT), 9803)
    }
}
