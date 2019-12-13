use crate::intcode::Computer;
use std::collections::HashSet;

pub const INPUT: &str = include_str!("../res/day13.txt");

pub fn part1(input: &str) -> usize {
    let mut computer = Computer::parse(input);
    let output = computer.iteration(&mut vec![]).flush_output();
    output
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

#[cfg(test)]
mod spec {
    use super::*;
    #[test]
    fn part1_my_input() {
        assert_eq!(part1(INPUT), 200)
    }
}
