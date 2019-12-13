use crate::intcode::Computer;

pub const INPUT: &str = include_str!("../res/day13.txt");

pub fn part1(input: &str) -> usize {
    Computer::parse(input)
        .iteration(&mut vec![])
        .flush_output()
        .chunks(3)
        .filter(|chunk| match chunk[..] {
            [_, _, 2] => true,
            _ => false,
        })
        .count()
}

pub fn part2(input: &str) -> i64 {
    let mut computer = Computer::parse(input);
    computer.memory[0] = 2;
    let mut input = vec![];
    loop {
        let mut score = 0;
        let mut paddlex = 0;
        let mut ballx = 0;
        for chunk in computer.iteration(&mut input).flush_output().chunks(3) {
            match chunk[..] {
                [-1, 0, s] => score = s,
                [x, _, 3] => paddlex = x,
                [x, _, 4] => ballx = x,
                _ => {}
            }
        }
        if computer.has_terminated {
            return score;
        }
        input.push((ballx - paddlex).signum());
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
