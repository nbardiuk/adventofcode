use crate::intcode::Computer;
use std::collections::HashMap;

pub const INPUT: &str = include_str!("../res/day11.txt");

pub fn part1(input: &str) -> usize {
    let mut computer = Computer::parse(input);

    let mut position: (i16, i16) = (0, 0);
    let mut orientation: (i16, i16) = (0, -1);
    let mut panels = HashMap::new();
    loop {
        computer.iteration(&mut vec![panels.get(&position).cloned().unwrap_or(0)]);
        let output = computer.flush_output();

        if let [color, direction] = output[..] {
            orientation = match direction {
                0 => (orientation.1, -orientation.0),
                _ => (-orientation.1, orientation.0),
            };
            panels.insert(position, color);
            position = (position.0 + orientation.0, position.1 + orientation.1);
        }

        if computer.has_terminated {
            return panels.len();
        }
    }
}

#[cfg(test)]
mod spec {
    use super::*;

    #[test]
    fn part1_my_input() {
        assert_eq!(part1(INPUT), 2021);
    }
}
