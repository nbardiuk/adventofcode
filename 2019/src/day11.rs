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
pub fn part2(input: &str) -> Vec<String> {
    let mut computer = Computer::parse(input);

    let mut position: (i16, i16) = (0, 0);
    let mut orientation: (i16, i16) = (0, -1);
    let mut panels = HashMap::new();
    panels.insert(position, 1);
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
            let columns = panels.keys().map(|(x, _)| x).max().cloned().unwrap_or(0);
            let rows = panels.keys().map(|(_, y)| y).max().cloned().unwrap_or(0);
            let mut result = vec![];
            for y in 0..=rows {
                let mut row = String::new();
                for x in 0..columns {
                    let c = match panels.get(&(x, y)) {
                        Some(1) => '█',
                        _ => ' ',
                    };
                    row.push(c);
                }
                result.push(row);
            }
            return result;
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
    #[test]
    fn part2_my_input() {
        assert_eq!(
            part2(INPUT),
            [
                " █    ███    ██ █  █ ████ █  █ █    █  █  ",
                " █    █  █    █ █  █ █    █ █  █    █  █  ",
                " █    ███     █ ████ ███  ██   █    ████  ",
                " █    █  █    █ █  █ █    █ █  █    █  █  ",
                " █    █  █ █  █ █  █ █    █ █  █    █  █  ",
                " ████ ███   ██  █  █ ████ █  █ ████ █  █  "
            ]
        );
    }
}
