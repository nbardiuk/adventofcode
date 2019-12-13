use crate::intcode::Computer;
use std::collections::HashMap;

pub const INPUT: &str = include_str!("../res/day11.txt");

pub fn part1(input: &str) -> usize {
    let computer = Computer::parse(input);
    let panels = HashMap::new();

    paint(computer, panels).len()
}

pub fn part2(input: &str) -> Vec<String> {
    let computer = Computer::parse(input);

    let mut panels = HashMap::new();
    panels.insert((0, 0), true);

    pprint(paint(computer, panels))
}

type Panels = HashMap<(i8, i8), bool>;

fn paint(mut computer: Computer, mut panels: Panels) -> Panels {
    let mut position = (0, 0);
    let mut orientation = (0, -1);
    let mut input = vec![];
    loop {
        input.push(panels.get(&position).cloned().unwrap_or(false) as i64);

        if let [color, direction] = computer.process(&mut input)[..] {
            panels.insert(position, color == 1);
            orientation = match direction {
                0 => (orientation.1, -orientation.0),
                _ => (-orientation.1, orientation.0),
            };
            position = (position.0 + orientation.0, position.1 + orientation.1);
        }

        if computer.halted {
            return panels;
        }
    }
}

fn pprint(panels: Panels) -> Vec<String> {
    let columns = panels.keys().map(|(x, _)| x).max().cloned().unwrap_or(0);
    let rows = panels.keys().map(|(_, y)| y).max().cloned().unwrap_or(0);

    let mut canvas = vec![];
    for y in 0..=rows {
        let mut row = String::new();
        for x in 0..columns {
            let c = match panels.get(&(x, y)) {
                Some(true) => '█',
                _ => ' ',
            };
            row.push(c);
        }
        canvas.push(row);
    }
    canvas
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
