use crate::intcode::Computer;
use std::collections::HashMap;

pub const INPUT: &str = include_str!("../res/day17.txt");

pub fn part1(input: &str) -> i64 {
    let grid = read_grid(&Computer::parse(input).run());

    let intersections = grid.keys().filter(|&&(x, y)| {
        grid.get(&(x - 1, y)).is_some()
            && grid.get(&(x + 1, y)).is_some()
            && grid.get(&(x, y - 1)).is_some()
            && grid.get(&(x, y + 1)).is_some()
    });

    intersections.map(|(x, y)| x * y).sum()
}

pub fn part2(input: &str) -> i64 {
    let mut computer = Computer::parse(input);
    computer.memory[0] = 2;

    let grid = read_grid(&computer.run());

    let commands = navigation_commands(grid);

    let compressed = compress(&commands);

    let mut lists = compressed.iter().map(|l| l.join(",")).collect::<Vec<_>>();
    lists.push(String::from("n\n"));
    let program = lists.join("\n");

    computer.call(program.chars().map(|c| (c as i64)).collect())
}

fn navigation_commands(grid: HashMap<(i64, i64), char>) -> Vec<String> {
    let mut commands = vec![];
    if let Some((&start, &direction)) = grid.iter().find(|(_, v)| "^v><".contains(**v)) {
        let mut queue = Some((start, to_vector(direction), 0));
        while let Some(((x, y), (i, j), forward)) = queue.take() {
            let check_move = |i, j| match (x + i, y + j) {
                position if grid.contains_key(&position) => Some((position, (i, j))),
                _ => None,
            };
            if let Some((next, direction)) = check_move(i, j) {
                queue.replace((next, direction, forward + 1));
            } else {
                if forward > 0 {
                    commands.push(format!("{}", forward));
                }
                if let Some((next, direction)) = check_move(-j, i) {
                    commands.push(String::from("R"));
                    queue.replace((next, direction, 1));
                } else if let Some((next, direction)) = check_move(j, -i) {
                    commands.push(String::from("L"));
                    queue.replace((next, direction, 1));
                }
            }
        }
    }
    commands
}

fn to_vector(direction: char) -> (i64, i64) {
    match direction {
        '^' => (0, -1),
        'v' => (0, 1),
        '>' => (1, 0),
        '<' => (-1, 0),
        _ => (0, 0),
    }
}

fn read_grid(encoded: &[i64]) -> HashMap<(i64, i64), char> {
    let mut grid = HashMap::new();
    for (y, line) in encoded.split(|&i| i == 10).enumerate() {
        for (x, value) in line.iter().enumerate() {
            if ![10, 46].contains(value) {
                grid.insert((x as i64, y as i64), (*value as u8) as char);
            }
        }
    }
    grid
}

fn compress<S: AsRef<str> + Clone + PartialEq + From<&'static str>>(input: &[S]) -> Vec<Vec<S>> {
    fn encode<S: AsRef<str> + Clone + PartialEq>(
        patterns: &[Vec<S>],
        input: &[S],
        code: &[usize],
    ) -> (Vec<usize>, Vec<S>) {
        let mut code = Vec::from(code);
        let mut input = Vec::from(input);
        let mut encoded = true;
        while encoded {
            encoded = false;
            for (i, pattern) in patterns.iter().enumerate() {
                if pattern.len() <= input.len() && pattern[..] == input[0..pattern.len()] {
                    code.push(i);
                    input = Vec::from(&input[pattern.len()..]);
                    encoded = true;
                }
            }
        }
        (code, input)
    }

    fn use_more_keys<S: AsRef<str> + Clone + PartialEq>(
        patterns: &[Vec<S>],
        input: &[S],
        code: &[usize],
    ) -> Option<(Vec<Vec<S>>, Vec<usize>)> {
        if patterns.len() == 4 {
            return None;
        }
        for len in (4..(10.min(input.len()))).step_by(2) {
            let mut patterns = Vec::from(patterns);
            patterns.push(Vec::from(&input[0..len]));

            let (code, rest) = encode(&patterns, input, &code);

            if rest.is_empty() && patterns.len() == 3 {
                return Some((patterns, code));
            }

            let recured = use_more_keys(&patterns, &rest, &code);
            if recured.is_some() {
                return recured;
            }
        }
        None
    }

    if let Some((mut dict, code)) = use_more_keys(&[], input, &[]) {
        dict.insert(
            0,
            code.iter().map(|&c| (["A", "B", "C"][c]).into()).collect(),
        );
        dict
    } else {
        vec![]
    }
}

#[cfg(test)]
mod spec {
    use super::*;

    #[test]
    fn part1_my_input() {
        assert_eq!(part1(INPUT), 3660);
    }

    #[test]
    fn part2_my_input() {
        assert_eq!(part2(INPUT), 962913);
    }

    #[test]
    fn compressing_example() {
        assert_eq!(
            compress(&[
                "R", "8", "R", "8", "R", "4", "R", "4", "R", "8", "L", "6", "L", "2", "R", "4",
                "R", "4", "R", "8", "R", "8", "R", "8", "L", "6", "L", "2"
            ]),
            [
                vec!["A", "B", "C", "B", "A", "C"],
                vec!["R", "8", "R", "8"],
                vec!["R", "4", "R", "4"],
                vec!["R", "8", "L", "6", "L", "2"]
            ]
        );
    }

    #[test]
    fn compressing() {
        assert_eq!(
            compress(&[
                "R", "6", "L", "10", "R", "10", "R", "10", "L", "10", "L", "12", "R", "10", "R",
                "6", "L", "10", "R", "10", "R", "10", "L", "10", "L", "12", "R", "10", "R", "6",
                "L", "10", "R", "10", "R", "10", "R", "6", "L", "12", "L", "10", "R", "6", "L",
                "10", "R", "10", "R", "10", "R", "6", "L", "12", "L", "10", "L", "10", "L", "12",
                "R", "10", "R", "6", "L", "12", "L", "10"
            ]),
            [
                vec!["A", "B", "A", "B", "A", "C", "A", "C", "B", "C"],
                vec!["R", "6", "L", "10", "R", "10", "R", "10"],
                vec!["L", "10", "L", "12", "R", "10"],
                vec!["R", "6", "L", "12", "L", "10"]
            ]
        );
    }
}
