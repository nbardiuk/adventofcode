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

    // TODO compress commands into program
    assert_eq!(commands.join(","), "R,6,L,10,R,10,R,10,L,10,L,12,R,10,R,6,L,10,R,10,R,10,L,10,L,12,R,10,R,6,L,10,R,10,R,10,R,6,L,12,L,10,R,6,L,10,R,10,R,10,R,6,L,12,L,10,L,10,L,12,R,10,R,6,L,12,L,10");
    let program = "A,B,A,B,A,C,A,C,B,C\n\
                   R,6,L,10,R,10,R,10\n\
                   L,10,L,12,R,10\n\
                   R,6,L,12,L,10\n\
                   n\n";

    computer.call(program.chars().map(|c| (c as i64)).collect())
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
}
