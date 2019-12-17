use crate::intcode::Computer;
use std::collections::HashMap;
use std::collections::HashSet;

pub const INPUT: &str = include_str!("../res/day17.txt");

pub fn part1(input: &str) -> usize {
    let output = Computer::parse(input).run();

    let mut grid = HashSet::new();
    for (y, line) in output.split(|&i| i == 10).enumerate() {
        for (x, value) in line.iter().enumerate() {
            if ![10, 46].contains(value) {
                grid.insert((x, y));
            }
        }
    }

    let intersections = grid.iter().filter(|&&(x, y)| {
        grid.get(&(x.saturating_sub(1), y)).is_some()
            && grid.get(&(x + 1, y)).is_some()
            && grid.get(&(x, y.saturating_sub(1))).is_some()
            && grid.get(&(x, y + 1)).is_some()
    });

    intersections.map(|(x, y)| x * y).sum()
}

pub fn part2(input: &str) -> i64 {
    let mut computer = Computer::parse(input);
    computer.memory[0] = 2;

    let mut grid = HashMap::new();
    for (y, line) in computer.run().split(|&i| i == 10).enumerate() {
        for (x, value) in line.iter().enumerate() {
            if ![10, 46].contains(value) {
                grid.insert((x, y), (*value as u8) as char);
            }
        }
    }

    let mut commands = vec![];
    if let Some(((x, y), v)) = grid.iter().find(|(_, v)| "^v><".contains(**v)) {
        let direction = match v {
            '^' => (0, -1),
            'v' => (0, 1),
            '>' => (1, 0),
            '<' => (-1, 0),
            _ => (0, 0),
        };
        let mut seen = HashSet::new();
        let mut queue = vec![((*x, *y), direction)];
        let mut distance = 0;
        while let Some(((x, y), (i, j))) = queue.pop() {
            seen.insert((x, y));
            let next = |i: i64, j: i64| match (x as i64 + i, y as i64 + j) {
                (x, y) if x >= 0 && y >= 0 => {
                    let forward = (x as usize, y as usize);
                    if grid.contains_key(&forward) {
                        Some(forward)
                    } else {
                        None
                    }
                }
                _ => None,
            };
            if let Some(forward) = next(i, j) {
                distance += 1;
                queue.push((forward, (i, j)));
            } else if let Some(right) = next(-j, i) {
                if distance > 0 {
                    commands.push(format!("{}", distance));
                }
                commands.push(String::from("R"));
                distance = 1;
                queue.push((right, (-j, i)));
            } else if let Some(left) = next(j, -i) {
                if distance > 0 {
                    commands.push(format!("{}", distance));
                }
                commands.push(String::from("L"));
                distance = 1;
                queue.push((left, (j, -i)));
            }
        }
        if distance > 0 {
            commands.push(format!("{}", distance));
        }
    };

    assert_eq!(commands.join(","), "R,6,L,10,R,10,R,10,L,10,L,12,R,10,R,6,L,10,R,10,R,10,L,10,L,12,R,10,R,6,L,10,R,10,R,10,R,6,L,12,L,10,R,6,L,10,R,10,R,10,R,6,L,12,L,10,L,10,L,12,R,10,R,6,L,12,L,10");
    // TODO compress commands into code
    let program = "A,B,A,B,A,C,A,C,B,C\n\
                   R,6,L,10,R,10,R,10\n\
                   L,10,L,12,R,10\n\
                   R,6,L,12,L,10\n\
                   n\n";
    let mut commands = program.chars().map(|c| (c as i64)).collect();
    let output = computer.process(&mut commands);
    *output.last().unwrap()
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
