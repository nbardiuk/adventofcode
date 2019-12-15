use crate::intcode::Computer;
use std::collections::HashSet;
use std::collections::VecDeque;

pub const INPUT: &str = include_str!("../res/day15.txt");

pub fn part1(input: &str) -> u16 {
    let start = (0, 0);
    let mut visited = HashSet::new();
    visited.insert(start);
    let mut queue = vec![];
    queue.push((start, 0, Computer::parse(input)));

    while let Some((position, distance, computer)) = queue.pop() {
        for direction in 1..=4 {
            let neighbour = next(position, direction);
            if !visited.contains(&neighbour) {
                visited.insert(neighbour);
                let mut computer = computer.clone();
                match computer.process(&mut vec![direction as i64])[..] {
                    [2] => {
                        return distance + 1;
                    }
                    [1] => {
                        queue.push((neighbour, distance + 1, computer));
                    }
                    _ => {}
                }
            }
        }
    }
    0
}

pub fn part2(input: &str) -> u16 {
    let start = (0, 0);
    let mut visited = HashSet::new();
    visited.insert(start);
    let mut queue = vec![];
    queue.push((start, Computer::parse(input)));
    let mut walls = HashSet::new();

    let mut oxygen = start;
    while let Some((position, computer)) = queue.pop() {
        for direction in 1..=4 {
            let neighbour = next(position, direction);
            if !visited.contains(&neighbour) {
                visited.insert(neighbour);
                let mut computer = computer.clone();
                match computer.process(&mut vec![direction as i64])[..] {
                    [2] => {
                        oxygen = neighbour;
                        queue.push((neighbour, computer));
                    }
                    [1] => {
                        queue.push((neighbour, computer));
                    }
                    _ => {
                        walls.insert(neighbour);
                    }
                }
            }
        }
    }

    visited = walls;
    visited.insert(oxygen);
    let mut queue = VecDeque::new();
    queue.push_front((oxygen, 0));
    let mut result = 0;
    while let Some((position, time)) = queue.pop_front() {
        for direction in 1..=4 {
            let neighbour = next(position, direction);
            if !visited.contains(&neighbour) {
                visited.insert(neighbour);
                queue.push_back((neighbour, time + 1));
            }
        }
        result = time;
    }
    result
}

fn next((x, y): (i16, i16), direction: u8) -> (i16, i16) {
    match direction {
        1 => (x, y - 1),
        2 => (x, y + 1),
        3 => (x - 1, y),
        _ => (x + 1, y),
    }
}

#[cfg(test)]
mod spec {
    use super::*;

    #[test]
    fn part1_my_input() {
        assert_eq!(part1(INPUT), 374);
    }

    #[test]
    fn part2_my_input() {
        assert_eq!(part2(INPUT), 482);
    }
}
