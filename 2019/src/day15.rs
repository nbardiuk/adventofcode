use crate::intcode::Computer;
use std::collections::HashMap;
use std::collections::VecDeque;

pub const INPUT: &str = include_str!("../res/day15.txt");

pub fn part1(input: &str) -> usize {
    let mut computer = Computer::parse(input);
    let mut queue = VecDeque::new();
    let mut grid = HashMap::new();
    let start = (0, 0);
    grid.insert(start, 0);
    queue.push_front((start, vec![]));
    while let Some((position, bread_crumps)) = queue.pop_front() {
        for d in &bread_crumps {
            computer.process(&mut vec![*d]);
        }

        let direcntions = (1..=4)
            .filter(|&d| !grid.contains_key(&next(position, d)))
            .collect::<Vec<_>>();
        for direction in direcntions {
            match computer.process(&mut vec![direction])[..] {
                [0] => {
                    grid.insert(next(position, direction), -1);
                }
                [2] => {
                    grid.insert(next(position, direction), -2);
                    return bread_crumps.len() + 1;
                }
                _ => {
                    let neighbour = next(position, direction);
                    grid.insert(neighbour, 0);

                    let mut bread_crumps = bread_crumps.clone();
                    bread_crumps.push(direction);
                    queue.push_back((neighbour, bread_crumps));

                    computer.process(&mut vec![opposite(direction)]);
                }
            }
        }

        for d in bread_crumps.iter().rev() {
            computer.process(&mut vec![opposite(*d)]);
        }

        // println!("\x1b[2J\x1b[H");
        // pprint(position, &grid);
        // std::thread::sleep(std::time::Duration::from_millis(100));
    }
    0
}

pub fn part2(input: &str) -> i32 {
    let mut computer = Computer::parse(input);
    let mut grid = HashMap::new();
    let start = (0, 0);
    grid.insert(start, -10);
    let mut queue = VecDeque::new();
    queue.push_front((start, vec![]));
    while let Some((position, bread_crumps)) = queue.pop_front() {
        for d in &bread_crumps {
            computer.process(&mut vec![*d]);
        }

        let direcntions = (1..=4)
            .filter(|&d| !grid.contains_key(&next(position, d)))
            .collect::<Vec<_>>();
        for direction in direcntions {
            let neighbour = next(position, direction);
            match computer.process(&mut vec![direction])[..] {
                [0] => {
                    grid.insert(neighbour, -1);
                }
                [2] => {
                    grid.insert(neighbour, -2);
                    computer.process(&mut vec![opposite(direction)]);
                }
                _ => {
                    grid.insert(neighbour, -10);

                    let mut bread_crumps = bread_crumps.clone();
                    bread_crumps.push(direction);
                    queue.push_back((neighbour, bread_crumps));

                    computer.process(&mut vec![opposite(direction)]);
                }
            }
        }

        for d in bread_crumps.iter().rev() {
            computer.process(&mut vec![opposite(*d)]);
        }
    }
    // pprint((0, 0), &grid);
    if let Some((&start, _)) = grid.iter().find(|(_, v)| **v == -2) {
        let mut queue = VecDeque::new();
        queue.push_back(start);
        grid.insert(start, 0);
        while let Some(position) = queue.pop_front() {
            let len = grid.get(&position).cloned().unwrap_or_default();
            for direction in 1..=4 {
                let neighbour = next(position, direction);
                if let Some(-10) = grid.get(&neighbour) {
                    grid.insert(neighbour, len + 1);
                    queue.push_back(neighbour);
                }
            }
        }
    }
    // println!("\x1b[2J\x1b[H");
    // pprint((0, 0), &grid);
    grid.values().max().cloned().unwrap_or_default()
}

fn opposite(direction: i64) -> i64 {
    match direction {
        1 => 2,
        2 => 1,
        3 => 4,
        _ => 3,
    }
}

fn next((x, y): (i32, i32), direction: i64) -> (i32, i32) {
    match direction {
        1 => (x, y - 1),
        2 => (x, y + 1),
        3 => (x - 1, y),
        _ => (x + 1, y),
    }
}

fn pprint(position: (i32, i32), grid: &HashMap<(i32, i32), i32>) {
    let x_min = grid.keys().map(|(x, _)| x).min().cloned().unwrap_or(0);
    let y_min = grid.keys().map(|(_, y)| y).min().cloned().unwrap_or(0);
    let x_max = grid.keys().map(|(x, _)| x).max().cloned().unwrap_or(0);
    let y_max = grid.keys().map(|(_, y)| y).max().cloned().unwrap_or(0);

    for y in y_min..=y_max {
        for x in x_min..=x_max {
            let c = match grid.get(&(x, y)) {
                Some(-2) => ">++<".to_string(),
                Some(-1) => "████".to_string(),
                Some(-10) => "    ".to_string(),
                Some(&v) if v >= 0 => format!("{:4}", v),
                _ => {
                    if position == (x, y) {
                        "(())".to_string()
                    } else {
                        "    ".to_string()
                    }
                }
            };
            print!("{}", c);
        }
        println!();
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
