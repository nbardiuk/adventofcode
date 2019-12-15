use crate::intcode::Computer;
use std::collections::HashMap;
use std::collections::VecDeque;

pub const INPUT: &str = include_str!("../res/day15.txt");

pub fn part1(input: &str) -> usize {
    let mut computer = Computer::parse(input);
    let mut stack = VecDeque::new();
    let mut grid = HashMap::new();
    let start = (0, 0);
    grid.insert(start, 0);
    stack.push_front((start, vec![]));
    while let Some((position, bread_crumps)) = stack.pop_front() {
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
                    stack.push_back((neighbour, bread_crumps));

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
                Some(-2) => "XX",
                Some(-1) => "██",
                _ => {
                    if position == (x, y) {
                        "()"
                    } else {
                        "  "
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
}
