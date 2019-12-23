use crate::intcode::Computer;
use std::collections::HashSet;

pub const INPUT: &str = include_str!("../res/day23.txt");

pub fn part1(input: &str) -> i64 {
    let mut computers = Vec::with_capacity(50);
    let mut queues = Vec::with_capacity(50);
    for i in 0..50 {
        computers.push(Computer::parse(input));
        queues.push(vec![i]);
    }

    loop {
        for i in 0..50 {
            if queues[i].is_empty() {
                queues[i].push(-1);
            }
            for packet in computers[i].process(&mut queues[i]).chunks(3) {
                match *packet {
                    [255, _, y] => return y,
                    [dest, x, y] => {
                        queues[dest as usize].push(x);
                        queues[dest as usize].push(y);
                    }
                    _ => {}
                }
            }
        }
    }
}

pub fn part2(input: &str) -> i64 {
    let mut computers = Vec::with_capacity(50);
    let mut queues = Vec::with_capacity(50);
    for i in 0..50 {
        computers.push(Computer::parse(input));
        queues.push(vec![i]);
    }

    let mut nat = vec![-1];
    let mut seen_y = HashSet::new();
    loop {
        for i in 0..50 {
            if queues[i].is_empty() {
                if i == 0 && queues.iter().all(|q| q.is_empty()) {
                    if nat.len() == 2 && !seen_y.insert(nat[1]) {
                        return nat[1];
                    }
                    queues[i].extend(&nat);
                } else {
                    queues[i].push(-1);
                }
            }
            for packet in computers[i].process(&mut queues[i]).chunks(3) {
                match *packet {
                    [255, x, y] => nat = vec![x, y],
                    [dest, x, y] => {
                        queues[dest as usize].push(x);
                        queues[dest as usize].push(y);
                    }
                    _ => {}
                }
            }
        }
    }
}

#[cfg(test)]
mod spec {
    use super::*;

    #[test]
    fn part1_my_input() {
        assert_eq!(part1(INPUT), 23213);
    }

    #[test]
    fn part2_my_input() {
        assert_eq!(part2(INPUT), 17874);
    }
}
