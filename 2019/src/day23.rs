use crate::intcode::Computer;
use std::ops::Range;

pub const INPUT: &str = include_str!("../res/day23.txt");

const IPS: Range<i64> = 0..50;

pub fn part1(input: &str) -> i64 {
    let nic = Computer::parse(input);
    let mut nics = IPS.map(|_| nic.clone()).collect::<Vec<_>>();
    let mut queues = IPS.map(|ip| vec![ip]).collect::<Vec<_>>();

    loop {
        for (i, nic) in nics.iter_mut().enumerate() {
            if queues[i].is_empty() {
                queues[i].push(-1);
            }
            for packet in nic.process(&mut queues[i]).chunks(3) {
                match *packet {
                    [255, _, y] => return y,
                    [ip, x, y] => queues[ip as usize].extend(&[x, y]),
                    _ => {}
                }
            }
        }
    }
}

pub fn part2(input: &str) -> i64 {
    let nic = Computer::parse(input);
    let mut nics = IPS.map(|_| nic.clone()).collect::<Vec<_>>();
    let mut queues = IPS.map(|ip| vec![ip]).collect::<Vec<_>>();

    let mut nat = vec![-1];
    let mut last_seen_y = None;
    loop {
        for (i, nic) in nics.iter_mut().enumerate() {
            if queues[i].is_empty() {
                if i == 0 && queues.iter().all(|queue| queue.is_empty()) {
                    if let [_, y] = nat[..] {
                        if last_seen_y.replace(y) == Some(y) {
                            return y;
                        }
                    }
                    queues[i].extend(&nat);
                } else {
                    queues[i].push(-1);
                }
            }
            for packet in nic.process(&mut queues[i]).chunks(3) {
                match *packet {
                    [255, x, y] => nat = vec![x, y],
                    [ip, x, y] => queues[ip as usize].extend(&[x, y]),
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
