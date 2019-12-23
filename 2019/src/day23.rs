use crate::intcode::Computer;
pub const INPUT: &str = include_str!("../res/day23.txt");

pub fn part1(input: &str) -> i64 {
    let mut computers = vec![];
    let mut queues = vec![];
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

#[cfg(test)]
mod spec {
    use super::*;

    #[test]
    fn part1_my_input() {
        assert_eq!(part1(INPUT), 23213);
    }
}
