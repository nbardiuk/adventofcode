pub const INPUT: &str = include_str!("../res/day03.txt");

pub fn part1(input: &str) -> Option<usize> {
    let wires = input
        .lines()
        .map(|line| {
            line.split(',')
                .filter_map(parse_command)
                .collect::<Vec<_>>()
        })
        .map(|commands| {
            let mut from = (0, 0);
            let mut result = vec![];
            for command in commands {
                let to = match (command, from) {
                    ((Direction::U, dist), (x, y)) => (x, y - dist as i32),
                    ((Direction::D, dist), (x, y)) => (x, y + dist as i32),
                    ((Direction::L, dist), (x, y)) => (x - dist as i32, y),
                    ((Direction::R, dist), (x, y)) => (x + dist as i32, y),
                };
                result.push((from, to));
                from = to;
            }
            result
        })
        .collect::<Vec<_>>();
    if let [first, second] = wires.as_slice() {
        first
            .iter()
            .flat_map(|a| {
                second.iter().filter_map(move |b| {
                    fn intersection(a1: i32, a2: i32, b1: i32, b2: i32) -> Option<i32> {
                        let (amin, amax) = (a1.min(a2), a1.max(a2));
                        let (bmin, bmax) = (b1.min(b2), b1.max(b2));
                        let (left, right) = (amin.max(bmin), amax.min(bmax));
                        if left <= right {
                            Some(left)
                        } else {
                            None
                        }
                    }
                    let ((ax1, ay1), (ax2, ay2)) = *a;
                    let ((bx1, by1), (bx2, by2)) = *b;
                    if let (Some(x), Some(y)) = (
                        intersection(ax1, ax2, bx1, bx2),
                        intersection(ay1, ay2, by1, by2),
                    ) {
                        Some(x.abs() as usize + y.abs() as usize)
                    } else {
                        None
                    }
                })
            })
            .filter(|distance| *distance != 0)
            .min()
    } else {
        None
    }
}

pub fn part2(input: &str) -> Option<usize> {
    let wires = input
        .lines()
        .map(|line| {
            line.split(',')
                .filter_map(parse_command)
                .collect::<Vec<_>>()
        })
        .map(|commands| {
            let mut from = (0, 0);
            let mut total_distance = 0;
            let mut result = vec![];
            for command in commands {
                let to = match (&command, from) {
                    (&(Direction::U, dist), (x, y)) => (x, y - dist as i32),
                    (&(Direction::D, dist), (x, y)) => (x, y + dist as i32),
                    (&(Direction::L, dist), (x, y)) => (x - dist as i32, y),
                    (&(Direction::R, dist), (x, y)) => (x + dist as i32, y),
                };
                result.push(((from, to), total_distance));
                total_distance += command.1;
                from = to;
            }
            result
        })
        .collect::<Vec<_>>();
    if let [first, second] = wires.as_slice() {
        first
            .iter()
            .flat_map(|a| {
                second.iter().filter_map(move |b| {
                    fn intersection(a1: i32, a2: i32, b1: i32, b2: i32) -> Option<i32> {
                        let (amin, amax) = (a1.min(a2), a1.max(a2));
                        let (bmin, bmax) = (b1.min(b2), b1.max(b2));
                        let (left, right) = (amin.max(bmin), amax.min(bmax));
                        if left <= right {
                            Some(left)
                        } else {
                            None
                        }
                    }
                    let (((ax1, ay1), (ax2, ay2)), adist) = *a;
                    let (((bx1, by1), (bx2, by2)), bdist) = *b;
                    if let (Some(x), Some(y)) = (
                        intersection(ax1, ax2, bx1, bx2),
                        intersection(ay1, ay2, by1, by2),
                    ) {
                        let dist =
                            (x - ax1).abs() + (x - bx1).abs() + (y - ay1).abs() + (y - by1).abs();
                        Some(adist + bdist + dist as usize)
                    } else {
                        None
                    }
                })
            })
            .filter(|distance| *distance != 0)
            .min()
    } else {
        None
    }
}

#[derive(Debug)]
enum Direction {
    L,
    R,
    U,
    D,
}

fn parse_command(txt: &str) -> Option<(Direction, usize)> {
    let result = match txt.split_at(1) {
        ("U", num) => (Direction::U, num.parse().ok()?),
        ("R", num) => (Direction::R, num.parse().ok()?),
        ("L", num) => (Direction::L, num.parse().ok()?),
        ("D", num) => (Direction::D, num.parse().ok()?),
        _ => return None,
    };
    Some(result)
}

#[cfg(test)]
mod spec {
    use super::*;

    #[test]
    fn part1_double_cross() {
        let input = "R8,U5,L5,D3\nU7,R6,D4,L4";
        assert_eq!(part1(input), Some(6));
    }

    #[test]
    fn part1_example_1() {
        let input = "R75,D30,R83,U83,L12,D49,R71,U7,L72\nU62,R66,U55,R34,D71,R55,D58,R83";
        assert_eq!(part1(input), Some(159));
    }

    #[test]
    fn part1_example_2() {
        let input =
            "R98,U47,R26,D63,R33,U87,L62,D20,R33,U53,R51\nU98,R91,D20,R16,D67,R40,U7,R15,U6,R7";
        assert_eq!(part1(input), Some(135));
    }

    #[test]
    fn part1_my_input() {
        assert_eq!(part1(INPUT), Some(209));
    }

    #[test]
    fn part2_double_cross() {
        let input = "R8,U5,L5,D3\nU7,R6,D4,L4";
        assert_eq!(part2(input), Some(30));
    }

    #[test]
    fn part2_example_1() {
        let input = "R75,D30,R83,U83,L12,D49,R71,U7,L72\nU62,R66,U55,R34,D71,R55,D58,R83";
        assert_eq!(part2(input), Some(610));
    }

    #[test]
    fn part2_example_2() {
        let input =
            "R98,U47,R26,D63,R33,U87,L62,D20,R33,U53,R51\nU98,R91,D20,R16,D67,R40,U7,R15,U6,R7";
        assert_eq!(part2(input), Some(410));
    }

    #[test]
    fn part2_my_input() {
        assert_eq!(part2(INPUT), Some(43258));
    }
}
