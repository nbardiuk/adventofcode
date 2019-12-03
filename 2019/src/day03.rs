pub const INPUT: &str = include_str!("../res/day03.txt");

pub fn part1(input: &str) -> Option<usize> {
    solve(input, |a, b| {
        a.intersects(&b).map(|i| i.distance_to(&Point::new(0, 0)))
    })
}

pub fn part2(input: &str) -> Option<usize> {
    solve(input, |a, b| {
        a.intersects(&b)
            .map(|i| a.distance + a.start.distance_to(&i) + b.distance + b.start.distance_to(&i))
    })
}

fn solve(input: &str, distance: fn(&Segment, &Segment) -> Option<usize>) -> Option<usize> {
    let wires = input
        .lines()
        .map(|line| build_path(parse_commands(line)))
        .collect::<Vec<_>>();

    if let [a, b] = wires.as_slice() {
        a.iter()
            .flat_map(|ia| b.iter().filter_map(move |ib| distance(ia, ib)))
            .filter(|distance| *distance != 0)
            .min()
    } else {
        None
    }
}

#[derive(Debug, Clone)]
struct Point {
    x: i32,
    y: i32,
}
impl Point {
    fn new(x: i32, y: i32) -> Self {
        Point { x, y }
    }
    fn distance_to(&self, point: &Point) -> usize {
        ((self.x - point.x).abs() + (self.y - point.y).abs()) as usize
    }
}

#[derive(Debug)]
struct Segment {
    distance: usize,
    start: Point,
    end: Point,
}
impl Segment {
    fn new(distance: usize, start: Point, end: Point) -> Self {
        Segment {
            distance,
            start,
            end,
        }
    }

    fn intersect_at(&self, other: &Segment, projection: fn(&Point) -> i32) -> Option<i32> {
        let minmax = |a, b| if a <= b { (a, b) } else { (b, a) };
        let (a_left, a_right) = minmax(projection(&self.start), projection(&self.end));
        let (b_left, b_right) = minmax(projection(&other.start), projection(&other.end));
        let (left, right) = (a_left.max(b_left), a_right.min(b_right));
        if left <= right {
            Some(left)
        } else {
            None
        }
    }

    fn intersects(&self, other: &Segment) -> Option<Point> {
        if let (Some(x), Some(y)) = (
            self.intersect_at(other, |p| p.x),
            self.intersect_at(other, |p| p.y),
        ) {
            Some(Point::new(x, y))
        } else {
            None
        }
    }
}

#[derive(Debug)]
enum Direction {
    L,
    R,
    U,
    D,
}

fn parse_commands<'a>(line: &'a str) -> impl Iterator<Item = (Direction, usize)> + 'a {
    line.split(',').filter_map(|val| {
        let (dir, num) = val.split_at(1);
        let direction = match dir {
            "U" => Some(Direction::U),
            "R" => Some(Direction::R),
            "L" => Some(Direction::L),
            "D" => Some(Direction::D),
            _ => return None,
        };
        let distance = num.parse().ok();
        Some((direction?, distance?))
    })
}

fn build_path(commands: impl Iterator<Item = (Direction, usize)>) -> Vec<Segment> {
    let mut start = Point::new(0, 0);
    let mut total_distance = 0;
    let mut result = vec![];
    for (directon, distance) in commands {
        let end = match directon {
            Direction::U => Point::new(start.x, start.y - distance as i32),
            Direction::D => Point::new(start.x, start.y + distance as i32),
            Direction::L => Point::new(start.x - distance as i32, start.y),
            Direction::R => Point::new(start.x + distance as i32, start.y),
        };
        result.push(Segment::new(total_distance, start, end.clone()));
        start = end;
        total_distance += distance;
    }
    result
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
