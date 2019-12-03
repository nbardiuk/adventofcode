pub const INPUT: &str = include_str!("../res/day03.txt");

pub fn part1(input: &str) -> Option<i32> {
    solve(input, |cross, _, _| Point::new(0, 0).distance_to(cross))
}

pub fn part2(input: &str) -> Option<i32> {
    solve(input, |cross, a, b| {
        a.distance + a.start.distance_to(cross) + b.distance + b.start.distance_to(cross)
    })
}

fn solve(input: &str, distance: fn(&Point, &Segment, &Segment) -> i32) -> Option<i32> {
    let wires = input
        .lines()
        .map(|line| build_path(parse_directoins(line)))
        .collect::<Vec<_>>();

    if let [left, right] = wires.as_slice() {
        let mut distances = vec![];
        for left_segment in left {
            for right_segment in right {
                if let Some(cross) = left_segment.cross(&right_segment) {
                    distances.push(distance(&cross, left_segment, right_segment));
                }
            }
        }
        distances.into_iter().filter(|d| *d != 0).min()
    } else {
        None
    }
}

#[derive(Clone)]
struct Point {
    x: i32,
    y: i32,
}
impl Point {
    fn new(x: i32, y: i32) -> Self {
        Point { x, y }
    }
    fn distance_to(&self, point: &Point) -> i32 {
        (self.x - point.x).abs() + (self.y - point.y).abs()
    }
}

struct Segment {
    distance: i32,
    start: Point,
    end: Point,
}
impl Segment {
    fn new(distance: i32, start: Point, end: Point) -> Self {
        Segment {
            distance,
            start,
            end,
        }
    }

    fn cross_projection(&self, other: &Segment, projection: fn(&Point) -> i32) -> Option<i32> {
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

    fn cross(&self, other: &Segment) -> Option<Point> {
        if let (Some(x), Some(y)) = (
            self.cross_projection(other, |p| p.x),
            self.cross_projection(other, |p| p.y),
        ) {
            Some(Point::new(x, y))
        } else {
            None
        }
    }
}

enum Direction {
    L(i32),
    R(i32),
    U(i32),
    D(i32),
}

fn parse_directoins<'a>(line: &'a str) -> impl Iterator<Item = Direction> + 'a {
    line.split(',').filter_map(|val| {
        let (dir, num) = val.split_at(1);
        let distance = num.parse().ok()?;
        match dir {
            "U" => Some(Direction::U(distance)),
            "R" => Some(Direction::R(distance)),
            "L" => Some(Direction::L(distance)),
            "D" => Some(Direction::D(distance)),
            _ => None,
        }
    })
}

fn build_path(directoins: impl Iterator<Item = Direction>) -> Vec<Segment> {
    let mut start = Point::new(0, 0);
    let mut total_distance = 0;
    let mut result = vec![];
    for directon in directoins {
        let (end, distance) = match directon {
            Direction::U(dist) => (Point::new(start.x, start.y - dist), dist),
            Direction::D(dist) => (Point::new(start.x, start.y + dist), dist),
            Direction::L(dist) => (Point::new(start.x - dist, start.y), dist),
            Direction::R(dist) => (Point::new(start.x + dist, start.y), dist),
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
