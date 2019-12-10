use rayon::prelude::*;
use std::collections::BTreeMap;
use std::collections::HashSet;
use std::f32::consts::FRAC_PI_2;

pub const INPUT: &str = include_str!("../res/day10.txt");

pub fn part1(input: &str) -> Option<usize> {
    let asteriods = parse_positions(input);
    asteriods
        .par_iter()
        .map(|&a| count_visible_from(a, &asteriods))
        .max()
}

pub fn part2(input: &str) -> Option<u16> {
    let asteroids = parse_positions(input);
    let monitoring = asteroids
        .par_iter()
        .max_by_key(|&&p| count_visible_from(p, &asteroids))
        .cloned()?;

    let mut visible_at_angle = BTreeMap::new();
    for asteroid in asteroids {
        if asteroid != monitoring {
            let angle = angle(asteroid, monitoring);
            let distance_asteroid = (distance(asteroid, monitoring), asteroid);
            let visible = visible_at_angle.entry(angle).or_insert_with(Vec::new);
            if let Err(i) = &visible.binary_search(&distance_asteroid) {
                visible.insert(*i, distance_asteroid);
            }
        }
    }

    let mut destroyed = 0;
    while !visible_at_angle.is_empty() {
        for angle in visible_at_angle.keys().cloned().collect::<Vec<_>>() {
            if let Some(visible) = visible_at_angle.get_mut(&angle) {
                destroyed += 1;
                let (_, (x, y)) = visible.remove(0);
                if visible.is_empty() {
                    visible_at_angle.remove(&angle);
                }
                if destroyed == 200 {
                    return Some(x * 100 + y);
                }
            }
        }
    }
    None
}

fn parse_positions(input: &str) -> Vec<(u16, u16)> {
    input
        .lines()
        .enumerate()
        .flat_map(|(y, line)| {
            line.chars()
                .enumerate()
                .filter_map(move |(x, ch)| match ch {
                    '#' => Some((x as u16, y as u16)),
                    _ => None,
                })
        })
        .collect()
}

fn count_visible_from(asteroid: (u16, u16), asteroids: &[(u16, u16)]) -> usize {
    asteroids
        .iter()
        .filter(|&&a| a != asteroid)
        .map(|&a| angle(a, asteroid))
        .collect::<HashSet<_>>()
        .len()
}

fn distance((ax, ay): (u16, u16), (bx, by): (u16, u16)) -> u16 {
    (((by as f32 - ay as f32).powi(2) - (bx as f32 - ax as f32).powi(2)).powf(0.5)) as u16
}

fn angle((ax, ay): (u16, u16), (bx, by): (u16, u16)) -> u16 {
    (((by as f32 - ay as f32).atan2(bx as f32 - ax as f32) - FRAC_PI_2) * 1000.0) as u16
}

#[cfg(test)]
mod spec {
    use super::*;

    #[test]
    fn example_3_4() {
        assert_eq!(
            part1(
                ".#..#\n\
                 .....\n\
                 #####\n\
                 ....#\n\
                 ...##"
            ),
            Some(8)
        )
    }

    #[test]
    fn example_5_8() {
        assert_eq!(
            part1(
                "......#.#.\n\
                 #..#.#....\n\
                 ..#######.\n\
                 .#.#.###..\n\
                 .#..#.....\n\
                 ..#....#.#\n\
                 #..#....#.\n\
                 .##.#..###\n\
                 ##...#..#.\n\
                 .#....####"
            ),
            Some(33)
        )
    }

    #[test]
    fn example_1_2() {
        assert_eq!(
            part1(
                "#.#...#.#.\n\
                 .###....#.\n\
                 .#....#...\n\
                 ##.#.#.#.#\n\
                 ....#.#.#.\n\
                 .##..###.#\n\
                 ..#...##..\n\
                 ..##....##\n\
                 ......#...\n\
                 .####.###."
            ),
            Some(35)
        )
    }
    #[test]
    fn example_6_3() {
        assert_eq!(
            part1(
                ".#..#..###\n\
                 ####.###.#\n\
                 ....###.#.\n\
                 ..###.##.#\n\
                 ##.##.#.#.\n\
                 ....###..#\n\
                 ..#.#..#.#\n\
                 #..#.#.###\n\
                 .##...##.#\n\
                 .....#.#.."
            ),
            Some(41)
        )
    }

    #[test]
    fn example_11_13() {
        assert_eq!(
            part1(
                ".#..##.###...#######\n\
                 ##.############..##.\n\
                 .#.######.########.#\n\
                 .###.#######.####.#.\n\
                 #####.##.#.##.###.##\n\
                 ..#####..#.#########\n\
                 ####################\n\
                 #.####....###.#.#.##\n\
                 ##.#################\n\
                 #####.##.###..####..\n\
                 ..######..##.#######\n\
                 ####.##.####...##..#\n\
                 .#####..#.######.###\n\
                 ##...#.##########...\n\
                 #.##########.#######\n\
                 .####.#.###.###.#.##\n\
                 ....##.##.###..#####\n\
                 .#.#.###########.###\n\
                 #.#.#.#####.####.###\n\
                 ###.##.####.##.#..##"
            ),
            Some(210)
        )
    }

    #[test]
    fn part1_my_input() {
        assert_eq!(part1(INPUT), Some(329))
    }

    #[test]
    fn part2_large_example() {
        assert_eq!(
            part2(
                ".#..##.###...#######\n\
                 ##.############..##.\n\
                 .#.######.########.#\n\
                 .###.#######.####.#.\n\
                 #####.##.#.##.###.##\n\
                 ..#####..#.#########\n\
                 ####################\n\
                 #.####....###.#.#.##\n\
                 ##.#################\n\
                 #####.##.###..####..\n\
                 ..######..##.#######\n\
                 ####.##.####...##..#\n\
                 .#####..#.######.###\n\
                 ##...#.##########...\n\
                 #.##########.#######\n\
                 .####.#.###.###.#.##\n\
                 ....##.##.###..#####\n\
                 .#.#.###########.###\n\
                 #.#.#.#####.####.###\n\
                 ###.##.####.##.#..##"
            ),
            Some(802)
        )
    }

    #[test]
    fn part2_my_input() {
        assert_eq!(part2(INPUT), Some(512))
    }
}
