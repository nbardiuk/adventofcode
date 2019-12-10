use std::collections::BTreeMap;
use std::collections::HashSet;
use std::f32::consts::FRAC_PI_2;

pub const INPUT: &str = include_str!("../res/day10.txt");

pub fn part1(input: &str) -> Option<usize> {
    let asteriods = parse_positions(input);
    asteriods
        .iter()
        .map(|&a| count_visible_from(a, &asteriods))
        .max()
}

pub fn part2(input: &str) -> Option<u16> {
    let asteroids = parse_positions(input);
    let monitoring = asteroids
        .iter()
        .max_by_key(|&&p| count_visible_from(p, &asteroids))
        .cloned()?;

    let mut mutlivalue_map = BTreeMap::new();
    for asteroid in asteroids {
        if asteroid != monitoring {
            let key = angle(asteroid, monitoring);
            let value = (distance(asteroid, monitoring), asteroid);
            let values = mutlivalue_map.entry(key).or_insert_with(Vec::new);
            if let Err(i) = &values.binary_search(&value) {
                values.insert(*i, value);
            }
        }
    }

    let mut count = 0;
    while !mutlivalue_map.is_empty() {
        for key in mutlivalue_map.keys().cloned().collect::<Vec<_>>() {
            count += 1;
            if let Some(values) = mutlivalue_map.get_mut(&key) {
                let (_, (x, y)) = values.remove(0);
                if values.is_empty() {
                    mutlivalue_map.remove(&key);
                }
                if count == 200 {
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
