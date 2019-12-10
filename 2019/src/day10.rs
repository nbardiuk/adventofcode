use std::collections::BTreeMap;
use std::collections::BTreeSet;
pub const INPUT: &str = include_str!("../res/day10.txt");

pub fn part1(input: &str) -> usize {
    let asteroids = input
        .lines()
        .enumerate()
        .flat_map(|(y, line)| {
            line.chars()
                .enumerate()
                .filter_map(|(x, ch)| if ch == '#' { Some((x, y)) } else { None })
                .collect::<Vec<_>>()
        })
        .collect::<Vec<_>>();

    asteroids
        .iter()
        .map(|&asteroid| {
            let angles = asteroids
                .iter()
                .filter(|&a| a != &asteroid)
                .map(|&a| angle(a, asteroid))
                .collect::<BTreeSet<_>>();
            angles.len()
        })
        .max()
        .unwrap()
}

pub fn part2(input: &str) -> usize {
    let asteroids = input
        .lines()
        .enumerate()
        .flat_map(|(y, line)| {
            line.chars()
                .enumerate()
                .filter_map(|(x, ch)| if ch == '#' { Some((x, y)) } else { None })
                .collect::<Vec<_>>()
        })
        .collect::<Vec<_>>();

    let position = asteroids
        .iter()
        .max_by_key(|&&asteroid| {
            let angles = asteroids
                .iter()
                .filter(|&&a| a != asteroid)
                .map(|&a| angle(a, asteroid))
                .collect::<BTreeSet<_>>();
            angles.len()
        })
        .cloned()
        .unwrap();

    let mut map = BTreeMap::new();
    for a in asteroids {
        if a != position {
            let ang = angle(a, position);
            let d = distance(a, position);
            let v = map.entry(ang).or_insert_with(Vec::new);
            if let Err(i) = &v.binary_search(&(d, a)) {
                v.insert(*i, (d, a));
            }
        }
    }

    let mut count = 0;
    while !map.is_empty() {
        let keys: Vec<_> = map.keys().cloned().collect();
        for key in keys {
            count += 1;
            if let Some(values) = map.get_mut(&key) {
                let (_, a) = values.remove(0);
                if values.is_empty() {
                    map.remove(&key);
                }
                if count == 200 {
                    return a.0 * 100 + a.1;
                }
            }
        }
    }

    0
}

fn distance((ax, ay): (usize, usize), (bx, by): (usize, usize)) -> u64 {
    (((by as f64 - ay as f64).powi(2) - (bx as f64 - ax as f64).powi(2)).powf(0.5)
        * 1_000_000_000.0) as u64
}
fn angle((ax, ay): (usize, usize), (bx, by): (usize, usize)) -> u64 {
    (((by as f64 - ay as f64).atan2(bx as f64 - ax as f64) - std::f64::consts::PI / 2.)
        * 1_000_000_000.0) as u64
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
            8
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
            33
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
            35
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
            41
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
            210
        )
    }

    #[test]
    fn part1_my_input() {
        assert_eq!(part1(INPUT), 329)
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
            802
        )
    }
    #[test]
    fn part2_my_input() {
        assert_eq!(part2(INPUT), 512)
    }
}
