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

fn angle((ax, ay): (usize, usize), (bx, by): (usize, usize)) -> u64 {
    let a = ((by as f64 - ay as f64).atan2(bx as f64 - ax as f64) * 1_000_000_000.0) as u64;
    println!("A {:?} B {:?} alpha {}", (ax, ay), (bx, by), a);
    a
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
}
