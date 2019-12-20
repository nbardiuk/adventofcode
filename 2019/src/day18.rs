use std::cmp::Reverse;
use std::collections::BinaryHeap;
use std::collections::HashMap;
use std::collections::HashSet;
use std::collections::VecDeque;

pub const INPUT: &str = include_str!("../res/day18.txt");

pub fn part1(input: &str) -> usize {
    let (walls, mut items) = parse_input(input);
    let start = *items.iter().find(|(_, v)| v == &&'@').unwrap().0;

    items.insert(start, '0');
    let distances = distances_between(&items, &walls);
    let keys_count = items.values().filter(|&c| c.is_ascii_lowercase()).count();

    let mut best = HashMap::new();
    let mut queue = BinaryHeap::new();
    queue.push((Reverse(0), '0', BitSet::default()));
    while let Some((Reverse(dist_u), u, keys)) = queue.pop() {
        if keys.len() == keys_count {
            return dist_u;
        }
        for (v, (len_u_v, required)) in distances[&u].iter() {
            if !keys.contains_all(*required) {
                continue;
            }

            let key = key(*v);
            if keys.contains(key) {
                continue;
            }
            let mut keys = keys;
            keys.add(key);

            let alternative = dist_u.saturating_add(*len_u_v);
            if alternative < *best.get(&(*v, keys)).unwrap_or(&std::usize::MAX) {
                best.insert((*v, keys), alternative);
                queue.push((Reverse(alternative), *v, keys));
            }
        }
    }
    0
}
pub fn part2(input: &str) -> usize {
    let (mut walls, mut items) = parse_input(input);
    let start = *items.iter().find(|(_, v)| v == &&'@').unwrap().0;

    for (i, j) in &[(-1, 0), (1, 0), (0, 1), (0, -1), (0, 0)] {
        walls.insert((start.0 + i, start.1 + j));
    }

    items.remove(&start);
    for (robot, i, j) in &[('0', -1, -1), ('1', 1, 1), ('2', -1, 1), ('3', 1, -1)] {
        items.insert((start.0 + i, start.1 + j), *robot);
    }

    let distances = distances_between(&items, &walls);
    let keys_count = items.values().filter(|&c| c.is_ascii_lowercase()).count();

    let mut best = HashMap::new();
    let mut queue = BinaryHeap::new();
    queue.push((Reverse(0_usize), ['0', '1', '2', '3'], BitSet::default()));
    while let Some((Reverse(dist_u), us, keys)) = queue.pop() {
        if keys.len() == keys_count {
            return dist_u;
        }
        for i in 0..us.len() {
            let u = us[i];
            for (v, (len_u_v, required)) in distances[&u].iter() {
                if !keys.contains_all(*required) {
                    continue;
                }

                let key = key(*v);
                if keys.contains(key) {
                    continue;
                }
                let mut keys = keys;
                keys.add(key);

                let mut us = us;
                us[i] = *v;

                let alternative = dist_u.saturating_add(*len_u_v);
                if alternative < *best.get(&(us, keys)).unwrap_or(&std::usize::MAX) {
                    best.insert((us, keys), alternative);
                    queue.push((Reverse(alternative), us, keys));
                }
            }
        }
    }
    0
}

fn parse_input(input: &str) -> (HashSet<(i64, i64)>, HashMap<(i64, i64), char>) {
    let mut walls = HashSet::new();
    let mut items = HashMap::new();
    for (y, line) in input.lines().enumerate() {
        for (x, v) in line.chars().enumerate() {
            let position = (x as i64, y as i64);
            if "#".contains(v) {
                walls.insert(position);
            } else if !".\n".contains(v) {
                items.insert(position, v);
            }
        }
    }
    (walls, items)
}

fn distances_between(
    items: &HashMap<(i64, i64), char>,
    walls: &HashSet<(i64, i64)>,
) -> HashMap<char, HashMap<char, (usize, BitSet)>> {
    items
        .iter()
        .filter(|(_, ch)| ch.is_ascii_lowercase() || ch.is_digit(10))
        .map(|(pos, ch)| (*ch, distances_from(*pos, walls, items)))
        .collect()
}

fn distances_from(
    start: (i64, i64),
    walls: &HashSet<(i64, i64)>,
    items: &HashMap<(i64, i64), char>,
) -> HashMap<char, (usize, BitSet)> {
    let mut seen = HashSet::new();
    let mut distances = HashMap::new();
    let mut queue = VecDeque::new();
    queue.push_front((0, start, BitSet::default()));
    while let Some((distance, (x, y), required)) = queue.pop_front() {
        let distance = distance + 1;
        for (i, j) in &[(-1, 0), (1, 0), (0, -1), (0, 1)] {
            let neighbour = (x + i, y + j);

            if walls.contains(&neighbour) || seen.contains(&neighbour) {
                continue;
            }

            let mut required = required;
            if let Some(&ch) = items.get(&neighbour) {
                if !ch.is_digit(10) {
                    if ch.is_ascii_lowercase() {
                        distances.insert(ch, (distance, required));
                    }
                    required.add(key(ch));
                }
            }

            seen.insert(neighbour);
            queue.push_back((distance, neighbour, required));
        }
    }
    distances
}

#[derive(Default, Debug, PartialEq, Hash, Eq, Clone, Copy, Ord, PartialOrd)]
struct BitSet {
    bits: u32,
}
impl BitSet {
    fn contains(self, key: u8) -> bool {
        (self.bits >> key) & 1 == 1
    }

    fn contains_all(self, other: Self) -> bool {
        self.bits & other.bits == other.bits
    }

    fn add(&mut self, key: u8) {
        self.bits |= 1_u32 << key;
    }

    fn len(self) -> usize {
        self.bits.count_ones() as usize
    }
}
fn key(ch: char) -> u8 {
    ch.to_ascii_lowercase() as u8 - 'a' as u8
}

#[cfg(test)]
mod spec {
    use super::*;

    #[test]
    fn part1_8() {
        assert_eq!(
            part1(
                "#########\n\
                 #b.A.@.a#\n\
                 #########",
            ),
            8,
        );
    }

    #[test]
    fn part1_86() {
        assert_eq!(
            part1(
                "########################\n\
                 #f.D.E.e.C.b.A.@.a.B.c.#\n\
                 ######################.#\n\
                 #d.....................#\n\
                 ########################",
            ),
            86,
        );
    }

    #[test]
    fn part1_132() {
        assert_eq!(
            part1(
                "########################\n\
                 #...............b.C.D.f#\n\
                 #.######################\n\
                 #.....@.a.B.c.d.A.e.F.g#\n\
                 ########################",
            ),
            132,
        );
    }

    #[test]
    fn part2_8() {
        assert_eq!(
            part2(
                "#######\n\
                 #a.#Cd#\n\
                 ##...##\n\
                 ##.@.##\n\
                 ##...##\n\
                 #cB#Ab#\n\
                 #######",
            ),
            8,
        );
    }

    #[test]
    fn part2_24() {
        assert_eq!(
            part2(
                "###############\n\
                 #d.ABC.#.....a#\n\
                 ######...######\n\
                 ######.@.######\n\
                 ######...######\n\
                 #b.....#.....c#\n\
                 ###############",
            ),
            24,
        );
    }
    #[test]
    fn part2_32() {
        assert_eq!(
            part2(
                "#############\n\
                 #DcBa.#.GhKl#\n\
                 #.###...#I###\n\
                 #e#d#.@.#j#k#\n\
                 ###C#...###J#\n\
                 #fEbA.#.FgHi#\n\
                 #############",
            ),
            32,
        );
    }

    #[test]
    fn part2_72() {
        assert_eq!(
            part2(
                "#############\n\
                 #g#f.D#..h#l#\n\
                 #F###e#E###.#\n\
                 #dCba...BcIJ#\n\
                 #####.@.#####\n\
                 #nK.L...G...#\n\
                 #M###N#H###.#\n\
                 #o#m..#i#jk.#\n\
                 #############",
            ),
            72,
        );
    }

    #[test]
    fn part1_my_input() {
        assert_eq!(part1(INPUT), 4270);
    }

    #[test]
    fn part2_my_input() {
        assert_eq!(part2(INPUT), 1982);
    }
}
