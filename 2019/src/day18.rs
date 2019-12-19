use std::cmp::Reverse;
use std::collections::BinaryHeap;
use std::collections::HashMap;
use std::collections::HashSet;
use std::collections::VecDeque;

pub const INPUT: &str = include_str!("../res/day18.txt");

fn distance(
    start: (i64, i64),
    finish: (i64, i64),
    walls: &HashSet<(i64, i64)>,
    items: &HashMap<(i64, i64), char>,
) -> Option<(usize, BitSet)> {
    let mut seen = HashSet::new();
    let mut queue = VecDeque::new();
    queue.push_front((0, start, BitSet::default()));
    while let Some((distance, (x, y), between)) = queue.pop_front() {
        let distance = distance + 1;
        for (i, j) in &[(-1, 0), (1, 0), (0, -1), (0, 1)] {
            let neighbour = (x + i, y + j);

            if neighbour == finish {
                return Some((distance, between));
            }

            if walls.contains(&neighbour) || seen.contains(&neighbour) {
                continue;
            }

            let mut between = between;
            if let Some(&ch) = items.get(&neighbour) {
                if ch != '@' {
                    between.add(ch.to_ascii_lowercase() as u8 - 'a' as u8);
                }
            }

            seen.insert(neighbour);
            queue.push_back((distance, neighbour, between));
        }
    }
    None
}

pub fn part1(input: &str) -> usize {
    let mut walls = HashSet::new();
    let mut start = (0, 0);
    let mut items = HashMap::new();
    for (y, line) in input.lines().enumerate() {
        for (x, v) in line.chars().enumerate() {
            let position = (x as i64, y as i64);
            if "#".contains(v) {
                walls.insert(position);
            } else if "@".contains(v) {
                start = position;
                items.insert(position, v);
            } else if !".\n".contains(v) {
                items.insert(position, v);
            }
        }
    }

    let mut distances: HashMap<char, HashMap<char, (usize, BitSet)>> = HashMap::new();
    for (a_pos, a) in items.iter() {
        if !(a.is_ascii_lowercase() || a == &'@') {
            continue;
        }
        for (b_pos, b) in items.iter() {
            if a == b
                || !(b.is_ascii_lowercase() || b == &'@')
                || distances.get(a).filter(|m| m.contains_key(b)).is_some()
            {
                continue;
            }
            if let Some((distance, between)) = distance(*a_pos, *b_pos, &walls, &items) {
                distances
                    .entry(*a)
                    .or_default()
                    .insert(*b, (distance, between));
                distances
                    .entry(*b)
                    .or_default()
                    .insert(*a, (distance, between));
            }
        }
    }

    items.remove(&start);
    let keys_count = items.values().filter(|&c| c.is_ascii_lowercase()).count();

    let mut best = HashMap::new();
    let mut queue = BinaryHeap::new();
    queue.push((Reverse(0), '@', BitSet::default()));
    while let Some((Reverse(dist_u), u, mut keys)) = queue.pop() {
        if u != '@' {
            keys.add(u as u8 - 'a' as u8);
        }
        if keys.len() == keys_count {
            return dist_u;
        }

        for (v, (len_u_v, between)) in distances[&u].iter() {
            if v == &'@' {
                continue;
            }
            if !keys.contains_all(*between) {
                continue;
            }
            if keys.contains(*v as u8 - 'a' as u8) {
                continue;
            }
            let alternative = dist_u.saturating_add(*len_u_v);
            if alternative < *best.get(&(*v, keys)).unwrap_or(&std::usize::MAX) {
                best.insert((*v, keys), alternative);
                queue.push((Reverse(alternative), *v, keys));
            }
        }
    }
    0
}

#[derive(Default, Debug, PartialEq, Hash, Eq, Clone, Copy, Ord, PartialOrd)]
struct BitSet {
    bits: u32,
}

impl BitSet {
    fn contains(&self, key: u8) -> bool {
        (self.bits >> key) & 1 == 1
    }

    fn contains_all(&self, other: Self) -> bool {
        self.bits & other.bits == other.bits
    }

    fn add(&mut self, key: u8) {
        self.bits |= 1_u32 << key;
    }

    fn len(&self) -> usize {
        self.bits.count_ones() as usize
    }
}

#[cfg(test)]
mod spec {
    use super::*;

    #[test]
    fn example_8() {
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
    fn example_86() {
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
    fn example_132() {
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
    fn part1_my_input() {
        assert_eq!(part1(INPUT), 4270);
    }
}
