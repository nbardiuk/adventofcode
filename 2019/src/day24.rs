use std::collections::HashSet;
pub const INPUT: (&str, usize) = (
    "#....\n\
     #...#\n\
     ##.##\n\
     ....#\n\
     #.##.",
    200,
);

pub fn part1((input, _): (&str, usize)) -> u32 {
    let size = 5;
    let mut grid = BitSet::default();
    for (y, line) in input.lines().enumerate() {
        for (x, v) in line.chars().enumerate() {
            let (x, y) = (x as u8, y as u8);
            if "#".contains(v) {
                grid.add(x + y * size);
            }
        }
    }

    let mut seen = HashSet::new();
    seen.insert(grid);
    loop {
        let mut next = BitSet::default();
        for y in 0..size {
            for x in 0..size {
                let mut neighbours = 0;
                for (i, j) in &[(-1, 0), (1, 0), (0, -1), (0, 1)] {
                    let limit = |a: i8| {
                        if a >= 0 && a < size as i8 {
                            Some(a as u8)
                        } else {
                            None
                        }
                    };
                    if let (Some(n), Some(m)) = (limit(x as i8 + i), limit(y as i8 + j)) {
                        if grid.contains(n + m * size) {
                            neighbours += 1;
                        }
                    }
                }
                let index = x + y * size;
                if grid.contains(index) && neighbours == 1
                    || !grid.contains(index) && [1, 2].contains(&neighbours)
                {
                    next.add(index);
                }
            }
        }
        let new_state = seen.insert(next);
        if !new_state {
            return next.bits;
        }
        grid = next;
    }
}

pub fn part2((input, duration): (&str, usize)) -> u32 {
    let size = 5;
    let mut grid = BitSet::default();
    for (y, line) in input.lines().enumerate() {
        for (x, v) in line.chars().enumerate() {
            let (x, y) = (x as u8, y as u8);
            if "#".contains(v) {
                grid.add(x + y * size);
            }
        }
    }

    let mut levels = vec![grid];
    for _time in 0..duration {
        let mut next_levels = vec![];
        if levels[0].len() > 0 {
            levels.insert(0, BitSet::default());
        }
        if levels[levels.len() - 1].len() > 0 {
            levels.push(BitSet::default());
        }
        for (level, &grid) in levels.iter().enumerate() {
            let mut next = BitSet::default();
            for y in 0..size {
                for x in 0..size {
                    if x == 2 && y == 2 {
                        continue;
                    }
                    let mut neighbours = 0;
                    for &(i, j) in &[(-1, 0), (1, 0), (0, -1), (0, 1)] {
                        let recur = |x: i8, y: i8| {
                            if level > 0 && x == 2 && y == 2 {
                                if j == 1 {
                                    (0..size).map(|i| (level - 1, i)).collect()
                                } else if j == -1 {
                                    (0..size).map(|i| (level - 1, i + 4 * size)).collect()
                                } else if i == 1 {
                                    (0..size).map(|i| (level - 1, i * size)).collect()
                                } else {
                                    (0..size).map(|i| (level - 1, 4 + i * size)).collect()
                                }
                            } else if 0 <= x && x < size as i8 {
                                if 0 <= y && y < size as i8 {
                                    vec![(level, x as u8 + y as u8 * size)]
                                } else if y < 0 {
                                    vec![(level + 1, 7)]
                                } else {
                                    vec![(level + 1, 17)]
                                }
                            } else if x < 0 {
                                vec![(level + 1, 11)]
                            } else {
                                vec![(level + 1, 13)]
                            }
                        };
                        for (level, index) in recur(x as i8 + i, y as i8 + j) {
                            if levels
                                .get(level)
                                .map(|grid| grid.contains(index))
                                .unwrap_or(false)
                            {
                                neighbours += 1;
                            }
                        }
                    }
                    let index = x + y * size;
                    if grid.contains(index) && neighbours == 1
                        || !grid.contains(index) && [1, 2].contains(&neighbours)
                    {
                        next.add(index);
                    }
                }
            }
            next_levels.push(next);
        }
        levels = next_levels;
    }
    levels.iter().map(|l| l.len()).sum()
}

#[derive(Default, Debug, PartialEq, Hash, Eq, Clone, Copy, Ord, PartialOrd)]
struct BitSet {
    bits: u32,
}
impl BitSet {
    fn contains(self, key: u8) -> bool {
        (self.bits >> key) & 1 == 1
    }

    fn add(&mut self, key: u8) {
        self.bits |= 1_u32 << key;
    }

    fn len(self) -> u32 {
        self.bits.count_ones()
    }
}

#[cfg(test)]
mod spec {
    use super::*;

    #[test]
    fn part1_example() {
        assert_eq!(
            part1((
                "....#\n\
                 #..#.\n\
                 #..##\n\
                 ..#..\n\
                 #....",
                10
            )),
            2129920
        );
    }

    #[test]
    fn part1_my_input() {
        assert_eq!(part1(INPUT), 7543003);
    }

    #[test]
    fn part2_example() {
        assert_eq!(
            part2((
                "....#\n\
                 #..#.\n\
                 #..##\n\
                 ..#..\n\
                 #....",
                10
            )),
            99
        );
    }

    #[test]
    fn part2_my_input() {
        assert_eq!(part2(INPUT), 1975);
    }
}
