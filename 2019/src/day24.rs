use std::collections::HashSet;
pub const INPUT: (&str, usize) = (
    "#....\n\
     #...#\n\
     ##.##\n\
     ....#\n\
     #.##.",
    200,
);

const SIZE: i8 = 5;

pub fn part1((input, _): (&str, usize)) -> u32 {
    let mut grid = FlatGrid::new(read_grid(input));
    let mut seen = HashSet::new();
    loop {
        if !seen.insert(grid.cells) {
            return grid.cells.bits;
        }
        grid = grid.iteration();
    }
}

struct FlatGrid {
    cells: BitSet,
}
impl FlatGrid {
    fn new(cells: BitSet) -> Self {
        FlatGrid { cells }
    }

    fn iteration(&self) -> Self {
        let mut next = FlatGrid::new(BitSet::default());
        for x in 0..SIZE {
            for y in 0..SIZE {
                let neighbours = self.alive_neighbours(x, y);
                if neighbours == 1 || neighbours == 2 && !self.is_alive(x, y) {
                    next.set_alive(x, y);
                }
            }
        }
        next
    }

    fn is_alive(&self, x: i8, y: i8) -> bool {
        0 <= x && x < SIZE && 0 <= y && y < SIZE && self.cells.contains((x + y * SIZE) as u8)
    }

    fn set_alive(&mut self, x: i8, y: i8) {
        self.cells.add((x + y * SIZE) as u8)
    }

    fn alive_neighbours(&self, x: i8, y: i8) -> usize {
        [(-1, 0), (1, 0), (0, -1), (0, 1)]
            .iter()
            .filter(|(i, j)| self.is_alive(x + i, y + j))
            .count()
    }
}

pub fn part2((input, duration): (&str, usize)) -> u32 {
    let empty = BitSet::default();
    let mut levels = vec![empty, read_grid(input), empty];
    for _second in 0..duration {
        levels = iteration(&levels);
    }
    levels.iter().map(|level| level.len()).sum()
}

fn iteration(levels: &[BitSet]) -> Vec<BitSet> {
    let mut next = vec![BitSet::default(); levels.len()];
    for x in 0..SIZE {
        for y in 0..SIZE {
            if x == 2 && y == 2 {
                continue;
            }
            for z in 0..levels.len() {
                let index = x + y * SIZE;
                let neighbours = neighbours(&levels, x, y, z);
                if neighbours == 1 || neighbours == 2 && !levels[z].contains(index as u8) {
                    next[z].add(index as u8);
                }
            }
        }
    }
    if 0 < next[0].len() {
        next.insert(0, BitSet::default());
    }
    if 0 < next[next.len() - 1].len() {
        next.push(BitSet::default());
    }
    next
}

fn neighbours(levels: &[BitSet], x: i8, y: i8, z: usize) -> usize {
    let mut neighbours = 0;
    for &(i, j) in &[(-1, 0), (1, 0), (0, -1), (0, 1)] {
        let recur = |a: i8, b: i8| {
            if a == 2 && b == 2 {
                if 0 < z {
                    let z = z - 1;
                    if j == 1 {
                        (0..SIZE).map(|i| (z, i)).collect()
                    } else if j == -1 {
                        (0..SIZE).map(|i| (z, i + 4 * SIZE)).collect()
                    } else if i == 1 {
                        (0..SIZE).map(|i| (z, i * SIZE)).collect()
                    } else {
                        (0..SIZE).map(|i| (z, 4 + i * SIZE)).collect()
                    }
                } else {
                    vec![]
                }
            } else if 0 <= a && a < SIZE {
                if 0 <= b && b < SIZE {
                    vec![(z, a + b * SIZE)]
                } else if b < 0 {
                    vec![(z + 1, 7)]
                } else {
                    vec![(z + 1, 17)]
                }
            } else {
                let z = z + 1;
                if a < 0 {
                    vec![(z, 11)]
                } else {
                    vec![(z, 13)]
                }
            }
        };
        for (z, index) in recur(x + i, y + j) {
            if levels
                .get(z)
                .map(|grid| grid.contains(index as u8))
                .unwrap_or(false)
            {
                neighbours += 1;
            }
        }
    }
    neighbours
}

fn read_grid(input: &str) -> BitSet {
    let mut grid = BitSet::default();
    for (i, c) in input.chars().filter(|c| c != &'\n').enumerate() {
        if c == '#' {
            grid.add(i as u8);
        }
    }
    grid
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

    const EXAMPLE: (&str, usize) = ("....#\n#..#.\n#..##\n..#..\n#....", 10);

    #[test]
    fn part1_example() {
        assert_eq!(part1(EXAMPLE), 2129920);
    }

    #[test]
    fn part1_my_input() {
        assert_eq!(part1(INPUT), 7543003);
    }

    #[test]
    fn part2_example() {
        assert_eq!(part2(EXAMPLE), 99);
    }

    #[test]
    fn part2_my_input() {
        assert_eq!(part2(INPUT), 1975);
    }
}
