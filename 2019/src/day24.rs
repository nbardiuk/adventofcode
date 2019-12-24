use std::collections::HashSet;
pub const INPUT: &str = "\
                         #....\n\
                         #...#\n\
                         ##.##\n\
                         ....#\n\
                         #.##.";

pub fn part1(input: &str) -> u32 {
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

    let pp = |grid: BitSet| {
        for y in 0..size {
            for x in 0..size {
                let index = x + y * size;
                if grid.contains(index) {
                    print!("#");
                } else {
                    print!(".");
                }
            }
            println!();
        }
        println!("Score {}", grid.bits);
    };
    // pp(grid);

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
        // pp(next);
        let new_state = seen.insert(next);
        if !new_state {
            return next.bits;
        }
        grid = next;
    }
}

#[derive(Default, Debug, PartialEq, Hash, Eq, Clone, Copy, Ord, PartialOrd)]
struct BitSet {
    bits: u32,
}
impl BitSet {
    fn contains(self, key: u8) -> bool {
        (self.bits >> key) & 1 == 1
    }

    // fn contains_all(self, other: Self) -> bool {
    //     self.bits & other.bits == other.bits
    // }

    fn add(&mut self, key: u8) {
        self.bits |= 1_u32 << key;
    }

    // fn len(self) -> usize {
    //     self.bits.count_ones() as usize
    // }
}

#[cfg(test)]
mod spec {
    use super::*;

    #[test]
    fn part1_example() {
        assert_eq!(
            part1(
                "....#\n\
                 #..#.\n\
                 #..##\n\
                 ..#..\n\
                 #...."
            ),
            2129920
        );
    }
    #[test]
    fn part1_my_input() {
        assert_eq!(part1(INPUT), 7543003);
    }
}
