use std::collections::HashMap;
use std::collections::HashSet;
use std::collections::VecDeque;

pub const INPUT: &str = include_str!("../res/day20.txt");

pub fn part1(input: &str) -> usize {
    let mut corridors = HashSet::new();
    let mut grid = HashMap::new();
    for (y, line) in input.lines().enumerate() {
        for (x, v) in line.chars().enumerate() {
            if '.' == v {
                corridors.insert((x as i64, y as i64));
            }
            grid.insert((x as i64, y as i64), v);
        }
    }

    let mut portal_positions = HashMap::new();
    let mut portal_labels: HashMap<[char; 2], Vec<(i64, i64)>> = HashMap::new();
    let mut start = (0, 0);
    for &(x, y) in corridors.iter() {
        if let Some(label) = {
            if grid[&(x - 1, y)].is_ascii_alphabetic() {
                Some([grid[&(x - 2, y)], grid[&(x - 1, y)]])
            } else if grid[&(x + 1, y)].is_ascii_alphabetic() {
                Some([grid[&(x + 1, y)], grid[&(x + 2, y)]])
            } else if grid[&(x, y - 1)].is_ascii_alphabetic() {
                Some([grid[&(x, y - 2)], grid[&(x, y - 1)]])
            } else if grid[&(x, y + 1)].is_ascii_alphabetic() {
                Some([grid[&(x, y + 1)], grid[&(x, y + 2)]])
            } else {
                None
            }
        } {
            portal_labels.entry(label).or_default().push((x, y));
            portal_positions.insert((x, y), label);
            if label == ['A', 'A'] {
                start = (x, y);
            }
        }
    }

    let mut seen = HashSet::new();
    let mut queue = VecDeque::new();
    queue.push_back((0, start));
    while let Some((distance, (x, y))) = queue.pop_front() {
        if let Some(portal) = portal_positions.get(&(x, y)) {
            if portal == &['Z', 'Z'] {
                return distance;
            }
            for neighbour in portal_labels[portal].iter() {
                if !seen.contains(neighbour) {
                    seen.insert(*neighbour);
                    queue.push_back((distance + 1, *neighbour));
                }
            }
        }
        for (i, j) in &[(-1, 0), (1, 0), (0, -1), (0, 1)] {
            let neighbour = (x + i, y + j);
            if corridors.contains(&neighbour) && !seen.contains(&neighbour) {
                seen.insert(neighbour);
                queue.push_back((distance + 1, neighbour));
            }
        }
    }

    0
}

#[cfg(test)]
mod spec {
    use super::*;

    #[test]
    fn part1_example_23() {
        assert_eq!(
            part1(
                "
         A           
         A           
  #######.#########  
  #######.........#  
  #######.#######.#  
  #######.#######.#  
  #######.#######.#  
  #####  B    ###.#  
BC...##  C    ###.#  
  ##.##       ###.#  
  ##...DE  F  ###.#  
  #####    G  ###.#  
  #########.#####.#  
DE..#######...###.#  
  #.#########.###.#  
FG..#########.....#  
  ###########.#####  
             Z       
             Z       "
            ),
            23
        );
    }

    #[test]
    fn part1_example_58() {
        assert_eq!(
            part1(
                "
                   A               
                   A               
  #################.#############  
  #.#...#...................#.#.#  
  #.#.#.###.###.###.#########.#.#  
  #.#.#.......#...#.....#.#.#...#  
  #.#########.###.#####.#.#.###.#  
  #.............#.#.....#.......#  
  ###.###########.###.#####.#.#.#  
  #.....#        A   C    #.#.#.#  
  #######        S   P    #####.#  
  #.#...#                 #......VT
  #.#.#.#                 #.#####  
  #...#.#               YN....#.#  
  #.###.#                 #####.#  
DI....#.#                 #.....#  
  #####.#                 #.###.#  
ZZ......#               QG....#..AS
  ###.###                 #######  
JO..#.#.#                 #.....#  
  #.#.#.#                 ###.#.#  
  #...#..DI             BU....#..LF
  #####.#                 #.#####  
YN......#               VT..#....QG
  #.###.#                 #.###.#  
  #.#...#                 #.....#  
  ###.###    J L     J    #.#.###  
  #.....#    O F     P    #.#...#  
  #.###.#####.#.#####.#####.###.#  
  #...#.#.#...#.....#.....#.#...#  
  #.#####.###.###.#.#.#########.#  
  #...#.#.....#...#.#.#.#.....#.#  
  #.###.#####.###.###.#.#.#######  
  #.#.........#...#.............#  
  #########.###.###.#############  
           B   J   C               
           U   P   P               "
            ),
            58
        );
    }

    #[test]
    fn part1_my_input() {
        assert_eq!(part1(INPUT), 400);
    }
}
