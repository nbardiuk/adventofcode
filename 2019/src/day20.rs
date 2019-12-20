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

pub fn part2(input: &str) -> usize {
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
    let minx = *portal_positions.keys().map(|(x, _)| x).min().unwrap_or(&0);
    let maxx = *portal_positions.keys().map(|(x, _)| x).max().unwrap_or(&0);
    let miny = *portal_positions.keys().map(|(_, y)| y).min().unwrap_or(&0);
    let maxy = *portal_positions.keys().map(|(_, y)| y).max().unwrap_or(&0);
    let is_outer = |(x, y)| [minx, maxx].contains(&x) || [miny, maxy].contains(&y);

    let mut seen = HashSet::new();
    let mut queue = VecDeque::new();
    queue.push_back((0, start, 0));
    while let Some((distance, (x, y), level)) = queue.pop_front() {
        if let Some(portal) = portal_positions.get(&(x, y)) {
            if portal == &['Z', 'Z'] && level == 0 {
                return distance;
            }
            for neighbour in portal_labels[portal].iter().filter(|n| n != &&(x, y)) {
                let mut levels = level;
                if is_outer((x, y)) {
                    if levels > 0 {
                        levels -= 1;
                    } else {
                        break;
                    }
                } else {
                    levels += 1;
                };
                if !seen.contains(&(*neighbour, levels)) {
                    seen.insert((*neighbour, levels));
                    queue.push_back((distance + 1, *neighbour, levels));
                }
            }
        }
        for (i, j) in &[(-1, 0), (1, 0), (0, -1), (0, 1)] {
            let neighbour = (x + i, y + j);
            if corridors.contains(&neighbour) && !seen.contains(&(neighbour, level)) {
                seen.insert((neighbour, level));
                queue.push_back((distance + 1, neighbour, level));
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
    fn part2_example_26() {
        assert_eq!(
            part2(
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
            26
        );
    }

    #[test]
    fn part2_example_396() {
        assert_eq!(
            part2(
                "
             Z L X W       C                 
             Z P Q B       K                 
  ###########.#.#.#.#######.###############  
  #...#.......#.#.......#.#.......#.#.#...#  
  ###.#.#.#.#.#.#.#.###.#.#.#######.#.#.###  
  #.#...#.#.#...#.#.#...#...#...#.#.......#  
  #.###.#######.###.###.#.###.###.#.#######  
  #...#.......#.#...#...#.............#...#  
  #.#########.#######.#.#######.#######.###  
  #...#.#    F       R I       Z    #.#.#.#  
  #.###.#    D       E C       H    #.#.#.#  
  #.#...#                           #...#.#  
  #.###.#                           #.###.#  
  #.#....OA                       WB..#.#..ZH
  #.###.#                           #.#.#.#  
CJ......#                           #.....#  
  #######                           #######  
  #.#....CK                         #......IC
  #.###.#                           #.###.#  
  #.....#                           #...#.#  
  ###.###                           #.#.#.#  
XF....#.#                         RF..#.#.#  
  #####.#                           #######  
  #......CJ                       NM..#...#  
  ###.#.#                           #.###.#  
RE....#.#                           #......RF
  ###.###        X   X       L      #.#.#.#  
  #.....#        F   Q       P      #.#.#.#  
  ###.###########.###.#######.#########.###  
  #.....#...#.....#.......#...#.....#.#...#  
  #####.#.###.#######.#######.###.###.#.#.#  
  #.......#.......#.#.#.#.#...#...#...#.#.#  
  #####.###.#####.#.#.#.#.###.###.#.###.###  
  #.......#.....#.#...#...............#...#  
  #############.#.#.###.###################  
               A O F   N                     
               A A D   M                     "
            ),
            396
        );
    }

    #[test]
    fn part1_my_input() {
        assert_eq!(part1(INPUT), 400);
    }

    #[test]
    fn part2_my_input() {
        assert_eq!(part2(INPUT), 4986);
    }
}
