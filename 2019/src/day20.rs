use std::cmp::Reverse;
use std::collections::BinaryHeap;
use std::collections::HashMap;
use std::collections::HashSet;
use std::collections::VecDeque;

pub const INPUT: &str = include_str!("../res/day20.txt");

pub fn part1(input: &str) -> usize {
    let (start, portal_positions, portal_labels, corridors) = read_input(input);

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
    let (start, portal_positions, portal_labels, corridors) = read_input(input);

    let minx = *portal_positions.keys().map(|(x, _)| x).min().unwrap_or(&0);
    let maxx = *portal_positions.keys().map(|(x, _)| x).max().unwrap_or(&0);
    let miny = *portal_positions.keys().map(|(_, y)| y).min().unwrap_or(&0);
    let maxy = *portal_positions.keys().map(|(_, y)| y).max().unwrap_or(&0);
    let is_outer = |(x, y)| x == minx || x == maxx || y == miny || y == maxy;

    let distances = distances_between(
        &portal_positions.keys().cloned().collect::<HashSet<_>>(),
        &corridors,
    );

    let mut best = HashMap::new();
    let mut queue = BinaryHeap::new();
    queue.push((Reverse(0), Reverse(0), (start)));
    while let Some((Reverse(distance), Reverse(level), (x, y))) = queue.pop() {
        if let Some(portal) = portal_positions.get(&(x, y)) {
            if level == 0 && *portal == ['Z', 'Z'] {
                return distance;
            }

            for (pos, dist) in distances[&(x, y)].iter() {
                let alternative = distance + *dist;
                if alternative < *best.get(&(pos, level)).unwrap_or(&std::usize::MAX) {
                    best.insert((pos, level), alternative);
                    queue.push((Reverse(alternative), Reverse(level), *pos));
                }
            }

            for neighbour in portal_labels[portal].iter().filter(|n| n != &&(x, y)) {
                let mut level = level;
                if is_outer((x, y)) {
                    if level > 0 {
                        level -= 1;
                    } else {
                        break;
                    }
                } else {
                    level += 1;
                };
                let alternative = distance + 1;
                if alternative < *best.get(&(neighbour, level)).unwrap_or(&std::usize::MAX) {
                    best.insert((neighbour, level), alternative);
                    queue.push((Reverse(alternative), Reverse(level), *neighbour));
                }
            }
        }
    }
    0
}

type Pos = (i64, i64);
type Label = [char; 2];
fn read_input(
    input: &str,
) -> (
    Pos,
    HashMap<Pos, Label>,
    HashMap<Label, Vec<Pos>>,
    HashSet<Pos>,
) {
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
    let mut portal_labels: HashMap<Label, Vec<Pos>> = HashMap::new();
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
    (start, portal_positions, portal_labels, corridors)
}

fn distances_between(
    portals: &HashSet<Pos>,
    corridors: &HashSet<Pos>,
) -> HashMap<Pos, HashMap<Pos, usize>> {
    portals
        .iter()
        .map(|pos| (*pos, distances_from(*pos, corridors, portals)))
        .collect()
}

fn distances_from(
    start: Pos,
    corridors: &HashSet<Pos>,
    portals: &HashSet<Pos>,
) -> HashMap<Pos, usize> {
    let mut seen = HashSet::new();
    let mut distances = HashMap::new();
    let mut queue = VecDeque::new();
    queue.push_front((0, start));
    while let Some((distance, (x, y))) = queue.pop_front() {
        let distance = distance + 1;
        for (i, j) in &[(-1, 0), (1, 0), (0, -1), (0, 1)] {
            let neighbour = (x + i, y + j);

            if !corridors.contains(&neighbour) || seen.contains(&neighbour) {
                continue;
            }

            if portals.contains(&neighbour) {
                distances.insert(neighbour, distance);
            }

            seen.insert(neighbour);
            queue.push_back((distance, neighbour));
        }
    }
    distances
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
