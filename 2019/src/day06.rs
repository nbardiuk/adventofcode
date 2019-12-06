use std::collections::HashMap;
use std::collections::LinkedList;

pub const INPUT: &str = include_str!("../res/day06.txt");

pub fn part1(input: &str) -> usize {
    let mut tree: HashMap<&str, Vec<&str>> = HashMap::new();
    for &(a, b) in split_pairs(input).iter() {
        tree.entry(a).or_insert_with(|| vec![]).push(b);
    }

    fn recur(node: &str, level: usize, tree: &HashMap<&str, Vec<&str>>) -> usize {
        if let Some(children) = tree.get(node) {
            children
                .iter()
                .map(|child| level + recur(child, level + 1, tree))
                .sum()
        } else {
            0
        }
    }
    recur("COM", 1, &tree)
}

pub fn part2(input: &str) -> usize {
    let mut graph: HashMap<&str, Vec<&str>> = HashMap::new();
    for &(a, b) in split_pairs(input).iter() {
        graph.entry(a).or_insert_with(|| vec![]).push(b);
        graph.entry(b).or_insert_with(|| vec![]).push(a);
    }

    let mut distances = HashMap::new();
    let mut queue = LinkedList::new();
    queue.push_back(("YOU", 0));
    while queue.front().is_some() {
        let (node, distance) = queue.pop_front().unwrap();
        distances.insert(node, distance);
        for neighbour in graph.get(node).unwrap_or(&vec![]) {
            if !distances.contains_key(neighbour) {
                queue.push_back((neighbour, distance + 1));
            }
        }
    }
    distances.get("SAN").copied().unwrap() - 2
}

fn split_pairs(input: &str) -> Vec<(&str, &str)> {
    input
        .lines()
        .filter_map(|line| {
            if let [a, b] = line.split(')').collect::<Vec<_>>()[..] {
                Some((a, b))
            } else {
                None
            }
        })
        .collect()
}

#[cfg(test)]
mod spec {
    use super::*;

    #[test]
    fn part1_exmple() {
        assert_eq!(
            part1("COM)B\nB)C\nC)D\nD)E\nE)F\nB)G\nG)H\nD)I\nE)J\nJ)K\nK)L"),
            42
        );
    }

    #[test]
    fn part1_my_input() {
        assert_eq!(part1(INPUT), 186597);
    }

    #[test]
    fn part2_exmple() {
        assert_eq!(
            part2("COM)B\nB)C\nC)D\nD)E\nE)F\nB)G\nG)H\nD)I\nE)J\nJ)K\nK)L\nK)YOU\nI)SAN"),
            4
        );
    }

    #[test]
    fn part2_my_input() {
        assert_eq!(part2(INPUT), 412);
    }
}
