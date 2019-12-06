use std::collections::HashMap;
use std::collections::VecDeque;

pub const INPUT: &str = include_str!("../res/day06.txt");

pub fn part1(input: &str) -> usize {
    let mut tree = HashMap::new();
    for (a, b) in split_pairs(input) {
        tree.entry(a).or_insert_with(|| vec![]).push(b);
    }

    fn recur(node: &str, level: usize, tree: &HashMap<&str, Vec<&str>>) -> usize {
        tree.get(node)
            .unwrap_or(&vec![])
            .iter()
            .map(|child| level + recur(child, level + 1, tree))
            .sum()
    }
    recur("COM", 1, &tree)
}

pub fn part2(input: &str) -> usize {
    let mut graph = HashMap::new();
    for (a, b) in split_pairs(input) {
        graph.entry(a).or_insert_with(|| vec![]).push(b);
        graph.entry(b).or_insert_with(|| vec![]).push(a);
    }

    let mut distances = HashMap::<&str, usize>::new();
    let mut queue = VecDeque::new();
    queue.push_back(("YOU", 0));
    while let Some((node, distance)) = queue.pop_front() {
        distances.insert(node, distance);
        graph
            .get(node)
            .unwrap_or(&vec![])
            .iter()
            .filter(|&neighbour| !distances.contains_key(neighbour))
            .for_each(|neighbour| queue.push_back((neighbour, distance + 1)));
    }
    distances.get("SAN").unwrap() - 2
}

fn split_pairs(input: &str) -> impl Iterator<Item = (&str, &str)> {
    input.lines().filter_map(|line| {
        let mut elems = line.split(')');
        let a = elems.next()?;
        let b = elems.next()?;
        Some((a, b))
    })
}

#[cfg(test)]
mod spec {
    use super::*;

    #[test]
    fn part1_exmple() {
        assert_eq!(
            part1(
                "COM)B\n\
                 B)C\n\
                 C)D\n\
                 D)E\n\
                 E)F\n\
                 B)G\n\
                 G)H\n\
                 D)I\n\
                 E)J\n\
                 J)K\n\
                 K)L"
            ),
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
            part2(
                "COM)B\n\
                 B)C\n\
                 C)D\n\
                 D)E\n\
                 E)F\n\
                 B)G\n\
                 G)H\n\
                 D)I\n\
                 E)J\n\
                 J)K\n\
                 K)L\n\
                 K)YOU\n\
                 I)SAN"
            ),
            4
        );
    }

    #[test]
    fn part2_my_input() {
        assert_eq!(part2(INPUT), 412);
    }
}
