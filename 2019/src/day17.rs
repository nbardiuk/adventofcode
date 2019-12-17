use crate::intcode::Computer;
use std::collections::HashMap;
pub const INPUT: &str = include_str!("../res/day17.txt");

pub fn part1(input: &str) -> usize {
    let output = Computer::parse(input).run();

    let mut grid = HashMap::new();
    for (y, line) in output.split(|&i| i == 10).enumerate() {
        for (x, value) in line
            .iter()
            .enumerate()
            .filter(|&(_, i)| *i != 10 && *i != 46)
        {
            grid.insert((x, y), *value);
        }
    }

    let intersections = grid.keys().filter(|&&(x, y)| {
        grid.get(&(x.saturating_sub(1), y)).is_some()
            && grid.get(&(x + 1, y)).is_some()
            && grid.get(&(x, y.saturating_sub(1))).is_some()
            && grid.get(&(x, y + 1)).is_some()
    });

    pprint(&grid);

    intersections.map(|(x, y)| x * y).sum()
}

fn pprint(panels: &HashMap<(usize, usize), i64>) {
    let columns = panels.keys().map(|(x, _)| x).max().cloned().unwrap_or(0);
    let rows = panels.keys().map(|(_, y)| y).max().cloned().unwrap_or(0);

    for y in 0..=rows {
        let mut row = String::new();
        for x in 0..columns {
            let c = match panels.get(&(x, y)) {
                Some(_) => '█',
                _ => ' ',
            };
            row.push(c);
            print!("{}", c);
        }
        println!();
    }
}

#[cfg(test)]
mod spec {
    use super::*;

    #[test]
    fn part1_my_input() {
        assert_eq!(part1(INPUT), 3660);
    }
}
