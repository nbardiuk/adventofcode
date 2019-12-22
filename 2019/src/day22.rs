pub const INPUT: &str = include_str!("../res/day22.txt");

#[derive(Debug)]
enum Command {
    IntoStack,
    WithIncrement(i64),
    Cut(i64),
}
pub fn part1(input: &str) -> i64 {
    let commands = input
        .lines()
        .map(|line| {
            if line.starts_with("deal with increment") {
                let rest = line[20..].parse().unwrap();
                Command::WithIncrement(rest)
            } else if line.starts_with("cut") {
                let rest = line[4..].parse().unwrap();
                Command::Cut(rest)
            } else {
                Command::IntoStack
            }
        })
        .collect::<Vec<_>>();

    let mut card = 2019;
    let size = 10007;
    for command in commands {
        match command {
            Command::IntoStack => card = (size - 1) - card,
            Command::Cut(index) => card = (card + size - index) % size,
            Command::WithIncrement(increment) => card = (card * increment) % size,
        }
    }
    card
}


#[cfg(test)]
mod spec {
    use super::*;
    #[test]
    fn part1_my_input() {
        assert_eq!(part1(INPUT), 3939);
    }
}
