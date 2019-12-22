pub const INPUT: &str = include_str!("../res/day22.txt");

#[derive(Debug)]
enum Command {
    IntoStack,
    WithIncrement(i32),
    Cut(i32),
}
use Command::*;

pub fn part1(input: &str) -> i32 {
    let size = 10007;
    let mut card = 2019;
    for command in parse_commands(input) {
        card = match command {
            IntoStack => (size - 1) - card,
            Cut(index) => (card + size - index) % size,
            WithIncrement(increment) => (card * increment) % size,
        }
    }
    card
}

pub fn part2(input: &str) -> i128 {
    let size = 119_315_717_514_047_i128;
    let add = |a, b| (a + b) % size;
    let mul = |a, b| (a * b) % size;
    let pow = |mut base: i128, mut exp: i128| {
        let mut result = 1;
        while exp > 0 {
            if exp & 1 == 1 {
                result = mul(result, base)
            };
            base = mul(base, base);
            exp >>= 1;
        }
        result
    };

    let mut a = 1;
    let mut b = 0;
    for command in parse_commands(input).rev() {
        match command {
            IntoStack => {
                a = mul(a, -1);
                b = mul(b + 1, -1);
            }
            Cut(index) => b = add(b, index as i128),
            WithIncrement(increment) => {
                let p = pow(increment as i128, size - 2);
                a = mul(a, p);
                b = mul(b, p);
            }
        }
    }

    let n = 101_741_582_076_661_i128;
    add(
        mul(pow(a, n), 2020),
        mul(mul(b, add(pow(a, n), size - 1)), pow(a - 1, size - 2)),
    )
}

fn parse_commands<'a>(input: &'a str) -> impl DoubleEndedIterator<Item = Command> + 'a {
    input.lines().map(|line| match &line[0..6] {
        "deal w" => WithIncrement(line[20..].parse().unwrap()),
        "deal i" => IntoStack,
        _ => Cut(line[4..].parse().unwrap()),
    })
}

#[cfg(test)]
mod spec {
    use super::*;

    #[test]
    fn part1_my_input() {
        assert_eq!(part1(INPUT), 3939);
    }

    #[test]
    fn part2_my_input() {
        assert_eq!(part2(INPUT), 55574110161534);
    }
}
