use num_bigint::*;

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

pub fn part2(input: &str) -> i64 {
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

    let size = ToBigInt::to_bigint(&119_315_717_514_047_usize).unwrap();
    let mut a = ToBigInt::to_bigint(&1).unwrap();
    let mut b = ToBigInt::to_bigint(&0).unwrap();
    for command in commands.iter().rev() {
        match command {
            Command::IntoStack => {
                b += 1;
                a *= -1;
                b *= -1;
            }
            Command::Cut(index) => b = b + index,
            Command::WithIncrement(increment) => {
                let p = ToBigInt::to_bigint(increment)
                    .unwrap()
                    .modpow(&(&size - 2), &size);
                a *= &p;
                b *= &p;
            }
        }
        a %= &size;
        b %= &size;
    }

    let x = 2020;
    let n = ToBigInt::to_bigint(&101_741_582_076_661_usize).unwrap();

    let pow = |a: &BigInt, n: &BigInt| a.modpow(n, &size);

    let result: BigInt =
        (pow(&a, &n) * x + b * (pow(&a, &n) + &size - 1) * pow(&(&a - 1), &(&size - 2))) % &size;

    result.to_str_radix(10).parse().unwrap()
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
