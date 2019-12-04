pub const INPUT: &str = "171309-643603";

pub fn part1(input: &str) -> usize {
    if let [from, to] = input.split('-').collect::<Vec<_>>().as_slice() {
        if let (Ok(from), Ok(to)) = (from.parse(), to.parse()) {
            return (from..=to).filter(|&n| matches_password(n)).count();
        }
    };
    0
}

fn matches_password(number: usize) -> bool {
    let (growing, has_dups, _) =
        digits(number)
            .iter()
            .fold(
                (false, false, None),
                |(growing, has_dups, prev), digit| match prev {
                    Some(before) => (
                        growing && before <= digit,
                        has_dups || before == digit,
                        Some(digit),
                    ),
                    None => (true, false, Some(digit)),
                },
            );
    growing && has_dups
}

fn digits(mut number: usize) -> Vec<usize> {
    let mut result = vec![];
    while number > 0 {
        result.push(number % 10);
        number /= 10;
    }
    result.reverse();
    result
}

#[cfg(test)]
mod spec {
    use super::*;

    #[test]
    fn test_digits() {
        assert_eq!(digits(111111), vec!(1, 1, 1, 1, 1, 1));
        assert_eq!(digits(12), vec!(1, 2));
        assert_eq!(digits(10), vec!(1, 0));
    }

    #[test]
    fn examples() {
        assert_eq!(matches_password(111111), true);
        assert_eq!(matches_password(223450), false);
        assert_eq!(matches_password(123789), false);
    }

    #[test]
    fn part1_my_input() {
        assert_eq!(part1(INPUT), 1625);
    }
}
