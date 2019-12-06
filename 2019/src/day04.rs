pub const INPUT: (u32, u32) = (171309, 643603);

pub fn part1((from, to): (u32, u32)) -> usize {
    (from..=to).filter(|&n| matches_part1(n)).count()
}
pub fn part2((from, to): (u32, u32)) -> usize {
    (from..=to).filter(|&n| matches_part2(n)).count()
}

fn matches_part1(number: u32) -> bool {
    let digits = digits_of(number);
    let is_sorted = || (&digits).windows(2).all(|w| w[0] <= w[1]);
    let has_pairs = || has_group_size(&digits, |g| g >= 2);
    is_sorted() && has_pairs()
}

fn matches_part2(number: u32) -> bool {
    let digits = digits_of(number);
    let is_sorted = || (&digits).windows(2).all(|w| w[0] <= w[1]);
    let has_pairs = || has_group_size(&digits, |g| g == 2);
    is_sorted() && has_pairs()
}

fn has_group_size(values: &[u8], p: fn(u8) -> bool) -> bool {
    let mut group = 0;
    let mut last = None;
    for value in values {
        if group == 0 || last == Some(value) {
            group += 1;
        } else {
            if p(group) {
                return true;
            };
            group = 1;
        }
        last = Some(value)
    }
    p(group)
}

fn digits_of(mut number: u32) -> Vec<u8> {
    let mut result = Vec::with_capacity(6);
    while number > 0 {
        result.push((number % 10) as u8);
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
        assert_eq!(digits_of(111111), vec!(1, 1, 1, 1, 1, 1));
        assert_eq!(digits_of(12), vec!(1, 2));
        assert_eq!(digits_of(10), vec!(1, 0));
    }

    #[test]
    fn examples_part1() {
        assert_eq!(matches_part1(111111), true);
        assert_eq!(matches_part1(223450), false, "not growing");
        assert_eq!(matches_part1(123789), false, "does not have pair");
    }

    #[test]
    fn examples_part2() {
        assert_eq!(matches_part2(112233), true, "growing groups");
        assert_eq!(matches_part2(111122), true, "has a pair");
        assert_eq!(matches_part2(123456), false, "does not have pair");
        assert_eq!(matches_part2(123444), false, "more than 2 in the group");
    }

    #[test]
    fn part1_my_input() {
        assert_eq!(part1(INPUT), 1625);
    }

    #[test]
    fn part2_my_input() {
        assert_eq!(part2(INPUT), 1111);
    }
}
