pub const INPUT: (usize, usize) = (171309, 643603);

pub fn part1((from, to): (usize, usize)) -> usize {
    (from..=to).filter(|&n| matches_part1(n)).count()
}
pub fn part2((from, to): (usize, usize)) -> usize {
    (from..=to).filter(|&n| matches_part2(n)).count()
}

fn matches_part1(number: usize) -> bool {
    let mut prev = 0;
    let mut has_pair = false;
    for digit in digits(number) {
        if prev > digit {
            return false; // not growing
        };
        if prev == digit {
            has_pair = true;
        }
        prev = digit;
    }
    has_pair
}

fn matches_part2(number: usize) -> bool {
    let mut group = (0, 0);
    let mut has_pair = false;
    for digit in digits(number) {
        group = match group {
            (prev, count) => {
                if prev > digit {
                    return false; // not growing
                };
                if prev == digit {
                    (prev, count + 1)
                } else {
                    if count == 2 {
                        has_pair = true;
                    }
                    (digit, 1)
                }
            }
        }
    }
    has_pair || (group.1 == 2)
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
    fn examples_part1() {
        assert_eq!(matches_part1(111111), true);
        assert_eq!(matches_part1(223450), false, "not growing");
        assert_eq!(matches_part1(123789), false, "does not have pair");
    }
    #[test]
    fn examples_part2() {
        assert_eq!(matches_part2(123456), false, "does not have pair");
        assert_eq!(matches_part2(112233), true, "growing groups");
        assert_eq!(matches_part2(123444), false, "more than 2 in group");
        assert_eq!(matches_part2(111122), true, "4 elements are 2 groups");
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
