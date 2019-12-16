use rayon::prelude::*;
pub const INPUT: &str = include_str!("../res/day16.txt");

pub fn part1(input: &str) -> String {
    let digits = (0..100).fold(parse_digits(input), |digits, _| phase1(&digits));
    format_result(&digits)
}

pub fn part2(input: &str) -> String {
    let mut digits = parse_digits(input);

    let offset = digits.iter().take(7).fold(0, |r, &d| r * 10 + d as usize);

    let len = 10_000 * digits.len() - offset;
    let off = offset % digits.len();
    digits = digits.iter().cycle().skip(off).take(len).cloned().collect();

    digits = (0..100).fold(digits, |digits, _| phase2(digits));

    format_result(&digits)
}

const PATTERN: [i16; 4] = [0, 1, 0, -1];

fn phase1(input: &[i16]) -> Vec<i16> {
    let results = (0..input.len())
        .into_par_iter()
        .map(|i| {
            let mut sum = 0;
            for (j, &digit) in input.iter().enumerate().skip(i) {
                sum += digit * PATTERN[((j + 1) / (i + 1)) % 4];
            }
            (i, (sum % 10).abs())
        })
        .collect::<Vec<_>>();

    let mut digits = vec![0; input.len()];
    for (i, digit) in results {
        digits[i] = digit;
    }
    digits
}

fn phase2(mut digits: Vec<i16>) -> Vec<i16> {
    let mut sum: usize = digits.iter().map(|&i| i as usize).sum();
    for digit in digits.iter_mut() {
        let temp = *digit;
        *digit = (sum % 10) as i16;
        sum -= temp as usize;
    }
    digits
}

fn parse_digits(input: &str) -> Vec<i16> {
    input
        .trim_end_matches('\n')
        .chars()
        .map(|c| c.to_digit(10).unwrap() as i16)
        .collect()
}

fn format_result(digits: &[i16]) -> String {
    digits.iter().take(8).map(|d| d.to_string()).collect()
}

#[cfg(test)]
mod spec {
    use super::*;

    #[test]
    fn example_phase() {
        assert_eq!(phase1(&[1, 2, 3, 4, 5, 6, 7, 8]), [4, 8, 2, 2, 6, 1, 5, 8]);
        assert_eq!(phase1(&[4, 8, 2, 2, 6, 1, 5, 8]), [3, 4, 0, 4, 0, 4, 3, 8]);
        assert_eq!(phase1(&[3, 4, 0, 4, 0, 4, 3, 8]), [0, 3, 4, 1, 5, 5, 1, 8]);
        assert_eq!(phase1(&[0, 3, 4, 1, 5, 5, 1, 8]), [0, 1, 0, 2, 9, 4, 9, 8]);
    }

    #[test]
    fn part1_exmples() {
        assert_eq!(part1("80871224585914546619083218645595"), "24176176");
        assert_eq!(part1("19617804207202209144916044189917"), "73745418");
        assert_eq!(part1("69317163492948606335995924319873"), "52432133");
    }

    #[test]
    fn part2_exmples() {
        assert_eq!(part2("03036732577212944063491565474664"), "84462026");
        assert_eq!(part2("02935109699940807407585447034323"), "78725270");
        assert_eq!(part2("03081770884921959731165446850517"), "53553731");
    }

    #[test]
    fn part1_my_input() {
        assert_eq!(part1(INPUT), "45834272");
    }

    #[test]
    fn part2_my_input() {
        assert_eq!(part2(INPUT), "37615297");
    }
}
