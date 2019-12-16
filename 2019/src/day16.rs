pub const INPUT: &str = include_str!("../res/day16.txt");

pub fn part1(input: &str) -> String {
    let mut digits = input
        .trim_end_matches('\n')
        .chars()
        .map(|c| c.to_digit(10).unwrap() as u8)
        .collect::<Vec<u8>>();

    for _ in 0..100 {
        digits = phase(&digits);
    }

    digits.iter().take(8).map(|d| d.to_string()).collect()
}

pub fn part2(input: &str) -> String {
    let digits = input
        .trim_end_matches('\n')
        .chars()
        .map(|c| c.to_digit(10).unwrap() as u8)
        .collect::<Vec<u8>>();

    let offset = digits.iter().take(7).fold(0, |r, &d| r * 10 + d as usize);
    let final_len = 10000 * digits.len() - offset;
    let singe_offset = offset % digits.len();
    let mut digits = digits
        .iter()
        .cycle()
        .skip(singe_offset)
        .take(final_len)
        .cloned()
        .collect::<Vec<u8>>();

    for _ in 0..100 {
        digits = phase2(&digits);
    }

    digits.iter().take(8).map(|d| d.to_string()).collect()
}

fn phase(input: &[u8]) -> Vec<u8> {
    let mut result = Vec::with_capacity(input.len());
    let pattern = [0, 1, 0, -1];
    for i in 0..input.len() {
        let mut sum = 0;
        for j in 0..input.len() {
            let p = ((j + 1) / (i + 1)) % 4;
            sum += input[j] as i64 * pattern[p];
        }
        result.push((sum % 10).abs() as u8);
    }
    result
}

fn phase2(input: &[u8]) -> Vec<u8> {
    let mut result = Vec::with_capacity(input.len());
    let mut sum: usize = input.iter().map(|&i| i as usize).sum();
    for i in 0..input.len() {
        result.push((sum % 10) as u8);
        sum -= input[i] as usize;
    }
    result
}

#[cfg(test)]
mod spec {
    use super::*;

    #[test]
    fn example_phase() {
        assert_eq!(phase(&[1, 2, 3, 4, 5, 6, 7, 8]), [4, 8, 2, 2, 6, 1, 5, 8]);
        assert_eq!(phase(&[4, 8, 2, 2, 6, 1, 5, 8]), [3, 4, 0, 4, 0, 4, 3, 8]);
        assert_eq!(phase(&[3, 4, 0, 4, 0, 4, 3, 8]), [0, 3, 4, 1, 5, 5, 1, 8]);
        assert_eq!(phase(&[0, 3, 4, 1, 5, 5, 1, 8]), [0, 1, 0, 2, 9, 4, 9, 8]);
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
        assert_eq!(part2(INPUT), "");
    }
}
