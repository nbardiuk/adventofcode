pub const INPUT: &str = include_str!("../res/day08.txt");

pub fn part1(input: &str) -> usize {
    let (width, height) = (25, 6);
    let digits = input.trim().chars().collect::<Vec<_>>();
    let layers = digits.chunks(width * height);

    let count = |layer: &[char], digit| layer.iter().filter(|&d| d == digit).count();

    let min_zero = layers.min_by_key(|l| count(l, &'0'));

    min_zero.map(|l| count(l, &'1') * count(l, &'2')).unwrap()
}

pub fn part2(input: &str) -> Vec<String> {
    let (width, height) = (25, 6);
    let digits = input.trim().chars().collect::<Vec<_>>();
    let layers = digits.chunks(width * height);

    let image = layers.fold(vec!['2'; width * height], |result, layer| {
        let pairs = result.iter().zip(layer.iter());
        pairs.map(|(&r, &l)| if r == '2' { l } else { r }).collect()
    });

    image
        .chunks(width)
        .map(|row| {
            row.iter()
                .map(|&c| if c == '0' { ' ' } else { '█' })
                .collect()
        })
        .collect()
}

#[cfg(test)]
mod spec {
    use super::*;

    #[test]
    fn part1_my_input() {
        assert_eq!(part1(INPUT), 2210);
    }

    #[test]
    fn part2_my_input() {
        assert_eq!(
            part2(INPUT),
            [
                " ██   ██  ████  ██  ████ ",
                "█  █ █  █ █    █  █ █    ",
                "█    █    ███  █    ███  ",
                "█    █ ██ █    █ ██ █    ",
                "█  █ █  █ █    █  █ █    ",
                " ██   ███ ████  ███ ████ "
            ]
        );
    }
}
