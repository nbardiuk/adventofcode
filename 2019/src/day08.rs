pub const INPUT: &str = include_str!("../res/day08.txt");

pub fn part1(input: &str) -> usize {
    let digits = read_digits(input);
    let layers = split_layers(&digits, 25, 6);

    let count = |layer: &[u8], digit| layer.iter().filter(|&d| d == digit).count();

    let with_counts = layers.iter().zip(layers.iter().map(|l| count(l, &0)));

    let min = with_counts
        .min_by(|(_, c1), (_, c2)| c1.cmp(c2))
        .map(|(l, _)| l);

    min.map(|l| count(l, &1) * count(l, &2)).unwrap()
}
pub fn part2(input: &str) -> Vec<u8> {
    let digits = read_digits(input);
    let layers = split_layers(&digits, 25, 6);
    merge(&layers, 25, 6)
}

fn merge(layers: &[&[u8]], width: usize, height: usize) -> Vec<u8> {
    layers
        .iter()
        .fold(vec![2; width * height], |result, layer| {
            let pairs = result.iter().zip(layer.iter());
            pairs.map(|(&r, &l)| if r == 2 { l } else { r }).collect()
        })
}

fn split_layers(digits: &[u8], width: usize, height: usize) -> Vec<&[u8]> {
    digits.chunks(width * height).collect()
}

fn read_digits(text: &str) -> Vec<u8> {
    text.chars()
        .filter_map(|c| Some(c.to_digit(10)? as u8))
        .collect()
}

#[cfg(test)]
mod spec {
    use super::*;

    #[test]
    fn reader() {
        assert_eq!(
            read_digits("0123456789012"),
            [0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 0, 1, 2]
        );
    }

    #[test]
    fn splitting_layers() {
        assert_eq!(
            split_layers(&[1, 2, 3, 4, 5, 6, 7, 8, 9, 0, 1, 2], 3, 2),
            [[1, 2, 3, 4, 5, 6], [7, 8, 9, 0, 1, 2]]
        );
    }

    #[test]
    fn part1_my_input() {
        assert_eq!(part1(INPUT), 2210);
    }

    #[test]
    fn merging_layers() {
        assert_eq!(
            merge(
                &[
                    &[0, 2, 2, 2], //
                    &[1, 1, 2, 2], //
                    &[2, 2, 1, 2], //
                    &[0, 0, 0, 0]
                ],
                2,
                2
            ),
            [0, 1, 1, 0]
        );
    }
    #[test]
    fn part2_my_input() {
        assert_eq!(
            part2(INPUT),
            vec![
                // CGEGE
                0, 1, 1, 0, 0, 0, 1, 1, 0, 0, 1, 1, 1, 1, 0, 0, 1, 1, 0, 0, 1, 1, 1, 1, 0, //
                1, 0, 0, 1, 0, 1, 0, 0, 1, 0, 1, 0, 0, 0, 0, 1, 0, 0, 1, 0, 1, 0, 0, 0, 0, //
                1, 0, 0, 0, 0, 1, 0, 0, 0, 0, 1, 1, 1, 0, 0, 1, 0, 0, 0, 0, 1, 1, 1, 0, 0, //
                1, 0, 0, 0, 0, 1, 0, 1, 1, 0, 1, 0, 0, 0, 0, 1, 0, 1, 1, 0, 1, 0, 0, 0, 0, //
                1, 0, 0, 1, 0, 1, 0, 0, 1, 0, 1, 0, 0, 0, 0, 1, 0, 0, 1, 0, 1, 0, 0, 0, 0, //
                0, 1, 1, 0, 0, 0, 1, 1, 1, 0, 1, 1, 1, 1, 0, 0, 1, 1, 1, 0, 1, 1, 1, 1, 0
            ]
        );
    }
}
