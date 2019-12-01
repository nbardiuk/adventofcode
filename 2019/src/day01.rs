fn part1(input: &str) -> u32 {
    numbers(input).into_iter().map(fuel_simple).sum()
}

fn part2(input: &str) -> u32 {
    numbers(input).into_iter().map(fuel_self_lifting).sum()
}

fn numbers(text: &str) -> Vec<u32> {
    text.lines().filter_map(|l| l.parse::<u32>().ok()).collect()
}

fn fuel_simple(mass: u32) -> u32 {
    (mass / 3).saturating_sub(2)
}

fn fuel_self_lifting(mass: u32) -> u32 {
    match fuel_simple(mass) {
        0 => 0,
        fuel => fuel + fuel_self_lifting(fuel),
    }
}

#[cfg(test)]
mod spec {
    use super::*;
    use std::fs::read_to_string;
    use std::path::Path;

    fn my_input() -> String {
        let day = Path::new(file!()).file_stem().unwrap().to_string_lossy();
        read_to_string(format!("./res/{}.txt", day)).expect("Cannot read input file")
    }

    #[test]
    fn fuel_part1() {
        assert_eq!(fuel_simple(12), 2);
        assert_eq!(fuel_simple(14), 2);
        assert_eq!(fuel_simple(1969), 654);
        assert_eq!(fuel_simple(100756), 33583);
    }

    #[test]
    fn part1_my_input() {
        assert_eq!(part1(&my_input()), 3369286);
    }

    #[test]
    fn fuel_part2() {
        assert_eq!(fuel_self_lifting(12), 2);
        assert_eq!(fuel_self_lifting(1969), 966);
        assert_eq!(fuel_self_lifting(100756), 50346);
    }

    #[test]
    fn part2_my_input() {
        assert_eq!(part2(&my_input()), 5051054);
    }
}
