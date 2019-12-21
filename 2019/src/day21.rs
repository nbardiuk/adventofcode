use crate::intcode::Computer;
pub const INPUT: &str = include_str!("../res/day21.txt");

pub fn part1(input: &str) -> i64 {
    // Jump if D is ground
    //   and there is any hole in A or B or C
    let program = "\
NOT A J
NOT B T
OR T J
NOT C T
OR T J
AND D J
WALK
";
    Computer::parse(input).call(program.chars().map(|c| (c as u8) as i64).collect())
}

pub fn part2(input: &str) -> i64 {
    // Jumpt if D is ground
    //   and there is any hole in A or B or C
    //   and (H is ground or (H is hole and E is ground))
    let program = "\
NOT A J
NOT B T
OR T J
NOT C T
OR T J
NOT H T
AND E T
OR H T
AND T J
AND D J
RUN
";
    Computer::parse(input).call(program.chars().map(|c| (c as u8) as i64).collect())
}

#[cfg(test)]
mod spec {
    use super::*;

    #[test]
    fn part1_my_input() {
        assert_eq!(part1(INPUT), 19358416);
    }

    #[test]
    fn part2_my_input() {
        assert_eq!(part2(INPUT), 1144641747);
    }
}
