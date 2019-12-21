use crate::intcode::Computer;
pub const INPUT: &str = include_str!("../res/day21.txt");

pub fn part1(input: &str) -> i64 {
    // any hole in [ A, C ]
    // and D is ground
    //
    // !(a && b) && d
    //
    let program = "\
OR A J
AND C J
NOT J J
AND D J
WALK
";
    survey_hull(input, program)
}

pub fn part2(input: &str) -> i64 {
    // any hole in [ A, B, C ]
    // and D is ground
    // and any is ground in [ H, E ]
    //
    // !(a && b && c) && d && (h || e)
    //
    let program = "\
OR A J
AND B J
AND C J
NOT J J
AND D J
OR H T
OR E T
AND T J
RUN
";
    survey_hull(input, program)
}

fn survey_hull(input: &str, program: &str) -> i64 {
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
