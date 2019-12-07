use crate::intcode::Program;
use itertools::Itertools;

pub const INPUT: &str = include_str!("../res/day07.txt");

pub fn part1(input: &str) -> i32 {
    let program = Program::parse(input);
    (0..5)
        .permutations(5)
        .map(|settings| {
            settings.into_iter().fold(vec![0], |output, setting| {
                program.clone().execute(vec![setting, output[0]]).output
            })[0]
        })
        .max()
        .unwrap()
}

pub fn part2(input: &str) -> i32 {
    let program = Program::parse(input);
    (5..10)
        .permutations(5)
        .map(|settings| {
            let mut prog_a = program.clone();
            let mut prog_b = program.clone();
            let mut prog_c = program.clone();
            let mut prog_d = program.clone();
            let mut prog_e = program.clone();

            let mut a_in = vec![settings[0], 0];
            let mut b_in = vec![settings[1]];
            let mut c_in = vec![settings[2]];
            let mut d_in = vec![settings[3]];
            let mut e_in = vec![settings[4]];
            loop {
                b_in.append(&mut prog_a.iteration(&mut a_in).flush_output());
                c_in.append(&mut prog_b.iteration(&mut b_in).flush_output());
                d_in.append(&mut prog_c.iteration(&mut c_in).flush_output());
                e_in.append(&mut prog_d.iteration(&mut d_in).flush_output());
                a_in.append(&mut prog_e.iteration(&mut e_in).flush_output());

                if prog_e.has_terminated() {
                    return a_in.last().cloned().unwrap();
                }
            }
        })
        .max()
        .unwrap()
}

#[cfg(test)]
mod spec {
    use super::*;

    #[test]
    fn part1_example_43210() {
        assert_eq!(
            part1("3,15,3,16,1002,16,10,16,1,16,15,15,4,15,99,0,0"),
            43210
        );
    }

    #[test]
    fn part1_example_01234() {
        assert_eq!(
            part1("3,23,3,24,1002,24,10,24,1002,23,-1,23,101,5,23,23,1,24,23,23,4,23,99,0,0"),
            54321
        );
    }

    #[test]
    fn part1_example_10432() {
        assert_eq!(
            part1("3,31,3,32,1002,32,10,32,1001,31,-2,31,1007,31,0,33,1002,33,7,33,1,33,31,31,1,32,31,31,4,31,99,0,0,0"),
            65210
        );
    }

    #[test]
    fn part1_my_input() {
        assert_eq!(part1(INPUT), 45730);
    }

    #[test]
    fn part2_example_98765() {
        assert_eq!(
            part2("3,26,1001,26,-4,26,3,27,1002,27,2,27,1,27,26,27,4,27,1001,28,-1,28,1005,28,6,99,0,0,5"),
            139629729
        );
    }

    #[test]
    fn part2_example_97865() {
        assert_eq!(
            part2("3,52,1001,52,-5,52,3,53,1,52,56,54,1007,54,5,55,1005,55,26,1001,54,-5,54,1105,1,12,1,53,54,53,1008,54,0,55,1001,55,1,55,2,53,55,53,4,53,1001,56,-1,56,1005,56,6,99,0,0,0,0,10"),
            18216
        );
    }

    #[test]
    fn part2_my_input() {
        assert_eq!(part2(INPUT), 5406484);
    }
}
