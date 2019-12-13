use crate::intcode::Computer;
use itertools::Itertools;

pub const INPUT: &str = include_str!("../res/day07.txt");

pub fn part1(input: &str) -> i64 {
    let computer = Computer::parse(input);
    (0..5)
        .permutations(5)
        .map(|settings| {
            settings.into_iter().fold(0, |output, setting| {
                computer.clone().call(vec![setting, output])
            })
        })
        .max()
        .unwrap()
}

pub fn part2(input: &str) -> i64 {
    let computer = Computer::parse(input);
    (5..10)
        .permutations(5)
        .map(|settings| {
            let mut comp_a = computer.clone();
            let mut comp_b = computer.clone();
            let mut comp_c = computer.clone();
            let mut comp_d = computer.clone();
            let mut comp_e = computer.clone();

            let mut e2a = vec![settings[0], 0];
            let mut a2b = vec![settings[1]];
            let mut b2c = vec![settings[2]];
            let mut c2d = vec![settings[3]];
            let mut d2e = vec![settings[4]];
            loop {
                a2b.append(&mut comp_a.process(&mut e2a));
                b2c.append(&mut comp_b.process(&mut a2b));
                c2d.append(&mut comp_c.process(&mut b2c));
                d2e.append(&mut comp_d.process(&mut c2d));
                e2a.append(&mut comp_e.process(&mut d2e));

                if comp_e.halted {
                    return e2a.last().cloned().unwrap();
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
