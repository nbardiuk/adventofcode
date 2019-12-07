use crate::intcode;
use itertools::Itertools;
use std::sync::mpsc::channel;
use std::thread;

pub const INPUT: &str = include_str!("../res/day07.txt");

pub fn part1(input: &str) -> i32 {
    let program = intcode::parse(input);
    (0..5)
        .permutations(5)
        .map(|settings| {
            settings.into_iter().fold(vec![0], |output, setting| {
                intcode::execute(&mut program.clone(), &[setting, output[0]])
            })[0]
        })
        .max()
        .unwrap()
}

pub fn part2(input: &str) -> i32 {
    let code = intcode::parse(input);
    (5..10)
        .permutations(5)
        .map(|settings| {
            let (a_sender, b_reciever) = channel();
            let (b_sender, c_reciever) = channel();
            let (c_sender, d_reciever) = channel();
            let (d_sender, e_reciever) = channel();
            let (e_sender, a_reciever) = channel();
            let (result_sender, result_reciever) = channel();

            e_sender.send(settings[0]).unwrap();
            e_sender.send(0).unwrap();
            a_sender.send(settings[1]).unwrap();
            b_sender.send(settings[2]).unwrap();
            c_sender.send(settings[3]).unwrap();
            d_sender.send(settings[4]).unwrap();

            let mut acode = code.clone();
            thread::spawn(move || {
                intcode::iterate(
                    &mut acode,
                    || a_reciever.recv().unwrap(),
                    |v| a_sender.send(v).expect("a could not send"),
                );
            });

            let mut bcode = code.clone();
            thread::spawn(move || {
                intcode::iterate(
                    &mut bcode,
                    || b_reciever.recv().unwrap(),
                    |v| b_sender.send(v).expect("b could not send"),
                );
            });

            let mut ccode = code.clone();
            thread::spawn(move || {
                intcode::iterate(
                    &mut ccode,
                    || c_reciever.recv().unwrap(),
                    |v| c_sender.send(v).expect("c could not send"),
                );
            });

            let mut dcode = code.clone();
            thread::spawn(move || {
                intcode::iterate(
                    &mut dcode,
                    || d_reciever.recv().unwrap(),
                    |v| d_sender.send(v).expect("d could not send"),
                );
            });

            let mut ecode = code.clone();
            thread::spawn(move || {
                intcode::iterate(
                    &mut ecode,
                    || e_reciever.recv().unwrap(),
                    |v| {
                        if e_sender.send(v).is_err() {}
                        result_sender.send(v).expect("result could not send")
                    },
                );
            });

            result_reciever.into_iter().last().unwrap()
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
