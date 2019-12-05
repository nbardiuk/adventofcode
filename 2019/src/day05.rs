pub const INPUT: &str = include_str!("../res/day05.txt");

pub fn part1(input: &str) -> i32 {
    let mut output = vec![];
    execute(numbers(input), &[1], &mut output);
    output[output.len() - 1]
}

pub fn part2(input: &str) -> i32 {
    let mut output = vec![];
    execute(numbers(input), &[5], &mut output);
    output[output.len() - 1]
}

fn numbers(text: &str) -> Vec<i32> {
    text.trim()
        .split(',')
        .filter_map(|num| num.trim().parse().ok())
        .collect()
}

fn execute(mut memory: Vec<i32>, input: &[i32], output: &mut Vec<i32>) -> Vec<i32> {
    let mut pointer = 0;
    let mut input_index = 0;
    loop {
        let operation = memory[pointer];

        let modes = operation / 100;
        let modes = [modes % 10, modes / 10];
        let arg = |index: usize| {
            let ptr = memory[pointer + index];
            if modes[index - 1] == 0 {
                memory[ptr as usize]
            } else {
                ptr
            }
        };

        let opcode = operation % 100;
        match opcode {
            1 => {
                let out = memory[pointer + 3] as usize;
                memory[out] = arg(1) + arg(2);
                pointer += 4;
            }
            2 => {
                let out = memory[pointer + 3] as usize;
                memory[out] = arg(1) * arg(2);
                pointer += 4;
            }
            3 => {
                let out = memory[pointer + 1] as usize;
                memory[out] = input[input_index];
                input_index += 1;
                pointer += 2;
            }
            4 => {
                output.push(arg(1));
                pointer += 2;
            }
            5 => {
                pointer = if arg(1) != 0 {
                    arg(2) as usize
                } else {
                    pointer + 3
                };
            }
            6 => {
                pointer = if arg(1) == 0 {
                    arg(2) as usize
                } else {
                    pointer + 3
                };
            }
            7 => {
                let out = memory[pointer + 3] as usize;
                memory[out] = if arg(1) < arg(2) { 1 } else { 0 };
                pointer += 4;
            }
            8 => {
                let out = memory[pointer + 3] as usize;
                memory[out] = if arg(1) == arg(2) { 1 } else { 0 };
                pointer += 4;
            }
            _ => break,
        };
    }
    memory
}

#[cfg(test)]
mod spec {
    use super::*;

    #[test]
    fn addition() {
        assert_eq!(
            execute(vec![1, 5, 5, 3, 99, 1], &[], &mut vec!()),
            [1, 5, 5, 2, 99, 1]
        );
        assert_eq!(
            execute(vec![101, 1, 5, 3, 99, 1], &[], &mut vec!()),
            [101, 1, 5, 2, 99, 1]
        );
        assert_eq!(
            execute(vec![1001, 5, 1, 3, 99, 1], &[], &mut vec!()),
            [1001, 5, 1, 2, 99, 1]
        );
        assert_eq!(
            execute(vec![1101, 1, 1, 3, 99], &[], &mut vec!()),
            [1101, 1, 1, 2, 99]
        );
        assert_eq!(
            execute(vec![1101, 100, -1, 4, 0], &[], &mut vec!()),
            [1101, 100, -1, 4, 99]
        );
    }

    #[test]
    fn multiplication() {
        assert_eq!(
            execute(vec![2, 5, 0, 3, 99, 3], &[], &mut vec!()),
            [2, 5, 0, 6, 99, 3]
        );
        assert_eq!(
            execute(vec![102, 3, 5, 3, 99, 2], &[], &mut vec!()),
            [102, 3, 5, 6, 99, 2]
        );
        assert_eq!(
            execute(vec![1002, 5, 3, 5, 99, 3], &[], &mut vec!()),
            [1002, 5, 3, 5, 99, 9]
        );
        assert_eq!(
            execute(vec![1102, 3, 4, 3, 99], &[], &mut vec!()),
            [1102, 3, 4, 12, 99]
        );
    }

    #[test]
    fn input() {
        assert_eq!(execute(vec![3, 1, 99], &[2], &mut vec!()), [3, 2, 99]);
        assert_eq!(execute(vec![3, 0, 99], &[-10], &mut vec!()), [-10, 0, 99]);
    }

    #[test]
    fn output() {
        let mut output = vec![];
        execute(vec![4, 2, 104, -29, 99], &[], &mut output);
        assert_eq!(output, [104, -29]);
    }

    #[test]
    fn jump_if_true() {
        let mut output = vec![];
        execute(vec![5, 7, 8, 99, 104, 1, 99, 0, 4], &[], &mut output);
        assert_eq!(output, []);

        let mut output = vec![];
        execute(vec![5, 7, 8, 99, 104, 1, 99, 1, 4], &[], &mut output);
        assert_eq!(output, [1]);

        let mut output = vec![];
        execute(vec![1005, 7, 4, 99, 104, 2, 99, 0], &[], &mut output);
        assert_eq!(output, []);

        let mut output = vec![];
        execute(vec![1005, 7, 4, 99, 104, 2, 99, 1], &[], &mut output);
        assert_eq!(output, [2]);

        let mut output = vec![];
        execute(vec![1105, 0, 4, 99, 104, 3, 99], &[], &mut output);
        assert_eq!(output, []);

        let mut output = vec![];
        execute(vec![1105, 1, 4, 99, 104, 3, 99], &[], &mut output);
        assert_eq!(output, [3]);
    }

    #[test]
    fn jump_if_false() {
        let mut output = vec![];
        execute(vec![6, 7, 8, 99, 104, 1, 99, 0, 4], &[], &mut output);
        assert_eq!(output, [1]);

        let mut output = vec![];
        execute(vec![6, 7, 8, 99, 104, 1, 99, 1, 4], &[], &mut output);
        assert_eq!(output, []);

        let mut output = vec![];
        execute(vec![1006, 7, 4, 99, 104, 2, 99, 0], &[], &mut output);
        assert_eq!(output, [2]);

        let mut output = vec![];
        execute(vec![1006, 7, 4, 99, 104, 2, 99, 1], &[], &mut output);
        assert_eq!(output, []);

        let mut output = vec![];
        execute(vec![1106, 0, 4, 99, 104, 3, 99], &[], &mut output);
        assert_eq!(output, [3]);

        let mut output = vec![];
        execute(vec![1106, 1, 4, 99, 104, 3, 99], &[], &mut output);
        assert_eq!(output, []);
    }

    #[test]
    fn less_than() {
        let mut output = vec![];
        execute(vec![1107, 1, 2, 5, 104, -9, 99], &[], &mut output);
        assert_eq!(output, [1]);

        let mut output = vec![];
        execute(vec![1107, 2, 1, 5, 104, -9, 99], &[], &mut output);
        assert_eq!(output, [0]);

        let mut output = vec![];
        execute(vec![1007, 1, 7, 5, 104, -9, 99, 2], &[], &mut output);
        assert_eq!(output, [1]);

        let mut output = vec![];
        execute(vec![1007, 2, 7, 5, 104, -9, 99, 1], &[], &mut output);
        assert_eq!(output, [0]);

        let mut output = vec![];
        execute(vec![7, 7, 8, 5, 104, -9, 99, 1, 2], &[], &mut output);
        assert_eq!(output, [1]);

        let mut output = vec![];
        execute(vec![7, 7, 8, 5, 104, -9, 99, 2, 1], &[], &mut output);
        assert_eq!(output, [0]);
    }

    #[test]
    fn equals() {
        let mut output = vec![];
        execute(vec![1108, 3, 3, 5, 104, -9, 99], &[], &mut output);
        assert_eq!(output, [1]);

        let mut output = vec![];
        execute(vec![1108, 1, 3, 5, 104, -9, 99], &[], &mut output);
        assert_eq!(output, [0]);

        let mut output = vec![];
        execute(vec![1008, 7, 3, 5, 104, -9, 99, 3], &[], &mut output);
        assert_eq!(output, [1]);

        let mut output = vec![];
        execute(vec![1008, 7, 1, 5, 104, -9, 99, 3], &[], &mut output);
        assert_eq!(output, [0]);

        let mut output = vec![];
        execute(vec![8, 7, 8, 5, 104, -9, 99, 3, 3], &[], &mut output);
        assert_eq!(output, [1]);

        let mut output = vec![];
        execute(vec![8, 7, 8, 5, 104, -9, 99, 1, 3], &[], &mut output);
        assert_eq!(output, [0]);
    }

    #[test]
    fn overriding_instruction() {
        let result = execute(vec![1, 1, 1, 4, 99, 5, 6, 0, 99], &[], &mut vec![]);
        assert_eq!(result, [30, 1, 1, 4, 2, 5, 6, 0, 99]);
    }

    #[test]
    fn day2_example_prograp() {
        let result = execute(
            vec![1, 9, 10, 3, 2, 3, 11, 0, 99, 30, 40, 50],
            &[],
            &mut vec![],
        );
        assert_eq!(result, [3500, 9, 10, 70, 2, 3, 11, 0, 99, 30, 40, 50]);
    }

    #[test]
    fn passes_diagnostic_tests() {
        let mut output = vec![];
        execute(numbers(INPUT), &[1], &mut output);
        assert_eq!(output[0..output.len() - 1], [0, 0, 0, 0, 0, 0, 0, 0, 0]);
    }

    #[test]
    fn example_equals_position_mode() {
        let mut output = vec![];
        execute(vec![3, 9, 8, 9, 10, 9, 4, 9, 99, -1, 8], &[8], &mut output);
        assert_eq!(output, [1]);

        let mut output = vec![];
        execute(vec![3, 9, 8, 9, 10, 9, 4, 9, 99, -1, 8], &[9], &mut output);
        assert_eq!(output, [0]);
    }

    #[test]
    fn example_equals_immediate_mode() {
        let mut output = vec![];
        execute(vec![3, 3, 1108, -1, 8, 3, 4, 3, 99], &[8], &mut output);
        assert_eq!(output, [1]);

        let mut output = vec![];
        execute(vec![3, 3, 1108, -1, 8, 3, 4, 3, 99], &[9], &mut output);
        assert_eq!(output, [0]);
    }

    #[test]
    fn example_less_than_position_mode() {
        let mut output = vec![];
        execute(vec![3, 9, 7, 9, 10, 9, 4, 9, 99, -1, 8], &[7], &mut output);
        assert_eq!(output, [1]);

        let mut output = vec![];
        execute(vec![3, 9, 7, 9, 10, 9, 4, 9, 99, -1, 8], &[8], &mut output);
        assert_eq!(output, [0]);
    }

    #[test]
    fn example_less_than_immediate_mode() {
        let mut output = vec![];
        execute(vec![3, 3, 1107, -1, 8, 3, 4, 3, 99], &[7], &mut output);
        assert_eq!(output, [1]);

        let mut output = vec![];
        execute(vec![3, 3, 1107, -1, 8, 3, 4, 3, 99], &[8], &mut output);
        assert_eq!(output, [0]);
    }

    #[test]
    fn example_jumps_position_mode() {
        let mut output = vec![];
        execute(
            vec![3, 12, 6, 12, 15, 1, 13, 14, 13, 4, 13, 99, -1, 0, 1, 9],
            &[0],
            &mut output,
        );
        assert_eq!(output, [0]);

        let mut output = vec![];
        execute(
            vec![3, 12, 6, 12, 15, 1, 13, 14, 13, 4, 13, 99, -1, 0, 1, 9],
            &[2],
            &mut output,
        );
        assert_eq!(output, [1]);
    }

    #[test]
    fn example_jumps_immediate_mode() {
        let mut output = vec![];
        execute(
            vec![3, 3, 1105, -1, 9, 1101, 0, 0, 12, 4, 12, 99, 1],
            &[0],
            &mut output,
        );
        assert_eq!(output, [0]);

        let mut output = vec![];
        execute(
            vec![3, 3, 1105, -1, 9, 1101, 0, 0, 12, 4, 12, 99, 1],
            &[2],
            &mut output,
        );
        assert_eq!(output, [1]);
    }

    #[test]
    fn example_larger() {
        let mut output = vec![];
        execute(
            vec![
                3, 21, 1008, 21, 8, 20, 1005, 20, 22, 107, 8, 21, 20, 1006, 20, 31, 1106, 0, 36,
                98, 0, 0, 1002, 21, 125, 20, 4, 20, 1105, 1, 46, 104, 999, 1105, 1, 46, 1101, 1000,
                1, 20, 4, 20, 1105, 1, 46, 98, 99,
            ],
            &[7],
            &mut output,
        );
        assert_eq!(output, [999]);

        let mut output = vec![];
        execute(
            vec![
                3, 21, 1008, 21, 8, 20, 1005, 20, 22, 107, 8, 21, 20, 1006, 20, 31, 1106, 0, 36,
                98, 0, 0, 1002, 21, 125, 20, 4, 20, 1105, 1, 46, 104, 999, 1105, 1, 46, 1101, 1000,
                1, 20, 4, 20, 1105, 1, 46, 98, 99,
            ],
            &[8],
            &mut output,
        );
        assert_eq!(output, [1000]);

        let mut output = vec![];
        execute(
            vec![
                3, 21, 1008, 21, 8, 20, 1005, 20, 22, 107, 8, 21, 20, 1006, 20, 31, 1106, 0, 36,
                98, 0, 0, 1002, 21, 125, 20, 4, 20, 1105, 1, 46, 104, 999, 1105, 1, 46, 1101, 1000,
                1, 20, 4, 20, 1105, 1, 46, 98, 99,
            ],
            &[9],
            &mut output,
        );
        assert_eq!(output, [1001]);
    }

    #[test]
    fn part1_my_input() {
        assert_eq!(part1(INPUT), 13346482);
    }

    #[test]
    fn part2_my_input() {
        assert_eq!(part2(INPUT), 12111395);
    }
}
