pub const INPUT: &str = include_str!("../res/day05.txt");

pub fn part1(input: &str) -> i32 {
    let mut output = vec![];
    execute(numbers(input), &[1], &mut output);
    output[output.len() - 1]
}

fn numbers(text: &str) -> Vec<i32> {
    text.split(',').filter_map(|num| num.parse().ok()).collect()
}

fn execute(mut memory: Vec<i32>, input: &[i32], output: &mut Vec<i32>) -> Vec<i32> {
    let mut pointer = 0;
    let mut input_index = 0;
    loop {
        let operation = memory[pointer];

        let modes = operation / 100;
        let modes = [0, modes % 10, modes / 10];
        let arg = |index: usize| {
            let ptr = memory[pointer + index];
            if modes[index] == 0 {
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
    fn overriding_instruction() {
        let result = execute(vec![1, 1, 1, 4, 99, 5, 6, 0, 99], &[], &mut vec![]);
        assert_eq!(result, [30, 1, 1, 4, 2, 5, 6, 0, 99]);
    }

    #[test]
    fn initial_example() {
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
    fn part1_my_input() {
        assert_eq!(part1(INPUT), 13346482);
    }
}
