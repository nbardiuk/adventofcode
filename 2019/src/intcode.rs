pub fn execute(program: &mut [i32], input: &[i32]) -> Vec<i32> {
    let mut input = Vec::from(input);
    let mut output = vec![];
    iterate(program, || input.remove(0), |v| output.push(v));
    output
}

pub fn iterate<IN, OUT>(program: &mut [i32], mut pull: IN, mut push: OUT)
where
    IN: FnMut() -> i32,
    OUT: FnMut(i32) -> (),
{
    let mut pointer = 0;
    loop {
        let operation = program[pointer];

        let modes = operation / 100;
        let modes = [modes % 10, modes / 10];
        let arg_in = |index: usize| {
            let ptr = program[pointer + index];
            if modes[index - 1] == 0 {
                program[ptr as usize]
            } else {
                ptr
            }
        };
        let arg_out = |index: usize| program[pointer + index] as usize;

        match operation % 100 {
            1 => {
                let out = arg_out(3);
                program[out] = arg_in(1) + arg_in(2);
                pointer += 4;
            }
            2 => {
                let out = arg_out(3);
                program[out] = arg_in(1) * arg_in(2);
                pointer += 4;
            }
            3 => {
                let out = arg_out(1);
                program[out] = pull();
                pointer += 2;
            }
            4 => {
                push(arg_in(1));
                pointer += 2;
            }
            5 => {
                pointer = if arg_in(1) != 0 {
                    arg_in(2) as usize
                } else {
                    pointer + 3
                };
            }
            6 => {
                pointer = if arg_in(1) == 0 {
                    arg_in(2) as usize
                } else {
                    pointer + 3
                };
            }
            7 => {
                let out = arg_out(3);
                program[out] = if arg_in(1) < arg_in(2) { 1 } else { 0 };
                pointer += 4;
            }
            8 => {
                let out = arg_out(3);
                program[out] = if arg_in(1) == arg_in(2) { 1 } else { 0 };
                pointer += 4;
            }
            _ => return,
        };
    }
}

pub fn parse(text: &str) -> Vec<i32> {
    text.trim_end()
        .split(',')
        .filter_map(|n| n.parse().ok())
        .collect()
}

#[cfg(test)]
mod spec {
    use super::*;

    fn memory(program: &mut [i32]) -> &[i32] {
        execute(program, &[]);
        program
    }

    #[test]
    fn addition() {
        assert_eq!(memory(&mut [1, 5, 5, 3, 99, 1]), [1, 5, 5, 2, 99, 1]);
        assert_eq!(memory(&mut [101, 1, 5, 3, 99, 1]), [101, 1, 5, 2, 99, 1]);
        assert_eq!(memory(&mut [1001, 5, 1, 3, 99, 1]), [1001, 5, 1, 2, 99, 1]);
        assert_eq!(memory(&mut [1101, 1, 1, 3, 99]), [1101, 1, 1, 2, 99]);
        assert_eq!(memory(&mut [1101, 100, -1, 4, 0]), [1101, 100, -1, 4, 99]);
    }

    #[test]
    fn multiplication() {
        assert_eq!(memory(&mut [2, 5, 0, 3, 99, 3]), [2, 5, 0, 6, 99, 3]);
        assert_eq!(memory(&mut [102, 3, 5, 3, 99, 2]), [102, 3, 5, 6, 99, 2]);
        assert_eq!(memory(&mut [1002, 5, 3, 5, 99, 3]), [1002, 5, 3, 5, 99, 9]);
        assert_eq!(memory(&mut [1102, 3, 4, 3, 99]), [1102, 3, 4, 12, 99]);
    }

    #[test]
    fn input() {
        let mut program = [3, 1, 99];
        execute(&mut program, &[2]);
        assert_eq!(program, [3, 2, 99]);

        let mut program = [3, 0, 99];
        execute(&mut program, &[-10]);
        assert_eq!(program, [-10, 0, 99]);
    }

    #[test]
    fn multiple_inputs() {
        let mut program = [3, 1, 3, 3, 3, 5, 99];
        execute(&mut program, &[11, 22, 33]);
        assert_eq!(program, [3, 11, 3, 22, 3, 33, 99]);
    }

    #[test]
    fn output() {
        assert_eq!(execute(&mut [4, 2, 104, -29, 99], &[]), [104, -29]);
    }

    #[test]
    fn jump_if_true() {
        assert_eq!(execute(&mut [5, 7, 8, 99, 104, 1, 99, 0, 4], &[]), []);
        assert_eq!(execute(&mut [5, 7, 8, 99, 104, 1, 99, 1, 4], &[]), [1]);
        assert_eq!(execute(&mut [1005, 7, 4, 99, 104, 2, 99, 0], &[]), []);
        assert_eq!(execute(&mut [1005, 7, 4, 99, 104, 2, 99, 1], &[]), [2]);
        assert_eq!(execute(&mut [1105, 0, 4, 99, 104, 3, 99], &[]), []);
        assert_eq!(execute(&mut [1105, 1, 4, 99, 104, 3, 99], &[]), [3]);
    }

    #[test]
    fn jump_if_false() {
        assert_eq!(execute(&mut [6, 7, 8, 99, 104, 1, 99, 0, 4], &[]), [1]);
        assert_eq!(execute(&mut [6, 7, 8, 99, 104, 1, 99, 1, 4], &[]), []);
        assert_eq!(execute(&mut [1006, 7, 4, 99, 104, 2, 99, 0], &[]), [2]);
        assert_eq!(execute(&mut [1006, 7, 4, 99, 104, 2, 99, 1], &[]), []);
        assert_eq!(execute(&mut [1106, 0, 4, 99, 104, 3, 99], &[]), [3]);
        assert_eq!(execute(&mut [1106, 1, 4, 99, 104, 3, 99], &[]), []);
    }

    #[test]
    fn less_than() {
        assert_eq!(execute(&mut [1107, 1, 2, 5, 104, -9, 99], &[]), [1]);
        assert_eq!(execute(&mut [1107, 2, 1, 5, 104, -9, 99], &[]), [0]);
        assert_eq!(execute(&mut [1007, 1, 7, 5, 104, -9, 99, 2], &[]), [1]);
        assert_eq!(execute(&mut [1007, 2, 7, 5, 104, -9, 99, 1], &[]), [0]);
        assert_eq!(execute(&mut [7, 7, 8, 5, 104, -9, 99, 1, 2], &[]), [1]);
        assert_eq!(execute(&mut [7, 7, 8, 5, 104, -9, 99, 2, 1], &[]), [0]);
    }

    #[test]
    fn equals() {
        assert_eq!(execute(&mut [1108, 3, 3, 5, 104, -9, 99], &[]), [1]);
        assert_eq!(execute(&mut [1108, 1, 3, 5, 104, -9, 99], &[]), [0]);
        assert_eq!(execute(&mut [1008, 7, 3, 5, 104, -9, 99, 3], &[]), [1]);
        assert_eq!(execute(&mut [1008, 7, 1, 5, 104, -9, 99, 3], &[]), [0]);
        assert_eq!(execute(&mut [8, 7, 8, 5, 104, -9, 99, 3, 3], &[]), [1]);
        assert_eq!(execute(&mut [8, 7, 8, 5, 104, -9, 99, 1, 3], &[]), [0]);
    }

    #[test]
    fn overriding_instruction() {
        assert_eq!(
            memory(&mut [1, 1, 1, 4, 99, 5, 6, 0, 99]),
            [30, 1, 1, 4, 2, 5, 6, 0, 99]
        );
    }

    #[test]
    fn day2_example_prograp() {
        assert_eq!(
            memory(&mut [1, 9, 10, 3, 2, 3, 11, 0, 99, 30, 40, 50]),
            [3500, 9, 10, 70, 2, 3, 11, 0, 99, 30, 40, 50]
        );
    }

    #[test]
    fn example_equals_position_mode() {
        let program = || [3, 9, 8, 9, 10, 9, 4, 9, 99, -1, 8];
        assert_eq!(execute(&mut program(), &[8],), [1]);
        assert_eq!(execute(&mut program(), &[9],), [0]);
    }

    #[test]
    fn example_equals_immediate_mode() {
        assert_eq!(execute(&mut [3, 3, 1108, -1, 8, 3, 4, 3, 99], &[8]), [1]);
        assert_eq!(execute(&mut [3, 3, 1108, -1, 8, 3, 4, 3, 99], &[9]), [0]);
    }

    #[test]
    fn example_less_than_position_mode() {
        let program = || [3, 9, 7, 9, 10, 9, 4, 9, 99, -1, 8];
        assert_eq!(execute(&mut program(), &[7],), [1]);
        assert_eq!(execute(&mut program(), &[8],), [0]);
    }

    #[test]
    fn example_less_than_immediate_mode() {
        assert_eq!(execute(&mut [3, 3, 1107, -1, 8, 3, 4, 3, 99], &[7]), [1]);
        assert_eq!(execute(&mut [3, 3, 1107, -1, 8, 3, 4, 3, 99], &[8]), [0]);
    }

    #[test]
    fn example_jumps_position_mode() {
        let program = || [3, 12, 6, 12, 15, 1, 13, 14, 13, 4, 13, 99, -1, 0, 1, 9];
        assert_eq!(execute(&mut program(), &[0],), [0]);
        assert_eq!(execute(&mut program(), &[2],), [1]);
    }

    #[test]
    fn example_jumps_immediate_mode() {
        let program = || [3, 3, 1105, -1, 9, 1101, 0, 0, 12, 4, 12, 99, 1];
        assert_eq!(execute(&mut program(), &[0],), [0]);
        assert_eq!(execute(&mut program(), &[2],), [1]);
    }

    #[test]
    fn passes_diagnostic_tests() {
        let output = execute(&mut parse(crate::day05::INPUT), &[1]);
        assert_eq!(output[0..output.len() - 1], [0, 0, 0, 0, 0, 0, 0, 0, 0]);
    }

    #[test]
    fn example_larger() {
        let program = || {
            [
                3, 21, 1008, 21, 8, 20, 1005, 20, 22, 107, 8, 21, 20, 1006, 20, 31, 1106, 0, 36,
                98, 0, 0, 1002, 21, 125, 20, 4, 20, 1105, 1, 46, 104, 999, 1105, 1, 46, 1101, 1000,
                1, 20, 4, 20, 1105, 1, 46, 98, 99,
            ]
        };

        assert_eq!(execute(&mut program(), &[7],), [999]);
        assert_eq!(execute(&mut program(), &[8],), [1000]);
        assert_eq!(execute(&mut program(), &[9],), [1001]);
    }
}
