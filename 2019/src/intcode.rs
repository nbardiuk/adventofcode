#[derive(Clone)]
pub struct Program {
    pub memory: Vec<i64>,
    pub pointer: usize,
    pub relative_base: i64,
    pub output: Vec<i64>,
}

impl Program {
    pub fn new(code: &[i64]) -> Self {
        Program {
            memory: Vec::from(code),
            pointer: 0,
            relative_base: 0,
            output: vec![],
        }
    }

    pub fn call(mut self, mut input: Vec<i64>) -> i64 {
        self.iteration(&mut input);
        self.output.last().cloned().unwrap()
    }

    pub fn execute(mut self, mut input: Vec<i64>) -> Self {
        self.iteration(&mut input);
        self
    }

    fn mode(&self, i: usize) -> u8 {
        let modes = self.memory[self.pointer] / 100;
        match i {
            0 => (modes % 10) as u8,
            1 => (modes % 100 / 10) as u8,
            2 => (modes / 100) as u8,
            _ => panic!(format!("unexpected mode index {}", i)),
        }
    }

    fn read_memory(&self, ptr: usize) -> i64 {
        *self.memory.get(ptr).unwrap_or(&0)
    }

    fn arg(&self, index: usize) -> i64 {
        let ptr = self.memory[self.pointer + index];
        match self.mode(index - 1) {
            0 => self.read_memory(ptr as usize),
            1 => ptr,
            2 => self.read_memory((self.relative_base + ptr) as usize),
            mode => panic!(format!("unexpected reading mode {}", mode)),
        }
    }

    fn write(&mut self, index: usize, value: i64) {
        let ptr = self.memory[self.pointer + index];
        let i = match self.mode(index - 1) {
            0 => ptr as usize,
            2 => (self.relative_base + ptr) as usize,
            mode => panic!(format!("unexpected writing mode {}", mode)),
        };
        if i >= self.memory.len() {
            self.memory.resize(i + 1, 0);
        }
        self.memory[i] = value;
    }

    pub fn iteration<'a>(&'a mut self, input: &mut Vec<i64>) -> &'a mut Self {
        loop {
            match self.memory[self.pointer] % 100 {
                1 => {
                    self.write(3, self.arg(1) + self.arg(2));
                    self.pointer += 4;
                }
                2 => {
                    self.write(3, self.arg(1) * self.arg(2));
                    self.pointer += 4;
                }
                3 => {
                    if input.is_empty() {
                        return self;
                    }
                    self.write(1, input.remove(0));
                    self.pointer += 2;
                }
                4 => {
                    self.output.push(self.arg(1));
                    self.pointer += 2;
                }
                5 => {
                    if self.arg(1) != 0 {
                        self.pointer = self.arg(2) as usize;
                    } else {
                        self.pointer += 3;
                    };
                }
                6 => {
                    if self.arg(1) == 0 {
                        self.pointer = self.arg(2) as usize;
                    } else {
                        self.pointer += 3;
                    };
                }
                7 => {
                    self.write(3, if self.arg(1) < self.arg(2) { 1 } else { 0 });
                    self.pointer += 4;
                }
                8 => {
                    self.write(3, if self.arg(1) == self.arg(2) { 1 } else { 0 });
                    self.pointer += 4;
                }
                9 => {
                    self.relative_base += self.arg(1);
                    self.pointer += 2;
                }
                code => {
                    assert_eq!(code, 99);
                    return self;
                }
            };
        }
    }

    pub fn flush_output(&mut self) -> Vec<i64> {
        std::mem::replace(&mut self.output, vec![])
    }

    pub fn has_terminated(&self) -> bool {
        self.memory[self.pointer] == 99
    }

    pub fn parse(text: &str) -> Self {
        let code = text
            .trim_end()
            .split(',')
            .filter_map(|n| n.parse().ok())
            .collect::<Vec<_>>();
        Program::new(&code)
    }
}

#[cfg(test)]
mod spec {
    use super::*;

    fn memory(code: &[i64]) -> Vec<i64> {
        Program::new(code).execute(vec![]).memory
    }

    fn execute(code: &[i64], input: &[i64]) -> Program {
        Program::new(code).execute(Vec::from(input))
    }

    #[test]
    fn addition() {
        assert_eq!(memory(&[1, 5, 5, 3, 99, 1]), [1, 5, 5, 2, 99, 1]);
        assert_eq!(memory(&[101, 1, 5, 3, 99, 1]), [101, 1, 5, 2, 99, 1]);
        assert_eq!(memory(&[1001, 5, 1, 3, 99, 1]), [1001, 5, 1, 2, 99, 1]);
        assert_eq!(memory(&[1101, 1, 1, 3, 99]), [1101, 1, 1, 2, 99]);
        assert_eq!(memory(&[1101, 100, -1, 4, 0]), [1101, 100, -1, 4, 99]);
    }

    #[test]
    fn multiplication() {
        assert_eq!(memory(&[2, 5, 0, 3, 99, 3]), [2, 5, 0, 6, 99, 3]);
        assert_eq!(memory(&[102, 3, 5, 3, 99, 2]), [102, 3, 5, 6, 99, 2]);
        assert_eq!(memory(&[1002, 5, 3, 5, 99, 3]), [1002, 5, 3, 5, 99, 9]);
        assert_eq!(memory(&[1102, 3, 4, 3, 99]), [1102, 3, 4, 12, 99]);
    }

    #[test]
    fn input() {
        assert_eq!(execute(&[3, 1, 99], &[2]).memory, [3, 2, 99]);
        assert_eq!(execute(&[3, 0, 99], &[-10]).memory, [-10, 0, 99]);
    }

    #[test]
    fn multiple_inputs() {
        assert_eq!(
            execute(&[3, 1, 3, 3, 3, 5, 99], &[11, 22, 33]).memory,
            [3, 11, 3, 22, 3, 33, 99]
        );
    }

    #[test]
    fn output() {
        assert_eq!(execute(&[4, 2, 104, -29, 99], &[]).output, [104, -29]);
    }

    #[test]
    fn jump_if_true() {
        assert_eq!(execute(&[5, 7, 8, 99, 104, 1, 99, 0, 4], &[]).output, []);
        assert_eq!(execute(&[5, 7, 8, 99, 104, 1, 99, 1, 4], &[]).output, [1]);
        assert_eq!(execute(&[1005, 7, 4, 99, 104, 2, 99, 0], &[]).output, []);
        assert_eq!(execute(&[1005, 7, 4, 99, 104, 2, 99, 1], &[]).output, [2]);
        assert_eq!(execute(&[1105, 0, 4, 99, 104, 3, 99], &[]).output, []);
        assert_eq!(execute(&[1105, 1, 4, 99, 104, 3, 99], &[]).output, [3]);
    }

    #[test]
    fn jump_if_false() {
        assert_eq!(execute(&[6, 7, 8, 99, 104, 1, 99, 0, 4], &[]).output, [1]);
        assert_eq!(execute(&[6, 7, 8, 99, 104, 1, 99, 1, 4], &[]).output, []);
        assert_eq!(execute(&[1006, 7, 4, 99, 104, 2, 99, 0], &[]).output, [2]);
        assert_eq!(execute(&[1006, 7, 4, 99, 104, 2, 99, 1], &[]).output, []);
        assert_eq!(execute(&[1106, 0, 4, 99, 104, 3, 99], &[]).output, [3]);
        assert_eq!(execute(&[1106, 1, 4, 99, 104, 3, 99], &[]).output, []);
    }

    #[test]
    fn less_than() {
        assert_eq!(execute(&[1107, 1, 2, 5, 104, -9, 99], &[]).output, [1]);
        assert_eq!(execute(&[1107, 2, 1, 5, 104, -9, 99], &[]).output, [0]);
        assert_eq!(execute(&[1007, 1, 7, 5, 104, -9, 99, 2], &[]).output, [1]);
        assert_eq!(execute(&[1007, 2, 7, 5, 104, -9, 99, 1], &[]).output, [0]);
        assert_eq!(execute(&[7, 7, 8, 5, 104, -9, 99, 1, 2], &[]).output, [1]);
        assert_eq!(execute(&[7, 7, 8, 5, 104, -9, 99, 2, 1], &[]).output, [0]);
    }

    #[test]
    fn equals() {
        assert_eq!(execute(&[1108, 3, 3, 5, 104, -9, 99], &[]).output, [1]);
        assert_eq!(execute(&[1108, 1, 3, 5, 104, -9, 99], &[]).output, [0]);
        assert_eq!(execute(&[1008, 7, 3, 5, 104, -9, 99, 3], &[]).output, [1]);
        assert_eq!(execute(&[1008, 7, 1, 5, 104, -9, 99, 3], &[]).output, [0]);
        assert_eq!(execute(&[8, 7, 8, 5, 104, -9, 99, 3, 3], &[]).output, [1]);
        assert_eq!(execute(&[8, 7, 8, 5, 104, -9, 99, 1, 3], &[]).output, [0]);
    }

    #[test]
    fn overriding_instruction() {
        assert_eq!(
            memory(&[1, 1, 1, 4, 99, 5, 6, 0, 99]),
            [30, 1, 1, 4, 2, 5, 6, 0, 99]
        );
    }

    #[test]
    fn day2_example_prograp() {
        assert_eq!(
            memory(&[1, 9, 10, 3, 2, 3, 11, 0, 99, 30, 40, 50]),
            [3500, 9, 10, 70, 2, 3, 11, 0, 99, 30, 40, 50]
        );
    }

    #[test]
    fn example_equals_position_mode() {
        let code = &[3, 9, 8, 9, 10, 9, 4, 9, 99, -1, 8];
        assert_eq!(execute(code, &[8],).output, [1]);
        assert_eq!(execute(code, &[9],).output, [0]);
    }

    #[test]
    fn example_equals_immediate_mode() {
        assert_eq!(execute(&[3, 3, 1108, -1, 8, 3, 4, 3, 99], &[8]).output, [1]);
        assert_eq!(execute(&[3, 3, 1108, -1, 8, 3, 4, 3, 99], &[9]).output, [0]);
    }

    #[test]
    fn example_less_than_position_mode() {
        let code = &[3, 9, 7, 9, 10, 9, 4, 9, 99, -1, 8];
        assert_eq!(execute(code, &[7],).output, [1]);
        assert_eq!(execute(code, &[8],).output, [0]);
    }

    #[test]
    fn example_less_than_immediate_mode() {
        assert_eq!(execute(&[3, 3, 1107, -1, 8, 3, 4, 3, 99], &[7]).output, [1]);
        assert_eq!(execute(&[3, 3, 1107, -1, 8, 3, 4, 3, 99], &[8]).output, [0]);
    }

    #[test]
    fn example_jumps_position_mode() {
        let code = &[3, 12, 6, 12, 15, 1, 13, 14, 13, 4, 13, 99, -1, 0, 1, 9];
        assert_eq!(execute(code, &[0],).output, [0]);
        assert_eq!(execute(code, &[2],).output, [1]);
    }

    #[test]
    fn example_jumps_immediate_mode() {
        let code = &[3, 3, 1105, -1, 9, 1101, 0, 0, 12, 4, 12, 99, 1];
        assert_eq!(execute(code, &[0],).output, [0]);
        assert_eq!(execute(code, &[2],).output, [1]);
    }

    #[test]
    fn passes_diagnostic_tests_day05() {
        let output = Program::parse(crate::day05::INPUT).execute(vec![1]).output;
        assert_eq!(output[0..output.len() - 1], [0, 0, 0, 0, 0, 0, 0, 0, 0]);
    }

    #[test]
    fn passes_diagnostic_tests_day09() {
        let output = Program::parse(crate::day09::INPUT).execute(vec![1]).output;
        assert_eq!(output[0..output.len() - 1], []);
    }

    #[test]
    fn example_larger() {
        let code = &[
            3, 21, 1008, 21, 8, 20, 1005, 20, 22, 107, 8, 21, 20, 1006, 20, 31, 1106, 0, 36, 98, 0,
            0, 1002, 21, 125, 20, 4, 20, 1105, 1, 46, 104, 999, 1105, 1, 46, 1101, 1000, 1, 20, 4,
            20, 1105, 1, 46, 98, 99,
        ];

        assert_eq!(execute(code, &[7],).output, [999]);
        assert_eq!(execute(code, &[8],).output, [1000]);
        assert_eq!(execute(code, &[9],).output, [1001]);
    }

    #[test]
    fn quine() {
        let code = &[
            109, 1, 204, -1, 1001, 100, 1, 100, 1008, 100, 16, 101, 1006, 101, 0, 99,
        ];
        assert_eq!(execute(code, &[],).output, code);
    }

    #[test]
    fn large_number() {
        let code = &[1102, 34915192, 34915192, 7, 4, 7, 99, 0];
        assert_eq!(execute(code, &[],).output, [1219070632396864]);
    }

    #[test]
    fn large_output() {
        let code = &[104, 1125899906842624, 99];
        assert_eq!(execute(code, &[],).output, [1125899906842624]);
    }
}
