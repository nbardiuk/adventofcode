#[derive(Default, Clone)]
pub struct Computer {
    pub memory: Vec<i64>,
    instruction_pointer: usize,
    relative_base: i64,
    output: Vec<i64>,
    pub has_terminated: bool,
}

impl Computer {
    pub fn new(program: Vec<i64>) -> Self {
        let mut instance: Self = Default::default();
        instance.memory = program;
        instance
    }

    pub fn call(mut self, mut input: Vec<i64>) -> i64 {
        self.iteration(&mut input);
        self.output.last().cloned().unwrap()
    }

    pub fn iteration<'a>(&'a mut self, input: &mut Vec<i64>) -> &'a mut Self {
        loop {
            let opcode = (self.memory[self.instruction_pointer] % 100) as u8;
            match opcode {
                // add
                1 => {
                    *self.to(3) = self.at(1) + self.at(2);
                    self.instruction_pointer += 4;
                }
                // multiply
                2 => {
                    *self.to(3) = self.at(1) * self.at(2);
                    self.instruction_pointer += 4;
                }
                // input
                3 => {
                    if input.is_empty() {
                        return self;
                    }
                    *self.to(1) = input.remove(0);
                    self.instruction_pointer += 2;
                }
                // output
                4 => {
                    self.output.push(self.at(1));
                    self.instruction_pointer += 2;
                }
                // jump if true
                5 => {
                    if self.at(1) != 0 {
                        self.instruction_pointer = self.at(2) as usize;
                    } else {
                        self.instruction_pointer += 3;
                    };
                }
                // jump if false
                6 => {
                    if self.at(1) == 0 {
                        self.instruction_pointer = self.at(2) as usize;
                    } else {
                        self.instruction_pointer += 3;
                    };
                }
                // less than
                7 => {
                    *self.to(3) = (self.at(1) < self.at(2)).into();
                    self.instruction_pointer += 4;
                }
                // equals
                8 => {
                    *self.to(3) = (self.at(1) == self.at(2)).into();
                    self.instruction_pointer += 4;
                }
                // adjust relative base
                9 => {
                    self.relative_base += self.at(1);
                    self.instruction_pointer += 2;
                }
                // halt
                _ => {
                    assert_eq!(opcode, 99);
                    self.has_terminated = true;
                    return self;
                }
            };
        }
    }

    pub fn flush_output(&mut self) -> Vec<i64> {
        std::mem::replace(&mut self.output, vec![])
    }

    pub fn parse(text: &str) -> Self {
        let program = text
            .trim_end_matches('\n')
            .split(',')
            .map(|n| n.parse().unwrap())
            .collect();
        Self::new(program)
    }

    fn mode_for(&self, mut parameter: u8) -> u8 {
        let mut modes = (self.memory[self.instruction_pointer] / 100) as u8;
        while parameter > 1 {
            modes /= 10;
            parameter -= 1;
        }
        modes % 10
    }

    fn address_for(&self, parameter: u8) -> usize {
        let address = self.instruction_pointer + parameter as usize;
        match self.mode_for(parameter) {
            // position
            0 => self.memory[address] as usize,
            // immediate
            1 => address,
            // relative
            _ => (self.relative_base + self.memory[address]) as usize,
        }
    }

    fn at(&self, parameter: u8) -> i64 {
        let address = self.address_for(parameter);
        if self.memory.len() <= address {
            0
        } else {
            self.memory[address]
        }
    }

    fn to(&mut self, parameter: u8) -> &mut i64 {
        let address = self.address_for(parameter);
        if self.memory.len() <= address {
            self.memory.resize(address + 100, 0);
        }
        &mut self.memory[address]
    }
}

#[cfg(test)]
mod spec {
    use super::*;

    fn memory(program: &[i64]) -> Vec<i64> {
        Computer::new(Vec::from(program))
            .iteration(&mut vec![])
            .memory
            .clone()
    }

    fn execute(program: &[i64], input: &[i64]) -> Computer {
        Computer::new(Vec::from(program))
            .iteration(&mut Vec::from(input))
            .clone()
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
        let program = &[3, 9, 8, 9, 10, 9, 4, 9, 99, -1, 8];
        assert_eq!(execute(program, &[8],).output, [1]);
        assert_eq!(execute(program, &[9],).output, [0]);
    }

    #[test]
    fn example_equals_immediate_mode() {
        assert_eq!(execute(&[3, 3, 1108, -1, 8, 3, 4, 3, 99], &[8]).output, [1]);
        assert_eq!(execute(&[3, 3, 1108, -1, 8, 3, 4, 3, 99], &[9]).output, [0]);
    }

    #[test]
    fn example_less_than_position_mode() {
        let program = &[3, 9, 7, 9, 10, 9, 4, 9, 99, -1, 8];
        assert_eq!(execute(program, &[7],).output, [1]);
        assert_eq!(execute(program, &[8],).output, [0]);
    }

    #[test]
    fn example_less_than_immediate_mode() {
        assert_eq!(execute(&[3, 3, 1107, -1, 8, 3, 4, 3, 99], &[7]).output, [1]);
        assert_eq!(execute(&[3, 3, 1107, -1, 8, 3, 4, 3, 99], &[8]).output, [0]);
    }

    #[test]
    fn example_jumps_position_mode() {
        let program = &[3, 12, 6, 12, 15, 1, 13, 14, 13, 4, 13, 99, -1, 0, 1, 9];
        assert_eq!(execute(program, &[0],).output, [0]);
        assert_eq!(execute(program, &[2],).output, [1]);
    }

    #[test]
    fn example_jumps_immediate_mode() {
        let program = &[3, 3, 1105, -1, 9, 1101, 0, 0, 12, 4, 12, 99, 1];
        assert_eq!(execute(program, &[0],).output, [0]);
        assert_eq!(execute(program, &[2],).output, [1]);
    }

    #[test]
    fn example_larger() {
        let program = &[
            3, 21, 1008, 21, 8, 20, 1005, 20, 22, 107, 8, 21, 20, 1006, 20, 31, 1106, 0, 36, 98, 0,
            0, 1002, 21, 125, 20, 4, 20, 1105, 1, 46, 104, 999, 1105, 1, 46, 1101, 1000, 1, 20, 4,
            20, 1105, 1, 46, 98, 99,
        ];

        assert_eq!(execute(program, &[7],).output, [999]);
        assert_eq!(execute(program, &[8],).output, [1000]);
        assert_eq!(execute(program, &[9],).output, [1001]);
    }

    #[test]
    fn quine() {
        let program = &[
            109, 1, 204, -1, 1001, 100, 1, 100, 1008, 100, 16, 101, 1006, 101, 0, 99,
        ];
        assert_eq!(execute(program, &[],).output, program);
    }

    #[test]
    fn large_number() {
        let program = &[1102, 34915192, 34915192, 7, 4, 7, 99, 0];
        assert_eq!(execute(program, &[],).output, [1219070632396864]);
    }

    #[test]
    fn large_output() {
        let program = &[104, 1125899906842624, 99];
        assert_eq!(execute(program, &[],).output, [1125899906842624]);
    }
}
