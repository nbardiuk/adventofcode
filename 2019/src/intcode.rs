#[derive(Default, Clone)]
pub struct Computer {
    pub memory: Vec<i64>,
    instruction_pointer: usize,
    relative_base: i64,
    pub halted: bool,
}

impl Computer {
    pub fn new(program: Vec<i64>) -> Self {
        let mut instance: Self = Default::default();
        instance.memory = program;
        instance
    }

    pub fn run(&mut self) -> Vec<i64> {
        self.process(&mut vec![])
    }

    pub fn call(mut self, mut input: Vec<i64>) -> i64 {
        *self.process(&mut input).last().unwrap()
    }

    pub fn process(&mut self, input: &mut Vec<i64>) -> Vec<i64> {
        let mut output = vec![];
        loop {
            match self.opcode() {
                // add
                1 => {
                    *self.to(3) = self.at(1) + self.at(2);
                    self.shift(4);
                }
                // multiply
                2 => {
                    *self.to(3) = self.at(1) * self.at(2);
                    self.shift(4);
                }
                // input
                3 => {
                    if input.is_empty() {
                        break;
                    }
                    *self.to(1) = input.remove(0);
                    self.shift(2);
                }
                // output
                4 => {
                    output.push(self.at(1));
                    self.shift(2);
                }
                // jump if true
                5 => {
                    if self.at(1) != 0 {
                        self.jump(self.at(2) as usize)
                    } else {
                        self.shift(3);
                    };
                }
                // jump if false
                6 => {
                    if self.at(1) == 0 {
                        self.jump(self.at(2) as usize)
                    } else {
                        self.shift(3);
                    };
                }
                // less than
                7 => {
                    *self.to(3) = (self.at(1) < self.at(2)).into();
                    self.shift(4);
                }
                // equals
                8 => {
                    *self.to(3) = (self.at(1) == self.at(2)).into();
                    self.shift(4);
                }
                // adjust relative base
                9 => {
                    self.adjust_base(self.at(1));
                    self.shift(2);
                }
                // halt
                opcode => {
                    debug_assert_eq!(opcode, 99);
                    self.halted = true;
                    break;
                }
            }
        }
        output
    }

    pub fn parse(text: &str) -> Self {
        let program = text
            .trim_end_matches('\n')
            .split(',')
            .map(|n| n.parse().unwrap())
            .collect();
        Self::new(program)
    }

    fn opcode(&self) -> u8 {
        (self.memory[self.instruction_pointer] % 100) as u8
    }

    fn shift(&mut self, amount: usize) {
        self.instruction_pointer += amount;
    }

    fn adjust_base(&mut self, amount: i64) {
        self.relative_base += amount;
    }

    fn jump(&mut self, position: usize) {
        self.instruction_pointer = position;
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

    fn memory(program: &[i64], input: &[i64]) -> Vec<i64> {
        let mut computer = Computer::new(Vec::from(program));
        computer.process(&mut Vec::from(input));
        computer.memory
    }

    fn output(program: &[i64], input: &[i64]) -> Vec<i64> {
        Computer::new(Vec::from(program)).process(&mut Vec::from(input))
    }

    #[test]
    fn addition() {
        assert_eq!(memory(&[1, 5, 5, 3, 99, 1], &[]), [1, 5, 5, 2, 99, 1]);
        assert_eq!(memory(&[101, 1, 5, 3, 99, 1], &[]), [101, 1, 5, 2, 99, 1]);
        assert_eq!(memory(&[1001, 5, 1, 3, 99, 1], &[]), [1001, 5, 1, 2, 99, 1]);
        assert_eq!(memory(&[1101, 1, 1, 3, 99], &[]), [1101, 1, 1, 2, 99]);
        assert_eq!(memory(&[1101, 100, -1, 4, 0], &[]), [1101, 100, -1, 4, 99]);
    }

    #[test]
    fn multiplication() {
        assert_eq!(memory(&[2, 5, 0, 3, 99, 3], &[]), [2, 5, 0, 6, 99, 3]);
        assert_eq!(memory(&[102, 3, 5, 3, 99, 2], &[]), [102, 3, 5, 6, 99, 2]);
        assert_eq!(memory(&[1002, 5, 3, 5, 99, 3], &[]), [1002, 5, 3, 5, 99, 9]);
        assert_eq!(memory(&[1102, 3, 4, 3, 99], &[]), [1102, 3, 4, 12, 99]);
    }

    #[test]
    fn input() {
        assert_eq!(memory(&[3, 1, 99], &[2]), [3, 2, 99]);
        assert_eq!(memory(&[3, 0, 99], &[-10]), [-10, 0, 99]);
    }

    #[test]
    fn multiple_inputs() {
        assert_eq!(
            memory(&[3, 1, 3, 3, 3, 5, 99], &[11, 22, 33]),
            [3, 11, 3, 22, 3, 33, 99]
        );
    }

    #[test]
    fn output_example() {
        assert_eq!(output(&[4, 2, 104, -29, 99], &[]), [104, -29]);
    }

    #[test]
    fn jump_if_true() {
        assert_eq!(output(&[5, 7, 8, 99, 104, 1, 99, 0, 4], &[]), []);
        assert_eq!(output(&[5, 7, 8, 99, 104, 1, 99, 1, 4], &[]), [1]);
        assert_eq!(output(&[1005, 7, 4, 99, 104, 2, 99, 0], &[]), []);
        assert_eq!(output(&[1005, 7, 4, 99, 104, 2, 99, 1], &[]), [2]);
        assert_eq!(output(&[1105, 0, 4, 99, 104, 3, 99], &[]), []);
        assert_eq!(output(&[1105, 1, 4, 99, 104, 3, 99], &[]), [3]);
    }

    #[test]
    fn jump_if_false() {
        assert_eq!(output(&[6, 7, 8, 99, 104, 1, 99, 0, 4], &[]), [1]);
        assert_eq!(output(&[6, 7, 8, 99, 104, 1, 99, 1, 4], &[]), []);
        assert_eq!(output(&[1006, 7, 4, 99, 104, 2, 99, 0], &[]), [2]);
        assert_eq!(output(&[1006, 7, 4, 99, 104, 2, 99, 1], &[]), []);
        assert_eq!(output(&[1106, 0, 4, 99, 104, 3, 99], &[]), [3]);
        assert_eq!(output(&[1106, 1, 4, 99, 104, 3, 99], &[]), []);
    }

    #[test]
    fn less_than() {
        assert_eq!(output(&[1107, 1, 2, 5, 104, -9, 99], &[]), [1]);
        assert_eq!(output(&[1107, 2, 1, 5, 104, -9, 99], &[]), [0]);
        assert_eq!(output(&[1007, 1, 7, 5, 104, -9, 99, 2], &[]), [1]);
        assert_eq!(output(&[1007, 2, 7, 5, 104, -9, 99, 1], &[]), [0]);
        assert_eq!(output(&[7, 7, 8, 5, 104, -9, 99, 1, 2], &[]), [1]);
        assert_eq!(output(&[7, 7, 8, 5, 104, -9, 99, 2, 1], &[]), [0]);
    }

    #[test]
    fn equals() {
        assert_eq!(output(&[1108, 3, 3, 5, 104, -9, 99], &[]), [1]);
        assert_eq!(output(&[1108, 1, 3, 5, 104, -9, 99], &[]), [0]);
        assert_eq!(output(&[1008, 7, 3, 5, 104, -9, 99, 3], &[]), [1]);
        assert_eq!(output(&[1008, 7, 1, 5, 104, -9, 99, 3], &[]), [0]);
        assert_eq!(output(&[8, 7, 8, 5, 104, -9, 99, 3, 3], &[]), [1]);
        assert_eq!(output(&[8, 7, 8, 5, 104, -9, 99, 1, 3], &[]), [0]);
    }

    #[test]
    fn overriding_instruction() {
        assert_eq!(
            memory(&[1, 1, 1, 4, 99, 5, 6, 0, 99], &[]),
            [30, 1, 1, 4, 2, 5, 6, 0, 99]
        );
    }

    #[test]
    fn day2_example_prograp() {
        assert_eq!(
            memory(&[1, 9, 10, 3, 2, 3, 11, 0, 99, 30, 40, 50], &[]),
            [3500, 9, 10, 70, 2, 3, 11, 0, 99, 30, 40, 50]
        );
    }

    #[test]
    fn example_equals_position_mode() {
        let program = &[3, 9, 8, 9, 10, 9, 4, 9, 99, -1, 8];
        assert_eq!(output(program, &[8],), [1]);
        assert_eq!(output(program, &[9],), [0]);
    }

    #[test]
    fn example_equals_immediate_mode() {
        assert_eq!(output(&[3, 3, 1108, -1, 8, 3, 4, 3, 99], &[8]), [1]);
        assert_eq!(output(&[3, 3, 1108, -1, 8, 3, 4, 3, 99], &[9]), [0]);
    }

    #[test]
    fn example_less_than_position_mode() {
        let program = &[3, 9, 7, 9, 10, 9, 4, 9, 99, -1, 8];
        assert_eq!(output(program, &[7],), [1]);
        assert_eq!(output(program, &[8],), [0]);
    }

    #[test]
    fn example_less_than_immediate_mode() {
        assert_eq!(output(&[3, 3, 1107, -1, 8, 3, 4, 3, 99], &[7]), [1]);
        assert_eq!(output(&[3, 3, 1107, -1, 8, 3, 4, 3, 99], &[8]), [0]);
    }

    #[test]
    fn example_jumps_position_mode() {
        let program = &[3, 12, 6, 12, 15, 1, 13, 14, 13, 4, 13, 99, -1, 0, 1, 9];
        assert_eq!(output(program, &[0],), [0]);
        assert_eq!(output(program, &[2],), [1]);
    }

    #[test]
    fn example_jumps_immediate_mode() {
        let program = &[3, 3, 1105, -1, 9, 1101, 0, 0, 12, 4, 12, 99, 1];
        assert_eq!(output(program, &[0],), [0]);
        assert_eq!(output(program, &[2],), [1]);
    }

    #[test]
    fn example_larger() {
        let program = &[
            3, 21, 1008, 21, 8, 20, 1005, 20, 22, 107, 8, 21, 20, 1006, 20, 31, 1106, 0, 36, 98, 0,
            0, 1002, 21, 125, 20, 4, 20, 1105, 1, 46, 104, 999, 1105, 1, 46, 1101, 1000, 1, 20, 4,
            20, 1105, 1, 46, 98, 99,
        ];

        assert_eq!(output(program, &[7],), [999]);
        assert_eq!(output(program, &[8],), [1000]);
        assert_eq!(output(program, &[9],), [1001]);
    }

    #[test]
    fn quine() {
        let program = &[
            109, 1, 204, -1, 1001, 100, 1, 100, 1008, 100, 16, 101, 1006, 101, 0, 99,
        ];
        assert_eq!(output(program, &[],), program);
    }

    #[test]
    fn large_number() {
        let program = &[1102, 34915192, 34915192, 7, 4, 7, 99, 0];
        assert_eq!(output(program, &[],), [1219070632396864]);
    }

    #[test]
    fn large_output() {
        let program = &[104, 1125899906842624, 99];
        assert_eq!(output(program, &[],), [1125899906842624]);
    }
}
