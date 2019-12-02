pub fn part1(input: &str) -> usize {
    program(12, 2, numbers(input))
}

pub fn part2(input: &str) -> usize {
    for noun in 0..99 {
        for verb in 0..99 {
            if 19_690_720 == program(noun, verb, numbers(input)) {
                return noun * 100 + verb;
            }
        }
    }
    0
}

fn program(noun: usize, verb: usize, mut memory: Vec<usize>) -> usize {
    memory[1] = noun;
    memory[2] = verb;
    execute(memory)[0]
}

fn numbers(text: &str) -> Vec<usize> {
    text.split(',')
        .filter_map(|line| line.parse::<usize>().ok())
        .collect()
}

fn execute(memory: Vec<usize>) -> Vec<usize> {
    fn go(memory: Vec<usize>, counter: usize) -> Vec<usize> {
        match iteration(memory, counter) {
            (memory, None) => memory,
            (memory, Some(counter)) => go(memory, counter),
        }
    };
    go(memory, 0)
}

fn iteration(mut memory: Vec<usize>, counter: usize) -> (Vec<usize>, Option<usize>) {
    match memory[counter] {
        1 => {
            let a = memory[counter + 1];
            let b = memory[counter + 2];
            let c = memory[counter + 3];
            memory[c] = memory[a] + memory[b];
            (memory, Some(counter + 4))
        }
        2 => {
            let a = memory[counter + 1];
            let b = memory[counter + 2];
            let c = memory[counter + 3];
            memory[c] = memory[a] * memory[b];
            (memory, Some(counter + 4))
        }
        _ => (memory, None),
    }
}

#[cfg(test)]
mod spec {
    use super::*;
    const INPUT: &str = include_str!("../res/day02.txt");

    #[test]
    fn iteration_addition() {
        let memory = vec![1, 0, 0, 0, 99];
        let counter = 0;

        let (memory, counter) = iteration(memory, counter);

        assert_eq!(memory, [2, 0, 0, 0, 99]);
        assert_eq!(counter, Some(4));
    }

    #[test]
    fn iteration_multiplication() {
        let memory = vec![2, 3, 0, 3, 99];
        let counter = 0;

        let (memory, counter) = iteration(memory, counter);

        assert_eq!(memory, [2, 3, 0, 6, 99]);
        assert_eq!(counter, Some(4));
    }

    #[test]
    fn iteration_halting() {
        let memory = vec![99];
        let counter = 0;

        let (memory, counter) = iteration(memory, counter);

        assert_eq!(memory, [99]);
        assert_eq!(counter, None);
    }

    #[test]
    fn execution_halting() {
        let memory = vec![2, 4, 4, 5, 99, 0];

        let memory = execute(memory);

        assert_eq!(memory, [2, 4, 4, 5, 99, 9801]);
    }

    #[test]
    fn execution_overriding_instruction() {
        let memory = vec![1, 1, 1, 4, 99, 5, 6, 0, 99];

        let memory = execute(memory);

        assert_eq!(memory, [30, 1, 1, 4, 2, 5, 6, 0, 99]);
    }

    #[test]
    fn part1_my_input() {
        assert_eq!(part1(INPUT), 4570637);
    }

    #[test]
    fn part2_my_input() {
        assert_eq!(part2(INPUT), 5485);
    }
}
