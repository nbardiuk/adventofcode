pub const INPUT: &str = include_str!("../res/day02.txt");

pub fn part1(input: &str) -> usize {
    run(12, 2, numbers(input))
}

pub fn part2(input: &str) -> Option<usize> {
    let memory = numbers(input);
    (0..10000).find(|i| 1969_07_20 == run(i / 100, i % 100, memory.clone()))
}

fn run(noun: usize, verb: usize, mut memory: Vec<usize>) -> usize {
    memory[1] = noun;
    memory[2] = verb;
    execute(memory)[0]
}

fn numbers(text: &str) -> Vec<usize> {
    text.split(',').filter_map(|num| num.parse().ok()).collect()
}

fn execute(memory: Vec<usize>) -> Vec<usize> {
    let mut state = (memory, Some(0));
    while let (memory, Some(pointer)) = state {
        state = iteration(memory, pointer);
    }
    state.0
}

fn iteration(mut memory: Vec<usize>, pointer: usize) -> (Vec<usize>, Option<usize>) {
    match memory.get(pointer..pointer + 4) {
        Some(&[1, in_a, in_b, out]) => {
            memory[out] = memory[in_a] + memory[in_b];
            (memory, Some(pointer + 4))
        }
        Some(&[2, in_a, in_b, out]) => {
            memory[out] = memory[in_a] * memory[in_b];
            (memory, Some(pointer + 4))
        }
        _ => (memory, None),
    }
}

#[cfg(test)]
mod spec {
    use super::*;

    #[test]
    fn iteration_addition() {
        let (memory, pointer) = iteration(vec![1, 0, 0, 0, 99], 0);

        assert_eq!(memory, [2, 0, 0, 0, 99]);
        assert_eq!(pointer, Some(4));
    }

    #[test]
    fn iteration_multiplication() {
        let (memory, pointer) = iteration(vec![2, 3, 0, 3, 99], 0);

        assert_eq!(memory, [2, 3, 0, 6, 99]);
        assert_eq!(pointer, Some(4));
    }

    #[test]
    fn iteration_halting() {
        let (memory, pointer) = iteration(vec![99], 0);

        assert_eq!(memory, [99]);
        assert_eq!(pointer, None);
    }

    #[test]
    fn execution_halting() {
        let memory = execute(vec![2, 4, 4, 5, 99, 0]);

        assert_eq!(memory, [2, 4, 4, 5, 99, 9801]);
    }

    #[test]
    fn execution_overriding_instruction() {
        let memory = execute(vec![1, 1, 1, 4, 99, 5, 6, 0, 99]);

        assert_eq!(memory, [30, 1, 1, 4, 2, 5, 6, 0, 99]);
    }

    #[test]
    fn part1_my_input() {
        assert_eq!(part1(INPUT), 4570637);
    }

    #[test]
    fn part2_my_input() {
        assert_eq!(part2(INPUT), Some(5485));
    }
}
