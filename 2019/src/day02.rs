use rayon::prelude::*;

pub const INPUT: &str = include_str!("../res/day02.txt");

pub fn part1(input: &str) -> usize {
    run(12, 2, numbers(input))
}

pub fn part2(input: &str) -> Option<usize> {
    let memory = numbers(input);
    (0 as usize..10000)
        .into_par_iter()
        .find_any(|i| 1969_07_20 == run(i / 100, i % 100, memory.clone()))
}

fn run(noun: usize, verb: usize, mut memory: Vec<usize>) -> usize {
    memory[1] = noun;
    memory[2] = verb;
    execute(memory)[0]
}

fn numbers(text: &str) -> Vec<usize> {
    text.split(',').filter_map(|num| num.parse().ok()).collect()
}

fn execute(mut memory: Vec<usize>) -> Vec<usize> {
    let size = 4;
    let mut pointer = 0;
    loop {
        match memory[pointer..(pointer + size).min(memory.len())] {
            [1, in_a, in_b, out] => {
                memory[out] = memory[in_a] + memory[in_b];
            }
            [2, in_a, in_b, out] => {
                memory[out] = memory[in_a] * memory[in_b];
            }
            _ => break,
        };
        pointer += size;
    }
    memory
}

#[cfg(test)]
mod spec {
    use super::*;

    #[test]
    fn addition() {
        assert_eq!(execute(vec![1, 0, 0, 0, 99]), [2, 0, 0, 0, 99]);
    }

    #[test]
    fn multiplication() {
        assert_eq!(execute(vec![2, 3, 0, 3, 99]), [2, 3, 0, 6, 99]);
    }

    #[test]
    fn overriding_instruction() {
        let result = execute(vec![1, 1, 1, 4, 99, 5, 6, 0, 99]);
        assert_eq!(result, [30, 1, 1, 4, 2, 5, 6, 0, 99]);
    }

    #[test]
    fn initial_example() {
        let result = execute(vec![1, 9, 10, 3, 2, 3, 11, 0, 99, 30, 40, 50]);
        assert_eq!(result, [3500, 9, 10, 70, 2, 3, 11, 0, 99, 30, 40, 50]);
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
