use criterion::{criterion_group, criterion_main, Criterion};

pub fn day01(c: &mut Criterion) {
    use aoc2019::day01::{part1, part2};
    let input = include_str!("../res/day01.txt");

    c.bench_function("day01::part1", |b| b.iter(|| part1(input)));
    c.bench_function("day01::part2", |b| b.iter(|| part2(input)));
}

pub fn day02(c: &mut Criterion) {
    use aoc2019::day02::{part1, part2};
    let input = include_str!("../res/day02.txt");

    c.bench_function("day02::part1", |b| b.iter(|| part1(input)));
    c.bench_function("day02::part2", |b| b.iter(|| part2(input)));
}

criterion_group!(benches, day01, day02);
criterion_main!(benches);
