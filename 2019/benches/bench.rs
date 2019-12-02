use criterion::{criterion_group, criterion_main, Criterion};

macro_rules! day {
    ($day:ident) => {
        pub fn $day(c: &mut Criterion) {
            use aoc2019::$day::{part1, part2};
            let input = include_str!(concat!("../res/", stringify!($day), ".txt"));

            let day = stringify!($day);
            c.bench_function(&format!("{}::part1", day), |b| b.iter(|| part1(input)));
            c.bench_function(&format!("{}::part2", day), |b| b.iter(|| part2(input)));
        }
    };
}

day! { day01 }
day! { day02 }

criterion_group! {
    name = benches;
    config = Criterion::default().noise_threshold(0.07);
    targets = day01, day02
}
criterion_main!(benches);
