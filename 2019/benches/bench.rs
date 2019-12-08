use criterion::{criterion_group, criterion_main, Criterion};

macro_rules! day {
    ($day:ident) => {
        pub fn $day(c: &mut Criterion) {
            use aoc2019::$day::{part1, part2, INPUT};
            c.bench_function(&stringify!($day part1), |b| b.iter(|| part1(INPUT)));
            c.bench_function(&stringify!($day part2), |b| b.iter(|| part2(INPUT)));
        }
    };
}

day! { day01 }
day! { day02 }
day! { day03 }
day! { day04 }
day! { day05 }
day! { day06 }
day! { day07 }
day! { day08 }

criterion_group! {
    name = benches;
    config = Criterion::default().noise_threshold(0.07);
    targets = day01, day02, day03, day04, day05, day06, day07, day08
}
criterion_main!(benches);
