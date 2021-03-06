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
day! { day09 }
day! { day10 }
day! { day11 }
day! { day12 }
day! { day13 }
day! { day14 }
day! { day15 }
day! { day16 }
day! { day17 }
day! { day18 }
day! { day19 }
day! { day20 }
day! { day21 }
day! { day22 }
day! { day23 }
day! { day24 }

criterion_group! {
    name = microseconds;
    config = Criterion::default().noise_threshold(0.07);
    targets = day01, day02, day03, day05, day06, day07, day08, day14, day22
}
criterion_group! {
    name = milliseconds;
    config = Criterion::default().sample_size(30).noise_threshold(0.07);
    targets = day04, day09, day10, day11, day12, day13, day15, day17, day20, day21, day23
}
criterion_group! {
    name = centiseconds;
    config = Criterion::default().sample_size(10).noise_threshold(0.07);
    targets = day16, day18, day19, day24
}
criterion_main!(microseconds, milliseconds, centiseconds);
