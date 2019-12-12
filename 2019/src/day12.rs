use num_integer::*;
use regex::Regex;

pub const INPUT: &str = "<x=5, y=-1, z=5>\n<x=0, y=-14, z=2>\n<x=16, y=4, z=0>\n<x=18, y=1, z=16>";

pub fn part1(input: &str) -> u16 {
    let moons = read_initial_state(input);
    total_energy(moons, 1000)
}

pub fn part2(input: &str) -> u64 {
    let moons = read_initial_state(input);
    if let [Some(x), Some(y), Some(z)] = repeats_projections(moons) {
        x.lcm(&y).lcm(&z)
    } else {
        0
    }
}

fn read_initial_state(input: &str) -> Vec<(V3, V3)> {
    let re = Regex::new(r"<x=(-?\d+), y=(-?\d+), z=(-?\d+)>").unwrap();
    re.captures_iter(input)
        .map(|capture| {
            let x = capture[1].parse().unwrap();
            let y = capture[2].parse().unwrap();
            let z = capture[3].parse().unwrap();
            ([x, y, z], [0, 0, 0])
        })
        .collect()
}

fn repeats_projections(mut moons: Vec<(V3, V3)>) -> [Option<u64>; 3] {
    let projection = |m: &[(V3, V3)], i| m.iter().map(|(p, v)| (p[i], v[i])).collect::<Vec<_>>();

    let start = [
        projection(&moons, 0),
        projection(&moons, 1),
        projection(&moons, 2),
    ];

    let mut count = 0;
    let mut repeats = [None, None, None];
    loop {
        count += 1;
        moons = gravity(moons);
        for i in 0..repeats.len() {
            if repeats[i].is_none() && start[i] == projection(&moons, i) {
                repeats[i] = Some(count);
            }
        }
        if repeats.iter().all(|i| i.is_some()) {
            return repeats;
        }
    }
}

type V3 = [i16; 3];

fn sum(v: V3) -> u16 {
    (v[0].abs() + v[1].abs() + v[2].abs()) as u16
}

fn add(a: V3, b: V3) -> V3 {
    [a[0] + b[0], a[1] + b[1], a[2] + b[2]]
}

fn sub(a: V3, b: V3) -> V3 {
    [a[0] - b[0], a[1] - b[1], a[2] - b[2]]
}

fn pull(a: V3, b: V3) -> V3 {
    [
        (b[0] - a[0]).signum(),
        (b[1] - a[1]).signum(),
        (b[2] - a[2]).signum(),
    ]
}

fn gravity(mut moons: Vec<(V3, V3)>) -> Vec<(V3, V3)> {
    for i in 0..moons.len() {
        for j in i + 1..moons.len() {
            let dv = pull(moons[i].0, moons[j].0);
            moons[i].1 = add(moons[i].1, dv);
            moons[j].1 = sub(moons[j].1, dv);
        }
    }
    for moon in moons.iter_mut() {
        moon.0 = add(moon.0, moon.1);
    }
    moons
}

fn total_energy(mut moons: Vec<(V3, V3)>, steps: u16) -> u16 {
    for _ in 0..steps {
        moons = gravity(moons);
    }
    moons.iter().map(|(p, v)| sum(*p) * sum(*v)).sum()
}

#[cfg(test)]
mod spec {
    use super::*;

    #[test]
    fn check_pull() {
        assert_eq!(pull([5, -1, 2], [0, 14, 2]), [-1, 1, 0]);
    }

    #[test]
    fn check_gravity() {
        assert_eq!(
            gravity(vec![
                ([-1, 0, 2], [0, 0, 0]),
                ([2, -10, -7], [0, 0, 0]),
                ([4, -8, 8], [0, 0, 0]),
                ([3, 5, -1], [0, 0, 0]),
            ]),
            [
                ([2, -1, 1], [3, -1, -1]),
                ([3, -7, -4], [1, 3, 3]),
                ([1, -7, 5], [-3, 1, -3]),
                ([2, 2, 0], [-1, -3, 1]),
            ]
        );
    }

    #[test]
    fn total_energy_10_steps() {
        assert_eq!(
            total_energy(
                vec![
                    ([-1, 0, 2], [0, 0, 0]),
                    ([2, -10, -7], [0, 0, 0]),
                    ([4, -8, 8], [0, 0, 0]),
                    ([3, 5, -1], [0, 0, 0])
                ],
                10
            ),
            179
        );
    }

    #[test]
    fn total_energy_100_steps() {
        assert_eq!(
            total_energy(
                vec![
                    ([-8, -10, 0], [0, 0, 0]),
                    ([5, 5, 10], [0, 0, 0]),
                    ([2, -7, 3], [0, 0, 0]),
                    ([9, -8, -3], [0, 0, 0])
                ],
                100
            ),
            1940
        );
    }

    #[test]
    fn part1_my_input() {
        assert_eq!(part1(INPUT), 7928);
    }

    #[test]
    fn part2_small_example() {
        assert_eq!(
            part2(
                "<x=-1, y=0, z=2>\n\
                 <x=2, y=-10, z=-7>\n\
                 <x=4, y=-8, z=8>\n\
                 <x=3, y=5, z=-1>"
            ),
            2772
        );
    }

    #[test]
    fn part2_big_example() {
        assert_eq!(
            part2(
                "<x=-8, y=-10, z=0>\n\
                 <x=5, y=5, z=10>\n\
                 <x=2, y=-7, z=3>\n\
                 <x=9, y=-8, z=-3>"
            ),
            4686774924
        );
    }

    #[test]
    fn part2_my_input() {
        assert_eq!(part2(INPUT), 518311327635164);
    }
}
