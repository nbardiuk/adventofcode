use num_integer::*;
use rayon::prelude::*;
use regex::Regex;

pub const INPUT: &str = "<x=5, y=-1, z=5>\n<x=0, y=-14, z=2>\n<x=16, y=4, z=0>\n<x=18, y=1, z=16>";
type V3 = [i16; 3];

pub fn part1(input: &str) -> u16 {
    let moons = read_initial_state(input);
    total_energy(moons, 1000)
}

pub fn part2(input: &str) -> u64 {
    let moons = read_initial_state(input);
    (0..3_usize)
        .into_par_iter()
        .map(|i| repeats_projection(&moons, i))
        .reduce(|| 1, |a, b| a.lcm(&b))
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

fn repeats_projection(moons: &[(V3, V3)], i: usize) -> u64 {
    let mut moons = moons.iter().map(|(p, v)| (p[i], v[i])).collect::<Vec<_>>();
    let start = moons.clone();
    let mut count = 0;
    loop {
        count += 1;
        moons = gravity(moons);
        if start == moons {
            return count;
        }
    }
}

fn gravity(mut moons: Vec<(i16, i16)>) -> Vec<(i16, i16)> {
    for i in 0..moons.len() {
        for j in i + 1..moons.len() {
            let dv = (moons[j].0 - moons[i].0).signum();
            moons[i].1 += dv;
            moons[j].1 -= dv;
        }
    }
    for moon in moons.iter_mut() {
        moon.0 += moon.1;
    }
    moons
}

fn total_energy(moons: Vec<(V3, V3)>, steps: u16) -> u16 {
    let mut energies = vec![(0_u16, 0_u16); moons.len()];
    for i in 0..3 {
        let mut moons = moons.iter().map(|(p, v)| (p[i], v[i])).collect();
        for _ in 0..steps {
            moons = gravity(moons);
        }

        for i in 0..energies.len() {
            energies[i].0 += moons[i].0.abs() as u16;
            energies[i].1 += moons[i].1.abs() as u16;
        }
    }
    energies.iter().map(|(pot, kin)| pot * kin).sum()
}

#[cfg(test)]
mod spec {
    use super::*;

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
