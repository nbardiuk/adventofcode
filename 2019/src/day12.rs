use regex::Regex;

pub const INPUT: &str = "<x=5, y=-1, z=5>\n\
                         <x=0, y=-14, z=2>\n\
                         <x=16, y=4, z=0>\n\
                         <x=18, y=1, z=16>";

pub fn part1(input: &str) -> u16 {
    let mut moons: Vec<V3> = vec![];
    let re = Regex::new(r"<x=(-?\d+), y=(-?\d+), z=(-?\d+)>").unwrap();
    for cap in re.captures_iter(input) {
        moons.push((
            cap[1].parse().unwrap(),
            cap[2].parse().unwrap(),
            cap[3].parse().unwrap(),
        ));
    }
    total_energy(&moons, 1000)
}

type V3 = (i16, i16, i16);
fn sum((ax, ay, az): V3) -> u16 {
    (ax.abs() + ay.abs() + az.abs()) as u16
}

fn add((ax, ay, az): V3, (bx, by, bz): V3) -> V3 {
    (ax + bx, ay + by, az + bz)
}

fn pull((ax, ay, az): V3, (bx, by, bz): V3) -> V3 {
    let x = (bx - ax).signum();
    let y = (by - ay).signum();
    let z = (bz - az).signum();
    (x, y, z)
}

fn gravity(moons: &[(V3, V3)]) -> Vec<(V3, V3)> {
    let mut result = vec![];

    for &moon in moons {
        let (position, velocity) = moon;
        let rest = moons.iter().filter(|&&m| m != moon);
        let dv = rest.map(|(p, _)| pull(position, *p)).fold((0, 0, 0), add);
        let velocity = add(velocity, dv);
        result.push((add(position, velocity), velocity))
    }

    result
}

fn total_energy(moons: &[V3], steps: u16) -> u16 {
    let mut moons = moons.iter().map(|&p| (p, (0, 0, 0))).collect::<Vec<_>>();
    for _ in 0..steps {
        moons = gravity(&moons);
    }
    moons.iter().map(|(p, v)| sum(*p) * sum(*v)).sum()
}

#[cfg(test)]
mod spec {
    use super::*;

    #[test]
    fn check_pull() {
        assert_eq!(pull((5, -1, 2), (0, 14, 2)), (-1, 1, 0));
    }

    #[test]
    fn check_gravity() {
        let moons = [
            ((-1, 0, 2), (0, 0, 0)),
            ((2, -10, -7), (0, 0, 0)),
            ((4, -8, 8), (0, 0, 0)),
            ((3, 5, -1), (0, 0, 0)),
        ];
        assert_eq!(
            gravity(&moons),
            [
                ((2, -1, 1), (3, -1, -1)),
                ((3, -7, -4), (1, 3, 3)),
                ((1, -7, 5), (-3, 1, -3)),
                ((2, 2, 0), (-1, -3, 1)),
            ]
        );
    }

    #[test]
    fn total_energy_10_steps() {
        assert_eq!(
            total_energy(&[(-1, 0, 2), (2, -10, -7), (4, -8, 8), (3, 5, -1)], 10),
            179
        );
    }

    #[test]
    fn total_energy_100_steps() {
        assert_eq!(
            total_energy(&[(-8, -10, 0), (5, 5, 10), (2, -7, 3), (9, -8, -3)], 100),
            1940
        );
    }

    #[test]
    fn part1_my_input() {
        assert_eq!(part1(INPUT), 7928);
    }
}
