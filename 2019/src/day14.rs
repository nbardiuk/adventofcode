use std::collections::HashMap;
use std::collections::HashSet;
pub const INPUT: &str = include_str!("../res/day14.txt");

pub fn part1(input: &str) -> usize {
    fn parse_pair(pair: &str) -> Option<(String, usize)> {
        if let [am, labl] = pair.split_whitespace().collect::<Vec<_>>()[..] {
            Some((labl.to_string(), am.parse().unwrap()))
        } else {
            None
        }
    };

    let data = input
        .lines()
        .filter_map(|line| {
            if let [ingredients, result] = line.split(" => ").collect::<Vec<_>>()[..] {
                parse_pair(result).map(|result| {
                    (
                        result,
                        ingredients.split(", ").filter_map(parse_pair).collect(),
                    )
                })
            } else {
                None
            }
        })
        .collect::<Vec<_>>();

    temp((String::from("FUEL"), 1), &data)
        .get("ORE")
        .cloned()
        .unwrap_or(0)
}

pub fn part2(input: &str) -> usize {
    fn parse_pair(pair: &str) -> Option<(String, usize)> {
        if let [am, labl] = pair.split_whitespace().collect::<Vec<_>>()[..] {
            Some((labl.to_string(), am.parse().unwrap()))
        } else {
            None
        }
    };

    let data = input
        .lines()
        .filter_map(|line| {
            if let [ingredients, result] = line.split(" => ").collect::<Vec<_>>()[..] {
                parse_pair(result).map(|result| {
                    (
                        result,
                        ingredients.split(", ").filter_map(parse_pair).collect(),
                    )
                })
            } else {
                None
            }
        })
        .collect::<Vec<_>>();

    let mut current = 1;
    let goal = 1_000000_000000_usize;
    let mut range_start = 1;
    let mut range_end = 100_000000;
    loop {
        if range_start + 1 == range_end {
            return range_start;
        }
        let result = temp((String::from("FUEL"), current), &data) .get("ORE") .cloned() .unwrap_or(0);
        if result > goal {
            range_end = current;
            current = range_start + (range_end - range_start) / 2;
        } else if result == goal {
            return current;
        } else {
            range_start = current;
            current = range_start + (range_end - range_start) / 2;
        }
    }
}

type Res = (String, usize);
type Ing = Vec<Res>;
fn temp((resource, amount): Res, data: &[(Res, Ing)]) -> HashMap<String, usize> {
    let mut ingredients: HashMap<String, usize> = data
        .iter()
        .find(|(r, _)| r.0 == resource)
        .map(|(_, i)| {
            i.iter()
                .map(|(n, v)| (n.clone(), v * amount))
                .collect::<HashMap<_, _>>()
        })
        .unwrap_or_else(HashMap::new);
    let mut leftovers = HashMap::new();
    loop {
        if ingredients.len() == 1 {
            break;
        }
        if let Some((name, need)) = ingredients
            .clone()
            .iter()
            .max_by_key(|(n, _)| unique_elements(n, data).len())
        {
            let (left, used) = data
                .iter()
                .find(|(a, _)| name == &a.0)
                .map(|((_, produces), i)| {
                    let mut k = need / produces;
                    if need % produces != 0 {
                        k += 1;
                    }
                    (
                        (name, (produces * k) - need),
                        i.iter()
                            .map(|(a, b)| (a.clone(), k * *b))
                            .collect::<Vec<_>>(),
                    )
                })
                .unwrap_or(((name, 0), vec![]));
            leftovers.insert(left.0.to_string(), left.1);
            ingredients.remove(name);
            for (n, a) in used {
                ingredients.entry(n).and_modify(|ea| *ea += a).or_insert(a);
            }
        } else {
            break;
        }
    }
    ingredients
}

fn unique_elements(name: &str, data: &[(Res, Ing)]) -> HashSet<String> {
    let mut result = HashSet::new();
    let ingredients = data
        .iter()
        .find(|(r, _)| r.0 == name)
        .map(|(_, i)| i)
        .cloned()
        .unwrap_or(vec![]);
    for (n, _) in ingredients {
        if result.insert(n.to_string()) {
            result.extend(unique_elements(&n, data));
        }
    }
    result
}

#[cfg(test)]
mod spec {
    use super::*;

    #[test]
    fn part1_example1() {
        assert_eq!(
            part1(
                "10 ORE => 10 A\n\
                 1 ORE => 1 B\n\
                 7 A, 1 B => 1 C\n\
                 7 A, 1 C => 1 D\n\
                 7 A, 1 D => 1 E\n\
                 7 A, 1 E => 1 FUEL"
            ),
            31
        );
    }

    #[test]
    fn part1_example2() {
        assert_eq!(
            part1(
                "9 ORE => 2 A\n\
                 8 ORE => 3 B\n\
                 7 ORE => 5 C\n\
                 3 A, 4 B => 1 AB\n\
                 5 B, 7 C => 1 BC\n\
                 4 C, 1 A => 1 CA\n\
                 2 AB, 3 BC, 4 CA => 1 FUEL"
            ),
            165
        );
    }

    #[test]
    fn part1_example_13312() {
        assert_eq!(
            part1(
                "157 ORE => 5 NZVS\n\
                 165 ORE => 6 DCFZ\n\
                 44 XJWVT, 5 KHKGT, 1 QDVJ, 29 NZVS, 9 GPVTF, 48 HKGWZ => 1 FUEL\n\
                 12 HKGWZ, 1 GPVTF, 8 PSHF => 9 QDVJ\n\
                 179 ORE => 7 PSHF\n\
                 177 ORE => 5 HKGWZ\n\
                 7 DCFZ, 7 PSHF => 2 XJWVT\n\
                 165 ORE => 2 GPVTF\n\
                 3 DCFZ, 7 NZVS, 5 HKGWZ, 10 PSHF => 8 KHKGT"
            ),
            13312
        );
    }

    #[test]
    fn part2_example_13312() {
        assert_eq!(
            part2(
                "157 ORE => 5 NZVS\n\
                 165 ORE => 6 DCFZ\n\
                 44 XJWVT, 5 KHKGT, 1 QDVJ, 29 NZVS, 9 GPVTF, 48 HKGWZ => 1 FUEL\n\
                 12 HKGWZ, 1 GPVTF, 8 PSHF => 9 QDVJ\n\
                 179 ORE => 7 PSHF\n\
                 177 ORE => 5 HKGWZ\n\
                 7 DCFZ, 7 PSHF => 2 XJWVT\n\
                 165 ORE => 2 GPVTF\n\
                 3 DCFZ, 7 NZVS, 5 HKGWZ, 10 PSHF => 8 KHKGT"
            ),
            82892753
        );
    }

    #[test]
    fn part1_example_180697() {
        assert_eq!(
            part1(
                "2 VPVL, 7 FWMGM, 2 CXFTF, 11 MNCFX => 1 STKFG\n\
                 17 NVRVD, 3 JNWZP => 8 VPVL\n\
                 53 STKFG, 6 MNCFX, 46 VJHF, 81 HVMC, 68 CXFTF, 25 GNMV => 1 FUEL\n\
                 22 VJHF, 37 MNCFX => 5 FWMGM\n\
                 139 ORE => 4 NVRVD\n\
                 144 ORE => 7 JNWZP\n\
                 5 MNCFX, 7 RFSQX, 2 FWMGM, 2 VPVL, 19 CXFTF => 3 HVMC\n\
                 5 VJHF, 7 MNCFX, 9 VPVL, 37 CXFTF => 6 GNMV\n\
                 145 ORE => 6 MNCFX\n\
                 1 NVRVD => 8 CXFTF\n\
                 1 VJHF, 6 MNCFX => 4 RFSQX\n\
                 176 ORE => 6 VJHF"
            ),
            180697
        );
    }

    #[test]
    fn part2_example_180697() {
        assert_eq!(
            part2(
                "2 VPVL, 7 FWMGM, 2 CXFTF, 11 MNCFX => 1 STKFG\n\
                 17 NVRVD, 3 JNWZP => 8 VPVL\n\
                 53 STKFG, 6 MNCFX, 46 VJHF, 81 HVMC, 68 CXFTF, 25 GNMV => 1 FUEL\n\
                 22 VJHF, 37 MNCFX => 5 FWMGM\n\
                 139 ORE => 4 NVRVD\n\
                 144 ORE => 7 JNWZP\n\
                 5 MNCFX, 7 RFSQX, 2 FWMGM, 2 VPVL, 19 CXFTF => 3 HVMC\n\
                 5 VJHF, 7 MNCFX, 9 VPVL, 37 CXFTF => 6 GNMV\n\
                 145 ORE => 6 MNCFX\n\
                 1 NVRVD => 8 CXFTF\n\
                 1 VJHF, 6 MNCFX => 4 RFSQX\n\
                 176 ORE => 6 VJHF"
            ),
            5586022
        );
    }

    #[test]
    fn part1_example_2210736() {
        assert_eq!(
            part1(
                "171 ORE => 8 CNZTR\n\
                 7 ZLQW, 3 BMBT, 9 XCVML, 26 XMNCP, 1 WPTQ, 2 MZWV, 1 RJRHP => 4 PLWSL\n\
                 114 ORE => 4 BHXH\n\
                 14 VRPVC => 6 BMBT\n\
                 6 BHXH, 18 KTJDG, 12 WPTQ, 7 PLWSL, 31 FHTLT, 37 ZDVW => 1 FUEL\n\
                 6 WPTQ, 2 BMBT, 8 ZLQW, 18 KTJDG, 1 XMNCP, 6 MZWV, 1 RJRHP => 6 FHTLT\n\
                 15 XDBXC, 2 LTCX, 1 VRPVC => 6 ZLQW\n\
                 13 WPTQ, 10 LTCX, 3 RJRHP, 14 XMNCP, 2 MZWV, 1 ZLQW => 1 ZDVW\n\
                 5 BMBT => 4 WPTQ\n\
                 189 ORE => 9 KTJDG\n\
                 1 MZWV, 17 XDBXC, 3 XCVML => 2 XMNCP\n\
                 12 VRPVC, 27 CNZTR => 2 XDBXC\n\
                 15 KTJDG, 12 BHXH => 5 XCVML\n\
                 3 BHXH, 2 VRPVC => 7 MZWV\n\
                 121 ORE => 7 VRPVC\n\
                 7 XCVML => 6 RJRHP\n\
                 5 BHXH, 4 VRPVC => 5 LTCX"
            ),
            2210736
        );
    }

    #[test]
    fn part2_example_2210736() {
        assert_eq!(
            part2(
                "171 ORE => 8 CNZTR\n\
                 7 ZLQW, 3 BMBT, 9 XCVML, 26 XMNCP, 1 WPTQ, 2 MZWV, 1 RJRHP => 4 PLWSL\n\
                 114 ORE => 4 BHXH\n\
                 14 VRPVC => 6 BMBT\n\
                 6 BHXH, 18 KTJDG, 12 WPTQ, 7 PLWSL, 31 FHTLT, 37 ZDVW => 1 FUEL\n\
                 6 WPTQ, 2 BMBT, 8 ZLQW, 18 KTJDG, 1 XMNCP, 6 MZWV, 1 RJRHP => 6 FHTLT\n\
                 15 XDBXC, 2 LTCX, 1 VRPVC => 6 ZLQW\n\
                 13 WPTQ, 10 LTCX, 3 RJRHP, 14 XMNCP, 2 MZWV, 1 ZLQW => 1 ZDVW\n\
                 5 BMBT => 4 WPTQ\n\
                 189 ORE => 9 KTJDG\n\
                 1 MZWV, 17 XDBXC, 3 XCVML => 2 XMNCP\n\
                 12 VRPVC, 27 CNZTR => 2 XDBXC\n\
                 15 KTJDG, 12 BHXH => 5 XCVML\n\
                 3 BHXH, 2 VRPVC => 7 MZWV\n\
                 121 ORE => 7 VRPVC\n\
                 7 XCVML => 6 RJRHP\n\
                 5 BHXH, 4 VRPVC => 5 LTCX"
            ),
            460664
        );
    }

    #[test]
    fn part1_my_input() {
        assert_eq!(part1(INPUT), 612880);
    }

     #[test]
    fn part2_my_input() {
        assert_eq!(part2(INPUT), 2509120);
    }
}
