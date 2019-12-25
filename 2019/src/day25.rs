use crate::intcode::Computer;

pub const INPUT: &str = include_str!("../res/day25.txt");

pub fn part1(input: &str) -> i64 {
    let mut inputs = vec![
        // Start at Hull Breach
        "east", //  to Gift Wrapping Center
        "take sand",
        "west",  // to Hull Breach
        "west",  // to Sick Bay
        "north", // to Hallway
        "take wreath",
        "east", //  to Engineering
        "take fixed point",
        "west",  // to Hallway
        "south", // to Sick Bay
        "south", // to Arcade
        "east",  // to Warp Drive Maintance
        "east",  // to Navigation
        "east",  // to Storage
        "take space law space brochure",
        "south", // to Science Lab
        "south", // to Scurity Checkpoint
        "west",  // to Pressure-Sensitive Floor
    ]
    .join("\n");
    inputs.push('\n');

    let mut encoded = inputs.chars().map(|c| (c as u8) as i64).collect::<Vec<_>>();
    let output = Computer::parse(input).process(&mut encoded);
    let text: String = output.iter().map(|i| (*i as u8) as char).collect();

    assert!(text.ends_with("\"Oh, hello! You should be able to get in by typing 16778274 on the keypad at the main airlock.\"\n"));
    text[text.len() - 45..][..8].parse().unwrap()
}

#[cfg(test)]
mod spec {
    use super::*;
    #[test]
    fn part1_my_input() {
        assert_eq!(part1(INPUT), 16778274);
    }
}
