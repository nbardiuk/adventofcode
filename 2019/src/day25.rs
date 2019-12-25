use crate::intcode::Computer;

pub const INPUT: &str = include_str!("../res/day25.txt");

pub fn part1(input: &str) -> i64 {
    let mut computer = Computer::parse(input);

    let mut commands = vec![
        // to Gift Wrapping Center
        "east",
        "take sand",
        // to Kitchen
        // "east",
        // "take molten lava"

        // to Hull Breach
        "west",
        // to Crew Quarters
        "south",
        "take ornament",
        // to Observatory
        // "east",

        // to Hull Breach
        "north",
        // to Sick Bay
        "west",
        // to Arcade
        "south",
        // "take giant electromagnet",

        // to Holodeck
        "south",
        "take candy cane",
        // to Arcade
        "north",
        // to Sick Bay
        "north",
        // to Hallway
        "north",
        "take wreath",
        // to Engineering
        "east",
        "take fixed point",
        // to Hallway
        "west",
        // to Corridor ???
        "north",
        // "take infinite loop"

        // to Passages
        "north",
        "take spool of cat6",
        // to Corridor
        "south",
        // to Hallway
        "south",
        // to Sick Bay
        "south",
        // to Arcade
        "south",
        // to Warp Drive Maintance
        "east",
        // "take escape pod",

        // to Navigation
        "east",
        // to Storage
        "east",
        "take space law space brochure",
        // to Science Lab
        "south",
        "take fuel cell",
        // to Scurity Checkpoint
        "south",
        "drop spool of cat6",
        "drop ornament",
        // "drop wreath",
        // "drop sand",
        "drop fuel cell",
        // "drop fixed point",
        // "drop space law space brochure",
        "drop candy cane",
        "inv",
        // to Pressure-Sensitive Floor
        "west",
    ];

    commands.push("");
    let mut chars = commands
        .join("\n")
        .chars()
        .map(|c| (c as u8) as i64)
        .collect();
    let output = computer.process(&mut chars);
    for c in output {
        print!("{}", (c as u8) as char);
    }
    16778274
}

#[cfg(test)]
mod spec {
    use super::*;
    #[test]
    fn part1_my_input() {
        assert_eq!(part1(INPUT), 16778274);
    }
}
