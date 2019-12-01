#![allow(dead_code)]

use std::fs::File;
use std::io::{BufRead, BufReader};

fn main() {
    day1();
}

fn fuel(mass: u32) -> u32 {
    ((mass as f64 / 3.0).floor() - 2.0).max(0.0) as u32
}

fn total_fuel(mass: u32) -> u32 {
    let f = fuel(mass);
    if f <= 0 {
        0
    } else {
        f + total_fuel(f)
    }
}

fn day1() {
    let input_file = File::open("../../input/day1-1.txt").expect("Could not find file");
    let buf_reader = BufReader::new(input_file);

    let mut total = 0;
    let mut module_fuel = 0;

    for line in buf_reader.lines() {
        let mass = line.unwrap().parse::<u32>().expect("Could not read fuel");
        total += total_fuel(mass);
        module_fuel += fuel(mass);
    }

    println!("Fuel for modules: {}", module_fuel);
    println!("Total fuel: {}", total);
}
