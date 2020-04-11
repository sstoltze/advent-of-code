#![allow(dead_code)]

use itertools::Itertools;
use std::fs::File;
use std::io::{BufRead, BufReader};
use std::ops;
use std::str::FromStr;

fn main() {
    day1();
    day12();
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

#[derive(Clone, Debug, Copy)]
struct Point3D {
    x: i32,
    y: i32,
    z: i32,
}

impl ops::Add<Point3D> for Point3D {
    type Output = Point3D;

    fn add(self, other: Point3D) -> Point3D {
        Point3D {
            x: self.x + other.x,
            y: self.y + other.y,
            z: self.z + other.z,
        }
    }
}

impl ops::Sub<Point3D> for Point3D {
    type Output = Point3D;

    fn sub(self, other: Point3D) -> Point3D {
        Point3D {
            x: self.x - other.x,
            y: self.y - other.y,
            z: self.z - other.z,
        }
    }
}

impl Point3D {
    fn new(x: i32, y: i32, z: i32) -> Point3D {
        Point3D { x, y, z }
    }

    fn coord_compare(&self, other: &Point3D) -> Point3D {
        Point3D::new(
            (other.x - self.x).signum(),
            (other.y - self.y).signum(),
            (other.z - self.z).signum(),
        )
    }

    fn energy(&self) -> i32 {
        self.x.abs() + self.y.abs() + self.z.abs()
    }
}

#[derive(Clone, Debug)]
struct Moon {
    position: Point3D,
    velocity: Point3D,
}

impl FromStr for Moon {
    type Err = std::num::ParseIntError;

    fn from_str(moon_str: &str) -> Result<Self, Self::Err> {
        let mut coords = vec![];
        for c in moon_str.trim_matches(|c| c == '<' || c == '>').split(", ") {
            for (i, _) in c.trim_matches(',').match_indices("=") {
                let coord = c[i + 1..].parse::<i32>().expect("Not a number");
                coords.push(coord);
            }
        }
        if coords.len() < 3 {
            panic!(format!("Error parsing {} to Moon", moon_str))
        } else {
            Ok(Moon {
                position: Point3D::new(coords[0], coords[1], coords[2]),
                velocity: Point3D::new(0, 0, 0),
            })
        }
    }
}

impl Moon {
    fn total_energy(&self) -> i32 {
        self.position.energy() + self.velocity.energy()
    }
}

fn apply_gravity(m1: &mut Moon, m2: &mut Moon) {
    let dif = m1.position.coord_compare(&m2.position);
    m1.position = m1.position + dif.clone();
    m2.position = m2.position - dif.clone();
}

fn apply_velocity(mut m: Moon) -> Moon {
    m.position = m.position + m.velocity.clone();
    m
}

fn run_step(mut moons: Vec<Moon>) -> Vec<Moon> {
    let mut result = Vec::new();
    let mut temp = moons.clone();
    for m1 in moons.iter_mut() {
        for m2 in temp.iter_mut() {
            apply_gravity(m1, m2);
        }
    }

    for m in moons.iter() {
        result.push(apply_velocity(m.clone()));
    }
    result
}

fn day12() {
    let input_file = File::open("../../input/day12-1.txt").expect("Could not find file");
    let buf_reader = BufReader::new(input_file);

    let mut moons = Vec::new();

    for line in buf_reader.lines() {
        let m = Moon::from_str(&line.unwrap()).expect("Error");
        moons.push(m);
    }

    println!("Found {} moons", moons.len());

    run_step(moons);
}
