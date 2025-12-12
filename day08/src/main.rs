use std::collections::LinkedList;

fn main() {
    let input = std::fs::read_to_string("input").unwrap();
    let input = parse_input(&input);
    let solution = part1(&input, 1000);
    println!("{}", solution);
    let solution = part2(&input);
    println!("{}", solution);
}

type Distance = i64;
type Circuit = LinkedList<usize>;

fn part2(points: &[Point]) -> i64 {
    let (_, ci, cj) = circuits(points, 0, true);
    points[ci].x * points[cj].x
}

fn part1(points: &[Point], iterations: usize) -> usize {
    let (mut cs, _, _) = circuits(points, iterations, false);
    cs.sort_by_key(|circuit| circuit.len());
    cs.reverse();
    cs[0].len() * cs[1].len() * cs[2].len()
}

// ugly ahh code
fn circuits(
    points: &[Point],
    mut iterations: usize,
    to_one_group: bool,
) -> (Vec<Circuit>, usize, usize) {
    let mut distances: Vec<_> = points
        .iter()
        .enumerate()
        .flat_map(|(i, point)| {
            points
                .iter()
                .enumerate()
                .skip(i + 1)
                .map(move |(j, p)| (i, j, point.distance_squared(p)))
        })
        .collect();

    distances.sort_by_key(|(_, _, dist)| *dist);
    distances.reverse(); // make the smallest on the back

    let mut circuits: Vec<Circuit> = (0..points.len()).map(|n| Circuit::from([n])).collect();

    fn find_circuit(circuits: &[Circuit], target: usize) -> usize {
        // unwrap(): circuit for a target should always exist (if target is a Point index)
        circuits.iter().position(|c| c.contains(&target)).unwrap()
    }

    let mut i = 0;
    let mut j = 0;
    while to_one_group && circuits.len() > 1 || iterations > 0 {
        iterations = iterations.saturating_sub(1);
        (i, j, _) = distances.pop().unwrap();
        let ci = find_circuit(&circuits, i);
        let cj = find_circuit(&circuits, j);

        if ci != cj {
            let l = ci.min(cj);
            let h = ci.max(cj);
            let mut other_group = circuits.swap_remove(h);
            let group = &mut circuits[l];
            group.append(&mut other_group);
        }
    }

    (circuits, i, j)
}

fn parse_input(input: &str) -> Vec<Point> {
    input
        .lines()
        .map(|line| {
            let mut iter = line.trim().split(',').map(|sect| sect.parse().unwrap());
            Point::new(
                iter.next().unwrap(),
                iter.next().unwrap(),
                iter.next().unwrap(),
            )
        })
        .collect()
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct Point {
    pub x: i64,
    pub y: i64,
    pub z: i64,
}

impl Point {
    pub fn new(x: i64, y: i64, z: i64) -> Self {
        Self { x, y, z }
    }

    pub fn distance_squared(&self, other: &Point) -> Distance {
        let dx = self.x - other.x;
        let dy = self.y - other.y;
        let dz = self.z - other.z;
        dx * dx + dy * dy + dz * dz
    }
}
