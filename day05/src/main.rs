fn main() {
    let content = std::fs::read_to_string("input").unwrap();
    let input = parse_input(&content).unwrap();

    println!("{}", part1(&input));
    println!("{}", part2(&input));
}

type Values = Vec<u64>;
type Input = (Vec<(u64, u64)>, Values);

fn part1((ranges_pairs, values): &Input) -> usize {
    values
        .iter()
        .filter(|&value| {
            ranges_pairs
                .iter()
                .any(|(start, end)| start <= value && value <= end)
        })
        .count()
}

fn part2((ranges_pairs, _): &Input) -> u64 {
    let mut ranges_pairs = ranges_pairs.clone();

    // sort in reverse order by the amount that the ranges occupy (from highest to lowest).
    // this is to prevent a case where a contained range appears before a range that contains it.
    ranges_pairs.sort_unstable_by_key(|(a, b)| b - a);
    ranges_pairs.reverse();

    // should filter out all ranges that are contained by another range
    let mut new_ranges = Vec::new();

    for (start, end) in ranges_pairs {
        // if any of the existing ranges contains this range
        // stupid filtering but fast enough
        if new_ranges.iter().any(|&(start1, end1)| start1 <= start && end <= end1) {
            continue
        }

        new_ranges.push((start, end));
    }

    new_ranges.sort();

    let mut total = 0;
    let mut old_end = 0;
    for (start, end) in new_ranges {
        total += end - start + 1;
        if start < old_end + 1 {
            let diff = old_end + 1 - start;
            // remove overlapping range
            total -= diff;
        }

        old_end = end;
    }

    total
}

fn parse_input(content: &str) -> Option<Input> {
    let (ranges_text, values_text) = content.split_once("\n\n")?;

    let ranges = ranges_text
        .lines()
        .map(|line| {
            let (start_text, end_text) = line.split_once('-')?;
            let start = start_text.parse().ok()?;
            let end = end_text.parse().ok()?;
            Some((start, end))
        })
        .collect::<Option<_>>()?;

    let values = values_text
        .lines()
        .map(|text| text.parse().ok())
        .collect::<Option<_>>()?;

    Some((ranges, values))
}
