use std::cmp::{max, min};
use std::io;
use std::ops::RangeInclusive;

pub fn main() {
    let (id_ranges, available_ids) = parse_input();
    println!("{}", count_fresh(&available_ids, &id_ranges));
    println!("{}", count_all_fresh(&id_ranges));
}

fn count_fresh(available_ids: &[u64], id_ranges: &[RangeInclusive<u64>]) -> usize {
    available_ids.iter().filter(|&&id| is_fresh(id, &id_ranges)).count()
}

fn count_all_fresh(id_ranges: &[RangeInclusive<u64>]) -> u64 {
    let merged = merge_intervals(id_ranges);
    merged.iter().map(|r| r.end() - r.start() + 1).sum()
}

fn merge_intervals(intervals: &[RangeInclusive<u64>]) -> Vec<RangeInclusive<u64>> {
    let mut sorted_intervals = SortedIntervals { intervals: Vec::new() };
    for interval in intervals {
        sorted_intervals.insert(interval.to_owned());
    }
    sorted_intervals.intervals
}

struct SortedIntervals {
    intervals: Vec<RangeInclusive<u64>>,
}

impl SortedIntervals {
    fn insert(&mut self, interval: RangeInclusive<u64>) {
        let index = self.find_index(*interval.start());
        if index == self.intervals.len() {
            self.intervals.push(interval);
        } else {
            let this_interval = &self.intervals[index];
            if is_overlap(this_interval, &interval) {
                self.intervals[index] = merge_two_intervals(this_interval, &interval);
                while index < self.intervals.len() - 1 {
                    let next_interval = &self.intervals[index + 1];
                    if is_overlap(next_interval, &interval) {
                        self.intervals[index] = merge_two_intervals(&self.intervals[index], next_interval);
                        self.intervals.remove(index + 1);
                    } else {
                        break;
                    }
                }
            } else {
                self.intervals.insert(index, interval);
            }
        }
    }

    fn find_index(&self, n: u64) -> usize {
        let mut min_index = 0;
        let mut max_index = self.intervals.len();
        while min_index < max_index {
            let middle = (min_index + max_index) / 2;
            let middle_interval = &self.intervals[middle];
            if n > *middle_interval.end() {
                min_index = middle + 1;
            } else {
                max_index = middle;
            }
        }
        min_index
    }
}

fn is_overlap(range1: &RangeInclusive<u64>, range2: &RangeInclusive<u64>) -> bool {
    if range1.start() <= range2.start() {
        range1.end() >= range2.start()
    } else {
        range2.end() >= range1.start()
    }
}

fn merge_two_intervals(range1: &RangeInclusive<u64>, range2: &RangeInclusive<u64>) -> RangeInclusive<u64> {
    *min(range1.start(), range2.start()) ..= *max(range1.end(), range2.end())
}

fn is_fresh(id: u64, id_ranges: &[RangeInclusive<u64>]) -> bool {
    id_ranges.iter().any(|range| range.contains(&id))
}

fn parse_input() -> (Vec<RangeInclusive<u64>>, Vec<u64>) {
    let mut id_ranges: Vec<RangeInclusive<u64>> = Vec::new();
    let mut available_ids: Vec<u64> = Vec::new();
    for line in io::stdin().lines() {
        let raw_line = line.unwrap();
        let trimmed = raw_line.trim_end();
        if trimmed.contains('-') {
            let nums: Vec<u64> = trimmed.split('-').map(|s| s.parse().unwrap()).collect();
            id_ranges.push(nums[0]..=nums[1]);
        } else if !trimmed.is_empty() {
            available_ids.push(trimmed.parse().unwrap());
        }
    }
    (id_ranges, available_ids)
}
