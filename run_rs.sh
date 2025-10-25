#!/bin/bash
. run_utils.sh

if [[ -r $year/Cargo.toml ]]; then
  cd $year
  aocd $day $year | cargo run -q $day
else
  aocd $day $year | rust-script $year/day$day.rs
fi
