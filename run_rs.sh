#!/bin/bash
. run_utils.sh

aocd $day $year | rust-script $year/day$day.rs
