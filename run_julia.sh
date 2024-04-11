#!/bin/bash
. run_utils.sh

aocd $day $year | julia --project=. $year/day$day.jl
