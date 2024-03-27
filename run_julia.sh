#!/bin/bash
day=$(printf "%02d" ${1:-$(date +%-d)})
year=${2:-$(date +%Y)}

aocd $day $year | julia --project=. $year/day$day.jl
