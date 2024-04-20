#!/bin/bash
. run_utils.sh

aocd $day $year | cabal v2-run aoc$year $day
