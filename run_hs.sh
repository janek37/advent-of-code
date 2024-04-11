#!/bin/bash
. run_utils.sh

aocd $day $year | runghc $year/day$day.hs
