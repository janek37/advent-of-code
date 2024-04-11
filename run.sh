#!/bin/bash
. run_utils.sh

aocd $day $year | python $year/day$day.py
