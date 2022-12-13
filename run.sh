#!/bin/bash
day=$(printf "%02d" ${1:-$(date +%d)})
year=2022

aocd $day $year | python day$day.py
