if [[ $# = 0 ]]; then
    day=$(date +%d)
    year=$(date +%Y)
elif [[ $# = 1 ]]; then
    day=$(printf "%02d" "$1")
    year=$(date +%Y)
else
    day=$(printf "%02d" "$2")
    year="$1"
fi
