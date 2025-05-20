#!/usr/bin/env bash

# sum_last_column.sh: Sum the last column of a delimited file.
# Usage: sum_last_column.sh [-f FILE] [-d DELIM]
#   -f | --file      Path to input file (required)
#   -d | --delim     Field delimiter (default: '|')
#   -h | --help      Show this help message and exit

set -euo pipefail
IFS=$'\n\t'

print_usage() {
  grep '^#' "$0" | sed 's/^#//'
}

# Parse command-line arguments
file=""
delim="|"
while [[ $# -gt 0 ]]; do
  case "$1" in
    -f|--file)  file="$2"; shift 2 ;;
    -d|--delim) delim="$2"; shift 2 ;;
    -h|--help)  print_usage; exit 0 ;;
    *)          echo "Unknown option: $1" >&2; print_usage; exit 1 ;;
  esac
done

# Validate inputs
if [[ -z "$file" ]]; then
  echo "Error: --file is required." >&2
  print_usage
  exit 1
fi
if [[ ! -r "$file" ]]; then
  echo "Error: cannot read file '$file'." >&2
  exit 1
fi

# Compute sum of last column
sum=$(awk -F"$delim" '{ sum += $NF } END { printf "%g\n", sum }' "$file")

# Output result
echo "Sum of last column in '$file' (delimiter='$delim'): $sum"

