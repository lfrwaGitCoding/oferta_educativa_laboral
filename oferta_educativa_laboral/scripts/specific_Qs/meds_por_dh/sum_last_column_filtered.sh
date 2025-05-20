#!/usr/bin/env bash
# sum_last_column_filtered.sh: Sum the last column of a delimited file,
#                             filtering only rows where a specified column equals a given value.
#
# Usage:
#   sum_last_column_filtered.sh \
#     --file FILE             Path to input file (required) \
#     --col INDEX             1-based column index to apply filter (default: 1) \
#     --value VALUE           Value to match in filter column (required) \
#     [--delim DELIM]         Field delimiter (default: '|') \
#   sum_last_column_filtered.sh --help

set -euo pipefail
IFS=$'\n\t'

print_usage() {
  grep '^#' "$0" | sed 's/^#//'
}

# Default parameters
file=""
filter_col=1
filter_val=""
delim="|"

# Parse arguments
while [[ $# -gt 0 ]]; do
  case "$1" in
    -f|--file)      file="$2";       shift 2 ;;
    -c|--col)       filter_col="$2"; shift 2 ;;
    -v|--value)     filter_val="$2"; shift 2 ;;
    -d|--delim)     delim="$2";      shift 2 ;;
    -h|--help)      print_usage; exit 0 ;;
    *) echo "Unknown option: $1" >&2; print_usage; exit 1 ;;
  esac
done

# Validate inputs
if [[ -z "$file" ]]; then
  echo "Error: --file is required." >&2
  print_usage; exit 1
fi
if [[ -z "$filter_val" ]]; then
  echo "Error: --value is required." >&2
  print_usage; exit 1
fi
if [[ ! -r "$file" ]]; then
  echo "Error: cannot read file '$file'." >&2
  exit 1
fi
if ! [[ "$filter_col" =~ ^[1-9][0-9]*$ ]]; then
  echo "Error: --col must be a positive integer." >&2
  exit 1
fi

# Compute sum
sum=$(
  awk -F"$delim" -v col="$filter_col" -v val="$filter_val" '
    $col == val { sum += $NF }
    END { printf "%.0f\n", sum }
  ' "$file"
)

# Output
echo "Sum of last column in '$file' (where column $filter_col == '$filter_val'): $sum"

