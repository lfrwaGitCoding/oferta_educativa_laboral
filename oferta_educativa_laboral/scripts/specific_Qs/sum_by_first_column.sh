#!/usr/bin/env bash
# sum_by_first_column.sh: Group by the 1st column and sum the last column.
#
# Usage:
#   sum_by_first_column.sh --file FILE [--delim DELIM]
#   sum_by_first_column.sh -h|--help
#
# Options:
#   -f, --file    Path to input file (required)
#   -d, --delim   Field delimiter (default: '|')
#   -h, --help    Show this message

set -euo pipefail
IFS=$'\n\t'

print_usage() {
  grep '^#' "$0" | sed 's/^#//'
}

# Default delimiter
delim='|'
file=''

# Parse CLI args
while [[ $# -gt 0 ]]; do
  case $1 in
    -f|--file)  file="$2";   shift 2 ;;
    -d|--delim) delim="$2";  shift 2 ;;
    -h|--help)  print_usage; exit 0 ;;
    *)          echo "Unknown option: $1" >&2; print_usage; exit 1 ;;
  esac
done

# Validate
if [[ -z "$file" ]]; then
  echo "Error: --file is required." >&2
  print_usage; exit 1
fi
if [[ ! -r "$file" ]]; then
  echo "Error: cannot read '$file'." >&2
  exit 1
fi

# Headers
h1='original_col1'
h2='sum_col1'

# Aggregate with awk, then sort by the first field
awk -F"$delim" -v H1="$h1" -v H2="$h2" '
  { sum[$1] += $NF }
  END {
    # print header
    print H1, H2
    # print each group; for deterministic order, sort externally
    for (k in sum) printf "%s\t%.0f\n", k, sum[k]
  }
' OFS='\t' "$file" \
  | sort -k1,1

