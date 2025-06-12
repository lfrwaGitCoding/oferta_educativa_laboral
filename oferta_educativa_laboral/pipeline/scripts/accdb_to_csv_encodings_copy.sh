#!/usr/bin/env bash

# Export each table from an Access .accdb file to a UTF-8 encoded CSV
# Run as:
#./accdb_to_csv.sh your_file.accdb

# Output:
# For each table t, e.g.:
# Qna 07 Bienestar 2025 will be Qna_07_Bienestar_2025.csv

# Try exporting each table assuming cp1252
# If cp1252 decoding fails, retry assuming UTF-8
# Log which tables failed cp1252
# Output clean UTF-8 CSVs for all successfully processed tables


set -euo pipefail

# Encoding:
MDB_JET3_CHARSET="utf-8"    # Primary guess (Western European Windows)
FALLBACK_CHARSET="cp1252"     # Fallback if cp1252 fails

# DB and tables:
DB_FILE="$1"
OUTDIR="${2:-.}"

# Output dir, but not using:
mkdir -p "$OUTDIR"

# Log if encodings fail:
LOGFILE="$OUTDIR/failed_tables.log"
: > "$LOGFILE"   # empty the logfile

# Get tables safely:
mapfile -t TABLES < <(mdb-tables -1 "$DB_FILE")

for t in "${TABLES[@]}"; do
    echo "Exporting table: $t..." >&2
    SAFE_NAME=$(echo "$t" | tr ' /' '_')
    TMPFILE="$(mktemp)"

    # Try cp1252, best for Spanish, but will use the var set for encoding:
    if mdb-export -D '%Y-%m-%d %H:%M:%S' -d ',' -q '"' "$DB_FILE" "$t" \
        | iconv -f "$MDB_JET3_CHARSET" -t utf-8//IGNORE > "$TMPFILE"; then
        echo "OK with $MDB_JET3_CHARSET" >&2
    else
        echo "cp1252 failed, trying $FALLBACK_CHARSET..." >&2
        echo "$t" >> "$LOGFILE"

        # Retry with fallback encoding:
        if mdb-export -D '%Y-%m-%d %H:%M:%S' -d ',' -q '"' "$DB_FILE" "$t" \
            | iconv -f "$FALLBACK_CHARSET" -t utf-8//IGNORE > "$TMPFILE"; then
            echo "OK with $FALLBACK_CHARSET" >&2
        else
            echo "FAILED: Could not export $t with $FALLBACK_CHARSET" >&2
            rm -f "$TMPFILE"
            continue
        fi
    fi

    # Move temporary output to final file:
    mv "$TMPFILE" "$OUTDIR/${SAFE_NAME}.csv"
done

echo "Done." >&2
echo "Tables needing $FALLBACK_CHARSET decoding are in: $LOGFILE" >&2

