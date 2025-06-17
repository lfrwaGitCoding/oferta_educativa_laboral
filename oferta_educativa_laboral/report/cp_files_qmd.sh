#!/usr/bin/env bash
set -euo pipefail

# Copy or symlink supporting files for Quarto PDF generation.
# Usage: ./cp_files_qmd.sh [project_root]
# If no project root is provided, the script uses `git rev-parse`.

PROJECT_ROOT="${1:-$(git rev-parse --show-toplevel)}"
REPORT_DIR="$PROJECT_ROOT/oferta_educativa_laboral/report"

ln -s "$REPORT_DIR/_extended_appendices.qmd" .
ln -s "$REPORT_DIR/_extended_ord_meds_Qna_17_2024.qmd" .
ln -s "$REPORT_DIR"/*.yml .
ln -s "$REPORT_DIR/_report_outputs" .
ln -s "$REPORT_DIR/resources" .
ln -s "$REPORT_DIR/_scripts" .
cp "$REPORT_DIR/SIAP_desc_stats.qmd" .

# Example of copying additional figures (edit as needed)
# cp -f ../31_03_2025_medicos_por_mil_derechohabientes/plot_bar_* .

