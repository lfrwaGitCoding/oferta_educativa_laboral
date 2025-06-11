# oferta_educativa_laboral

This repository contains scripts used to analyse the "Oferta Educativa" data.

## Scripts

All R scripts now live under [`scripts/`](scripts/) with descriptive
scripts in [`scripts/descriptive`](scripts/descriptive). The previous
copies from `pipeline/scripts/` and duplicated files under
`legacy/November_2024` have been removed.

Symlinked helper files have been replaced with placeholders:

- `scripts/0_project_setup.R`
- `scripts/grep_many_terms.sh`
- `scripts/specific_Qs/meds_por_dh/pda-2025-03-31.csv`

To run the analysis you must provide the real versions of these files.
For `0_project_setup.R` and `grep_many_terms.sh` fetch them from the
[`project_tools`](https://github.com/antoniojbt/project_tools) project.
The CSV contains restricted data and needs to be copied from your local
source.
