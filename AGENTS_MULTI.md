# AGENTS.md

This file guides AI agents (e.g. OpenAI Codex) and human contributors when working on this multi-language data-science project.
A nested `AGENTS.md` in any subfolder will override the root settings for that scope.

---

## 📁 Project Structure

| Path                  | Purpose                                                            |
|-----------------------|--------------------------------------------------------------------|
| `scripts/`            | Mainly R code, `*.R` scripts, roxygen2 documentation               |
| `pipeline/`           | Mainly Python code, uses cgat-core and Ruffus, `*.py` scripts      |
| `bash/`               | If present, shell utilities, ETL scripts (`set -euo pipefail`)     |
| `sql/`                | If present, SQL files: DDL, DML, views, stored procedures          |
| `tests/`              | If present, unit tests (`testthat` for R, `pytest` for Python, `bats` for Bash) |
| `legacy/`             | Leave alone largely, simply contains freezes                       |
| `report/`             | quarto scripts for automatic reporting                             |


---

## Coding Conventions

- **R**  
  - `snake_case`; 2-space indent; no tabs  
  - Document with roxygen2; enforce style with `styler::style_pkg()` and `lintr::lint_package()`

- **Python**  
  - `snake_case`; 4-space indent; use type hints (PEP 484)  
  - Format with `black .`; lint with `flake8`  

- **Bash**  
  - At top: `#!/usr/bin/env bash` + `set -euo pipefail`  
  - Write reusable functions; parse args via `getopts`  
  - Provide `--help` usage text  

- **SQL**  
  - Keywords uppercase; identifiers lowercase  
  - Terminate statements with semicolon  
  - Modularize queries; avoid vendor-specific extensions unless necessary  

---

## Testing Protocols

- **R**:  
  ```bash
  Rscript -e "devtools::test(reporter = 'summary')"
  Rscript -e "covr::report()"
  ```

- **Python**:  
  ```bash
  pytest --maxfail=1 --disable-warnings -q
  coverage run -m pytest && coverage report
  ```

- **Bash**:  
  ```bash
  shellcheck bash/*.sh
  bats tests/bash
  ```

- **SQL**:  
  - Integrate `pgTAP`, or validate queries via Python DB fixtures  

---

## Documentation and Build

- **R**  
  ```bash
  Rscript -e "devtools::document()"
  R CMD build . --compact-vignettes=gs+qpdf
  R CMD check --no-manual --as-cran
  ```

- **Python**  
  ```bash
  sphinx-build docs/ docs/_build/html
  ```

- **Combined**  
  ```makefile
  make docs   # calls R + Python docs builders
  make build  # builds packages and containers
  ```

---

## Pull Request Guidelines

- **Branch naming**:  
  `feature/<lang>/<short-description>` or `bugfix/<lang>/<short-description>`

- **PR title**:  
  `[LANG][TYPE] Brief summary`  
  e.g. `[R][Feature] Add epi_stats_dates_multi()`

- **PR description**:  
  1. Scope and rationale of changes  
  2. Tests added/modified  
  3. Impact on outputs or downstream processes  

---

## Programmatic Checks

Before opening a PR, run:

<!--- ```bash
make lint    # runs linters across R, Python, Bash, SQL
make test    # runs all tests
make build   # builds and checks packages
```
--->

---

## Dependency Management

<!--- - **R**: use `renv`; declare in `DESCRIPTION`  --->
- **Python**: `requirements.txt` + `pipenv`/`poetry`
- **Bash & SQL**: document in README; pin CLI tool versions  

---

## Agent Instructions

When generating or refactoring code, AI agents must:

1. Follow per-language style rules above.  
2. Write or update tests covering new code.  
3. Update documentation (`roxygen2`, Sphinx, README).  
4. Respect CI commands under **Programmatic Checks**.  

---

_Last updated: 2025-06-11_  
_Maintainer: Antonio J. Berlanga-Taylor (github.com/AntonioJBT)_  
