# Verification record — 2026-10-31

## Completed

- Active application, R modules and R scripts parse successfully in R 4.3.3.
- `scripts/check.R`: **35 passing assertions, zero failed, zero warnings, zero skipped**.
- Shiny server checks cover synthetic startup, company selection, empty selection, file upload, English status normalisation, date filtering and absent-model behaviour.
- Data tests cover multiline CSV records, R-escaped TSV quotes, required fields, invalid dates, invalid rating/target, resolution denominators, duplicates and spreadsheet export escaping.
- Original complaint CSV imports as **11,699 rows / 14 companies**, with zero exact duplicate rows, four missing cells and no unknown statuses.
- Prepared TXT imports as **8,574 rows / nine companies**, with zero exact duplicate rows, two missing cells and no unknown statuses.
- Synthetic demo regenerates byte-for-byte from `scripts/generate_demo.py`.
- HTTP startup check returned the application HTML and HTTP 200 for all 23 referenced local script/style assets.

The test environment used locally extracted Ubuntu R packages. An environment-only adjustment supplied jquerylib's installed version because the extracted packages were not registered in the system package database. This adjustment is not application code and is unnecessary for a normal CRAN installation.

## Not established

- Browser visual and interactive end-to-end verification was not completed: no working browser binary was available in this environment. Server-level reactive behaviour and HTTP resources were tested instead.
- The new training pipeline was parsed but not trained end-to-end. Required individual labels and timing/rating fields are absent from the supplied datasets. No new performance result is claimed.
- GitHub Actions configuration is included. Consult the pull request checks for current remote execution results. Repository write access was restored after an initial HTTP 403; the working branch is `portfolio-modernization`.
- Dependency versions have not been captured in a verified `renv.lock`; the README explains how to snapshot the installed environment. The initial install command resolves current CRAN packages.
- Hosting, authentication, load testing and production privacy controls are outside this local application package.

## Reproduce

```sh
Rscript scripts/install.R
Rscript scripts/check.R
Rscript -e "shiny::runApp('.', launch.browser = TRUE)"
```

Source documentation and structural dataset fingerprints are in `PROVENANCE.md` and `source-profile.json`. No historical consumer narratives are included in this package.
