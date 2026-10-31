# Consumer Complaint Intelligence

**Author: Enilton de Oliveira Machado**

An R/Shiny portfolio application for exploring consumer complaint patterns, validating uploaded data, and scoring declared return intent with a compatible trained model. Originally developed during the FIAP Big Data MBA in 2021; modernised with explicit data contracts, modular source, automated tests and reproducible training scripts.

## Try it locally

Requires R >= 4.3. Run these commands from the repository root:

```sh
Rscript scripts/install.R
Rscript -e "shiny::runApp('.', launch.browser = TRUE)"
```

The app opens with **600 synthetic complaints across four fictional companies**. No API key, original dataset or pretrained model is required for the dashboard. The interface is in English and accepts the original Portuguese column names.

## What works

- CSV/TSV upload with encoding and delimiter selection; 25 MB / 100,000 row limits.
- Company/date filters, complaint counts, status breakdown, monthly timeline and word frequencies.
- Explicit resolution denominators: all complaints versus evaluated complaints.
- Quality report for duplicates, missing cells and unexpected statuses.
- Filtered CSV download with spreadsheet-formula escaping.
- Optional batch predictions from a locally trained compatible model; download reports P(Yes).
- Contract tests and Shiny server tests, plus a GitHub Actions workflow.

## Data contract

Minimum: `company` and `status` (or `Empresa` and `Status`). Optional: `date`/`Data`, `text`/`Reclamacao`, `city`/`Localidade`, `state`/`Estado`, `title`/`Titulo`. Dates accept `DD/MM/YYYY` and `YYYY-MM-DD`. See [the full contract](docs/DATA_CONTRACT.md).

Upload files from your machine through the app. Do not commit original consumer narratives. Files are processed in the Shiny session and are not written into the repository; standard temporary-upload handling still occurs on the server. Public hosting requires an explicit operational privacy review before accepting real data.

## Train a model

The supplied historical complaint files do **not** contain the individual target and all predictors. Prediction is intentionally unavailable until a compatible model is trained. Aggregated company percentages cannot substitute for individual labels.

```sh
Rscript scripts/install.R --training
Rscript scripts/train.R data/raw/training.csv
```

Required predictors: `segment`, `status`, `state`, `days_response`, `days_resolution`, `rating`; target: `return_intent` (Yes/No or Sim/Não). Training compares logistic regression and XGBoost using five-fold cross-validation, chooses the algorithm on training data only, evaluates a separate holdout, then refits the selected workflow. Dates enable a temporal holdout. Reports and model artifacts remain local by default. See [model card](docs/MODEL_CARD.md).

**This is retrospective return intent after service, not actual repurchase, early churn prediction or evidence of causal impact.** Historical reported metrics are not claimed as performance of this edition.

## Tests and environment

```sh
Rscript -e 'testthat::test_dir("tests/testthat", stop_on_failure = TRUE)'
```

The install script obtains CRAN packages; it does not claim a locked environment. After verifying your complete application and optional training environment, record the actual resolved versions:

```r
renv::init(bare = TRUE)
renv::snapshot(type = "all")
```

Commit the resulting `renv.lock` and bootstrap files, then use `renv::restore()` on another machine. No fabricated lockfile is provided.

## Repository layout

| Path | Purpose |
|---|---|
| `app.R` | Shiny interface and session lifecycle |
| `R/` | Data contracts, transformations and model adapter |
| `scripts/` | Dependency setup and training |
| `data/demo/` | Explicitly synthetic demonstration |
| `data/raw/` | Local input only; ignored by Git |
| `models/`, `reports/` | Locally generated artifacts |
| `tests/testthat/` | Contract and Shiny regression checks |
| `docs/` | Architecture, model card, provenance and data dictionary |
| `legacy/` | Original 2021 source, retained for provenance; not the active app |

## Evidence and attribution

Author reports sole implementation of the original academic work. This repository documents software and evaluation evidence; it does not claim production adoption, commercial impact or immigration eligibility. New improvements are recorded separately in Git history and [CHANGELOG.md](CHANGELOG.md).

Project data remain subject to their original source terms. No redistribution licence is asserted for historical datasets. The project currently has no open-source licence grant.

Technical references: [Shiny](https://shiny.posit.co/r/getstarted/shiny-basics/lesson1/), [tidymodels](https://www.tidymodels.org/start/), [XGBoost](https://xgboost.readthedocs.io/), [renv](https://rstudio.github.io/renv/).
