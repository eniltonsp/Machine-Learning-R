# Model artifacts

Run `Rscript scripts/train.R data/raw/training.csv` after installing training dependencies. The resulting `model.rds` is a trusted local workflow bundle, not committed by default. Models from the original notebooks use a different format and are not silently loaded. Never deserialize untrusted RDS files.
