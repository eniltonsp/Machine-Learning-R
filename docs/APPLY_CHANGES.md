# Applying this review package

The proposed application is developed on `portfolio-modernization`. Review its pull request before merging into `main`. The original academic source is retained in `legacy/`.

This package contains the complete proposed application source. To review it locally:

1. Extract the package and open a terminal in its root directory.
2. Install R >= 4.3, then run `Rscript scripts/install.R`.
3. Run `Rscript scripts/check.R`.
4. Start the app with `Rscript -e "shiny::runApp('.', launch.browser = TRUE)"`.
5. Start with the synthetic demo, then upload authorised complaint-level data.

To apply it to the existing repository, create a branch, copy the package contents into its root, and remove the three old root entry points (their copies are already in `legacy/`):

- `ReclameAqui-Machine-Learning.Rmd`
- `ReclameAqui Machine Learning por Empresa.Rmd`
- `WebAppReclameAqui.R`

Review the changes in a pull request before merging. Never add the uploaded raw datasets, local model artifacts or reports containing sensitive data to the public repository.
