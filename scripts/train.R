# Usage: Rscript scripts/train.R data/raw/training.csv [encoding] [delimiter]
# Run only on an authorised, individual-level labelled dataset.
library(tidymodels)
source("R/data.R")
source("R/model.R")
args <- commandArgs(trailingOnly = TRUE)
if (!length(args)) stop("Usage: Rscript scripts/train.R training.csv [encoding] [delimiter]")
encoding <- if (length(args) >= 2) args[2] else "UTF-8"
delim <- if (length(args) >= 3) args[3] else ","
d <- prepare_data(read_input(args[1], encoding, delim))
validate_features(d)
if (!"return_intent" %in% names(d) || anyNA(d$return_intent))
  stop("Training requires a complete individual-level return_intent target. Aggregated percentages cannot be used.")
if (anyDuplicated(d)) stop("Exact duplicate rows found. Investigate before splitting.")
if ("complaint_id" %in% names(d) && (anyNA(d$complaint_id) || anyDuplicated(d$complaint_id)))
  stop("Complaint IDs must be complete and unique to prevent cross-split duplication.")
if (nrow(d) < 200 || any(table(factor(d$return_intent, levels = c("No", "Yes"))) < 50))
  stop("Training requires at least 200 rows and 50 examples of each class.")
d$return_intent <- factor(d$return_intent, levels = c("No", "Yes"))
set.seed(20261031)
# A date-based holdout never splits a single date across train and test.
if ("date" %in% names(d)) {
  if (anyNA(d$date)) stop("Temporal evaluation requires complete dates.")
  unique_dates <- sort(unique(d$date))
  if (length(unique_dates) < 10) stop("At least 10 distinct dates are required for temporal evaluation.")
  cutoff <- unique_dates[max(1L, floor(length(unique_dates) * 0.7))]
  train_index <- which(d$date <= cutoff)
  test_index <- which(d$date > cutoff)
  split <- rsample::make_splits(list(analysis = train_index, assessment = test_index), d)
  split_method <- paste("Temporal holdout; training through", cutoff)
} else {
  warning("No date column: using random stratified holdout. This does not establish future-period performance.")
  split <- initial_split(d, prop = 0.7, strata = return_intent)
  split_method <- "Random stratified holdout; no temporal generalisation claim"
}
train <- training(split); test <- testing(split)
for (part in list(train, test)) if (any(table(part$return_intent) < 10))
  stop("Each split needs at least 10 examples of each class.")
# Exclude identifiers and text; fit preprocessing independently in each resample.
recipe_spec <- recipe(return_intent ~ segment + status + state + days_response + days_resolution + rating, data = train) |>
  step_unknown(all_nominal_predictors(), new_level = "missing") |>
  step_novel(all_nominal_predictors(), new_level = "new") |>
  step_impute_median(all_numeric_predictors()) |>
  step_dummy(all_nominal_predictors()) |>
  step_zv(all_predictors())
if (any(vapply(train[c("rating", "days_response", "days_resolution")], function(x) all(is.na(x)), logical(1))))
  stop("A numeric predictor is entirely missing in training.")
folds <- vfold_cv(train, v = 5, strata = return_intent)
# Explicit event: Yes (the second factor level).
auc_yes <- metric_tweak("roc_auc_yes", roc_auc, event_level = "second")
metrics <- metric_set(auc_yes)
baseline <- workflow() |> add_recipe(recipe_spec) |>
  add_model(logistic_reg() |> set_engine("glm") |> set_mode("classification"))
boosted <- workflow() |> add_recipe(recipe_spec) |>
  add_model(boost_tree(trees = 400, tree_depth = tune(), learn_rate = tune(), min_n = tune()) |>
    set_engine("xgboost", nthread = 1, seed = 20261031) |> set_mode("classification"))
baseline_cv <- fit_resamples(baseline, folds, metrics = metrics)
grid <- tidyr::expand_grid(tree_depth = c(2L, 5L), learn_rate = c(0.03, 0.1), min_n = c(10L, 30L))
tuned <- tune_grid(boosted, resamples = folds, grid = grid, metrics = metrics)
baseline_metrics <- collect_metrics(baseline_cv)
boost_metrics <- collect_metrics(tuned)
if (any(baseline_metrics$n != 5L) || any(boost_metrics$n != 5L))
  stop("Some resamples failed. Inspect collect_notes() before accepting a model.")
best <- select_best(tuned, metric = "roc_auc_yes")
best_auc <- show_best(tuned, metric = "roc_auc_yes", n = 1)$mean
base_auc <- baseline_metrics$mean[baseline_metrics$.metric == "roc_auc_yes"]
if (!is.finite(base_auc) || !is.finite(best_auc)) stop("Non-finite cross-validation results.")
selected <- if (best_auc > base_auc) finalize_workflow(boosted, best) else baseline
algorithm <- if (best_auc > base_auc) "XGBoost" else "Logistic regression"
heldout <- fit(selected, data = train)
prob <- predict(heldout, test, type = "prob")$.pred_Yes
cls <- predict(heldout, test)$.pred_class
evaluation <- data.frame(
  metric = c("accuracy", "roc_auc_yes", "sensitivity_yes", "specificity_yes", "brier", "majority_baseline_accuracy"),
  value = c(mean(cls == test$return_intent), roc_auc_vec(test$return_intent, prob, event_level = "second"),
    sens_vec(test$return_intent, cls, event_level = "second"), spec_vec(test$return_intent, cls, event_level = "second"),
    mean((prob - as.numeric(test$return_intent == "Yes"))^2),
    mean(test$return_intent == names(which.max(table(train$return_intent))))))
dir.create("models", showWarnings = FALSE)
dir.create("reports", showWarnings = FALSE)
readr::write_csv(evaluation, "reports/holdout_metrics.csv")
readr::write_csv(as.data.frame(table(predicted = cls, truth = test$return_intent)), "reports/confusion_matrix.csv")
readr::write_csv(baseline_metrics, "reports/baseline_cv.csv")
readr::write_csv(boost_metrics, "reports/xgboost_cv.csv")
metadata <- list(schema_version = 1L, features = model_features, algorithm = algorithm,
  trained_at = format(Sys.time(), tz = "UTC", usetz = TRUE), seed = 20261031,
  split_method = split_method, train_rows = nrow(train), test_rows = nrow(test),
  input_md5 = unname(tools::md5sum(args[1])), r_version = R.version.string,
  metrics = evaluation, task = "Retrospective declared return intent, not observed repurchase")
jsonlite::write_json(metadata, "reports/training_metadata.json", pretty = TRUE, auto_unbox = TRUE)
# Refit only after independent holdout evaluation; the saved artifact is not the heldout model.
metadata$workflow <- fit(selected, data = d)
saveRDS(metadata, "models/model.rds")
writeLines(capture.output(sessionInfo()), "reports/session-info.txt")
print(evaluation)
message("Saved trusted server-side model to models/model.rds; evaluation in reports/.")
