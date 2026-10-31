testthat::test_that("missing model is a supported state", {
  testthat::expect_null(load_model_bundle(tempfile()))
})
testthat::test_that("incompatible model artifacts are rejected", {
  f <- tempfile(); saveRDS(list(schema_version = 0L), f)
  testthat::expect_error(load_model_bundle(f), "Incompatible")
})
testthat::test_that("scoring requires the complete model contract", {
  testthat::expect_error(validate_features(data.frame(company = "A")), "Model requires")
})
