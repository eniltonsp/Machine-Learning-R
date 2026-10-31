# Run from the repository root.
files <- c("app.R", list.files("R", pattern = "[.]R$", full.names = TRUE),
  list.files("scripts", pattern = "[.]R$", full.names = TRUE))
for (f in files) parse(f)
testthat::test_dir("tests/testthat", stop_on_failure = TRUE)
message("Source parsing and tests completed.")
