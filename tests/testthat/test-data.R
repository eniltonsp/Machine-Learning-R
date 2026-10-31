testthat::test_that("Portuguese contracts support multiline text and preserve row boundaries", {
  f <- tempfile(fileext = ".csv")
  writeLines(c('Empresa,Status,Data,Reclamacao', 'Example,Resolvido,23/05/2021,"First line', 'second line"'), f)
  d <- prepare_data(read_input(f))
  testthat::expect_equal(nrow(d), 1L)
  testthat::expect_equal(d$date, as.Date("2021-05-23"))
  testthat::expect_match(d$text, "second line")
})

testthat::test_that("invalid schemas, dates, ranges and targets fail visibly", {
  testthat::expect_error(prepare_data(data.frame(company = "A")), "Missing columns")
  d <- data.frame(company = "A", status = "Resolvido", date = "not a date")
  testthat::expect_error(prepare_data(d), "Invalid dates")
  d$date <- "2021-05-23"; d$rating <- "11"
  testthat::expect_error(prepare_data(d), "between 0 and 10")
  d$rating <- "5"; d$return_intent <- "perhaps"
  testthat::expect_error(prepare_data(d), "Return intent")
})

testthat::test_that("R-exported TSV quotes do not silently merge complaint rows", {
  f <- tempfile(fileext = ".txt")
  original <- data.frame(Empresa = c("A", "B"), Status = c("Resolvido", "Respondida"),
    Reclamacao = c('Customer said "hello"', 'Another "quoted" complaint'))
  write.table(original, f, sep = "\t", row.names = FALSE, fileEncoding = "UTF-8")
  d <- prepare_data(read_input(f, delim = "\t"))
  testthat::expect_equal(nrow(d), 2L)
  testthat::expect_equal(d$text, original$Reclamacao)
})

testthat::test_that("resolution denominators do not treat pending cases as evaluated", {
  d <- prepare_data(data.frame(company = rep("A", 4), status = c("Resolvido", "Não Resolvido", "Respondida", "Não respondida")))
  s <- company_summary(d)
  testthat::expect_equal(s$resolved_share_all, 0.25)
  testthat::expect_equal(s$resolved_share_evaluated, 0.5)
  testthat::expect_true(is.na(company_summary(d[3:4, ])$resolved_share_evaluated))
  testthat::expect_equal(nrow(company_summary(d[FALSE, ])), 0L)
})

testthat::test_that("quality reporting flags duplicates without deleting them", {
  d <- data.frame(company = c("A", "A"), status = c("Other", "Other"))
  q <- quality_report(d)
  testthat::expect_equal(q$value[q$check == "Exact duplicate rows"], 1L)
  testthat::expect_equal(q$value[q$check == "Unknown status rows"], 2L)
  testthat::expect_equal(nrow(d), 2L)
})

testthat::test_that("export escapes formula strings and handles missing values", {
  d <- safe_export(data.frame(text = c("=1+1", " @SUM(A1)", "ordinary", NA_character_)))
  testthat::expect_equal(d$text[1:3], c("'=1+1", "' @SUM(A1)", "ordinary"))
  testthat::expect_true(is.na(d$text[4]))
})

testthat::test_that("demo starts without private data or a model", {
  d <- prepare_data(read_input(file.path(project_root, "data/demo/complaints.csv")))
  testthat::expect_equal(nrow(d), 600L)
  testthat::expect_equal(length(unique(d$company)), 4L)
  testthat::expect_false("return_intent" %in% names(d))
  testthat::expect_equal(sum(is.na(d$company)), 0L)
})
