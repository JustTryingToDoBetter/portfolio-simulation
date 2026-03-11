library(testthat)

# Tests for dashboard artifact loading and view-model normalization helpers.

test_that("load_run_artifact reads an existing RDS", {
  art <- make_mini_artifact()
  f <- tempfile(fileext = ".rds")
  on.exit(unlink(f), add = TRUE)
  saveRDS(art, f)

  loaded <- load_run_artifact(f)
  expect_type(loaded, "list")
  expect_true(!is.null(loaded$cfg))
})

test_that("load_run_artifact errors for missing file", {
  expect_error(
    load_run_artifact("outputs/does_not_exist.rds"),
    regexp = "Artifact file not found"
  )
})

test_that("validate_run_artifact passes complete artifact", {
  art <- make_mini_artifact()
  out <- validate_run_artifact(art)
  expect_true(isTRUE(out$ok))
  expect_type(out$warnings, "character")
})

test_that("validate_run_artifact fails when required fields are missing", {
  art <- make_mini_artifact()
  art$backtest <- NULL
  expect_error(
    validate_run_artifact(art),
    regexp = "missing required field"
  )
})

test_that("validate_run_artifact reports optional warnings", {
  art <- make_mini_artifact()
  art$christoffersen <- NULL
  art$timings <- NULL
  out <- validate_run_artifact(art)
  expect_true(length(out$warnings) >= 2)
})

test_that("build_dashboard_view_model returns normalized sections", {
  art <- make_mini_artifact()
  vm <- build_dashboard_view_model(art)

  expect_type(vm, "list")
  expect_true(all(c("summary", "risk_table", "validation_table", "health_checks") %in% names(vm)))
  expect_true(is.data.frame(vm$risk_table))
  expect_true(nrow(vm$risk_table) >= 3)
  expect_true(is.data.frame(vm$validation_table))
  expect_true(is.data.frame(vm$health_checks))
})

test_that("build_dashboard_view_model handles missing christoffersen gracefully", {
  art <- make_mini_artifact()
  art$christoffersen <- NULL
  vm <- build_dashboard_view_model(art)

  expect_true(is.null(vm$transition_counts))
  expect_true(any(grepl("Christoffersen", vm$validation_table$Metric)))
})

test_that("build_dashboard_view_model handles missing garch safely", {
  art <- make_mini_artifact()
  art$model_metrics$garch <- NULL
  art$simulations$garch <- NULL
  vm <- build_dashboard_view_model(art)

  expect_false("garch" %in% vm$available_models)
  expect_true(any(grepl("GARCH", vm$validation$warnings)))
})

test_that("build_dashboard_view_model includes interpretation lines", {
  art <- make_mini_artifact()
  vm <- build_dashboard_view_model(art)

  expect_type(vm$interpretation_lines, "character")
  expect_true(length(vm$interpretation_lines) >= 1)
})
