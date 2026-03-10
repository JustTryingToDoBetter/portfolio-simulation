library(testthat)

# Run the full testthat suite from the project root.
# All tests live in tests/testthat/.
# helper-fixtures.R is sourced automatically by test_dir() before test files.

test_dir(
  path     = "tests/testthat",
  reporter = "progress"
)