test_files <- list.files("tests", pattern = "^test_.*\\.R$", full.names = TRUE)

if (length(test_files) == 0) {
  stop("No test files found in tests/", call. = FALSE)
}

for (f in test_files) {
  source(f)
}

cat(sprintf("All tests passed (%d files).\n", length(test_files)))