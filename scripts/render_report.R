#!/usr/bin/env Rscript
# scripts/render_report.R
#
# Standalone entry point: reads outputs/latest_run.rds, generates all plots
# under outputs/plots/, and writes the markdown report to outputs/reports/.
#
# Usage:
#   Rscript scripts/render_report.R
#   ARTIFACT=outputs/my_run.rds Rscript scripts/render_report.R
#
# Environment variables (all optional):
#   ARTIFACT     - path to the RDS artifact  (default: outputs/latest_run.rds)
#   PLOTS_DIR    - PNG output directory      (default: outputs/plots)
#   REPORTS_DIR  - markdown output directory (default: outputs/reports)

# Resolve the project root from the --file= argument Rscript injects, then
# fall back to the current working directory when called interactively.
# This avoids the dirname(getwd()) trap that walks one level too far up.
.args      <- commandArgs(trailingOnly = FALSE)
.file_flag <- grep("^--file=", .args, value = TRUE)
if (length(.file_flag) > 0L) {
  # Running as: Rscript scripts/render_report.R
  # --file= gives us scripts/render_report.R; its parent is scripts/; its
  # grandparent is the project root.
  .script_path <- sub("^--file=", "", .file_flag[[1L]])
  proj_root    <- dirname(dirname(normalizePath(.script_path, mustWork = FALSE)))
} else {
  # Sourced interactively from the project root.
  proj_root <- getwd()
}
setwd(proj_root)
rm(.args, .file_flag)

source("R/logging.R")
source("R/benchmark.R")
source("R/globals.R")
source("R/returns.R")
source("R/risk_metrics.R")
source("R/sim_mc.R")
source("R/validation.R")
source("R/plots.R")
source("R/reporting.R")

artifact_path <- Sys.getenv("ARTIFACT",     unset = "outputs/latest_run.rds")
plots_dir     <- Sys.getenv("PLOTS_DIR",    unset = "outputs/plots")
reports_dir   <- Sys.getenv("REPORTS_DIR",  unset = "outputs/reports")

log_info("render_report artifact=", artifact_path)

summary <- render_run_report(
  artifact_path = artifact_path,
  plots_dir     = plots_dir,
  reports_dir   = reports_dir,
  save_plots    = TRUE
)

log_info("render_report complete")
invisible(summary)
