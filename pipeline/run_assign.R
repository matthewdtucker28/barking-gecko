# Usage (from the repo root): Rscript pipeline/run_assign.R [pipeline/config.yml]
# Safe to re-run as clips accumulate: clips already in the ledger are never reassigned or re-copied.
args <- commandArgs(trailingOnly = TRUE)
config_path <- if (length(args) >= 1) args[1] else "pipeline/config.yml"

source("pipeline/R/config.R")
source("pipeline/R/assign.R")

cfg <- load_config(config_path)$assign
invisible(assign_clips(cfg$trimmed_root, cfg$annotator_root, cfg$ledger_path, cfg$n_annotators,
                       cfg$reference_night1_date, cfg$seed))
