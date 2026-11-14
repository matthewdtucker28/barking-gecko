# Usage (from the repo root): Rscript pipeline/run_trim.R [pipeline/config.yml]
args <- commandArgs(trailingOnly = TRUE)
config_path <- if (length(args) >= 1) args[1] else "pipeline/config.yml"

source("pipeline/R/config.R")
source("pipeline/R/trim.R")

cfg <- load_config(config_path)$trim
invisible(trim_recordings(cfg$input_root, cfg$output_root, cfg$log_path, cfg$clip_length_min,
                          cfg$target_hours, cfg$timestamp_regex, cfg$timestamp_format,
                          cfg$recorder_regex))
