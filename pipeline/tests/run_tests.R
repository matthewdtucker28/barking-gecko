# Usage (from the repo root): Rscript pipeline/tests/run_tests.R
source("pipeline/R/config.R")
source("pipeline/R/trim.R")
source("pipeline/R/assign.R")

passed <- 0
check <- function(name, ok) {
  if (!isTRUE(ok)) stop("FAILED: ", name, call. = FALSE)
  passed <<- passed + 1
  cat("ok  ", name, "\n")
}

SR <- 200
silent_wave <- function(seconds) {
  tuneR::Wave(left = as.integer(round(1000 * sin(seq_len(seconds * SR) / 7))), samp.rate = SR, bit = 16)
}
write_src <- function(path, seconds) {
  dir.create(dirname(path), recursive = TRUE, showWarnings = FALSE)
  tuneR::writeWave(silent_wave(seconds), path)
}
clip_seconds <- function(path) {
  h <- tuneR::readWave(path, header = TRUE)
  h$samples / h$sample.rate
}

tmp <- tempfile("bga_")
raw <- file.path(tmp, "raw")

# single long file: 17:00:00 -> 21:30:00 (4.5 h)
write_src(file.path(raw, "GP2_Night1", "sub", "20261102_170000.WAV"), 4.5 * 3600)
# duty-cycled recorder: 55 s files every 60 s from 18:40:00 for 30 minutes
for (k in 0:29) {
  start <- as.POSIXct("2026-11-02 18:40:00", tz = "UTC") + 60 * k
  write_src(file.path(raw, "SS1", paste0(format(start, "%Y%m%d_%H%M%S"), ".WAV")), 55)
}
# macOS sidecar that must be ignored
invisible(file.create(file.path(raw, "GP2_Night1", "sub", "._20261102_170000.WAV")))

out <- file.path(tmp, "trimmed")
log <- trim_recordings(raw, out, file.path(tmp, "logs", "trim.csv"), target_hours = c(19, 20, 21))

check("sidecar ignored, 2 recorders found", setequal(unique(log$recorder), c("GP2", "SS1")))
gp <- log[log$recorder == "GP2", ]
check("GP2 yields the 3 target-hour clips", nrow(gp) == 3 && all(gp$status == "extracted_single_file"))
check("GP2 clip names", setequal(basename(gp$output_file),
      paste0("GP2_2026-11-02_", c("1850-1900", "1950-2000", "2050-2100"), ".wav")))
check("GP2 clips are exactly 600 s", all(vapply(gp$output_file, clip_seconds, 0) == 600))
ss <- log[log$recorder == "SS1", ]
check("SS1 only has the 18:50-19:00 window (target hour 19)", nrow(ss) == 1 && grepl("1850-1900", ss$output_file))
check("SS1 stitched from 10 files, 550 s", ss$status == "extracted_stitched" && ss$n_segments == 10 &&
        clip_seconds(ss$output_file) == 550)
check("log csv written", file.exists(file.path(tmp, "logs", "trim.csv")))

# non-default clip length, no hour filter
out5 <- file.path(tmp, "trimmed5")
log5 <- trim_recordings(raw, out5, clip_length_min = 5, recorder_regex = "^[A-Z]+[0-9]")
g5 <- log5[log5$recorder == "GP2", ]
check("5-minute clips, all hours", nrow(g5) == 4 && all(vapply(g5$output_file, clip_seconds, 0) == 300))

# other filename convention + unrecognized folder
raw2 <- file.path(tmp, "raw2")
write_src(file.path(raw2, "siteA", "rec_2026-11-03T17-00-00.wav"), 2 * 3600)
log2 <- trim_recordings(raw2, file.path(tmp, "trimmed2"), timestamp_regex = "\\d{4}-\\d{2}-\\d{2}T\\d{2}-\\d{2}-\\d{2}",
                        timestamp_format = "%Y-%m-%dT%H-%M-%S", recorder_regex = "^site[A-Z]")
check("custom filename pattern and recorder regex", nrow(log2) == 2 && all(log2$recorder == "siteA") &&
        all(log2$status == "extracted_single_file"))
err <- tryCatch(trim_recordings(raw2, file.path(tmp, "trimmed3")), error = function(e) conditionMessage(e))
check("filename not matching the pattern stops with a clear message", grepl("No timestamp matching", err))

# window edge cases
w <- clip_windows(as.POSIXct("2026-11-02 18:55:00", tz = "UTC"), 3600, 600)
check("window starting before first full clip is dropped", nrow(w) == 0 || min(w$clip_start) >= as.POSIXct("2026-11-02 18:55:00", tz = "UTC"))
check("recording shorter than an hour boundary gives no windows",
      nrow(clip_windows(as.POSIXct("2026-11-02 19:10:00", tz = "UTC"), 600, 600)) == 0)

# ---- assignment ----
tr <- file.path(tmp, "trimmed_assign")
make_clips <- function(n_per_recorder, day) {
  for (rec in c("GP1", "RB1", "SS1")) for (h in seq_len(n_per_recorder)) {
    p <- file.path(tr, rec, sprintf("%s_2026-11-%02d_%02d50-%02d00.wav", rec, day, 18 + h, 19 + h))
    dir.create(dirname(p), recursive = TRUE, showWarnings = FALSE)
    invisible(file.create(p))
  }
}
ann <- file.path(tmp, "ann")
ledger_path <- file.path(tmp, "ledger.csv")

make_clips(2, 2)
l1 <- assign_clips(tr, ann, ledger_path, 3, "2026-11-02", seed = 1)
check("batch 1: 6 clips, 2 per annotator", nrow(l1) == 6 && all(table(l1$annotator) == 2))
check("ledger paths are relative to trimmed_root", all(!grepl("^/", l1$path)) && all(l1$path == paste0(l1$recorder, "/", l1$file)))
check("clips copied into Night subfolders", all(file.exists(file.path(ann, paste0("Annotator_", l1$annotator), "Night1", l1$file))))

l1b <- assign_clips(paste0(tr, "/"), ann, ledger_path, 3, "2026-11-02")
check("re-run (even with trailing slash) assigns nothing new", nrow(l1b) == 6)

before <- read.csv(ledger_path, stringsAsFactors = FALSE)
make_clips(3, 3)
l2 <- assign_clips(tr, ann, ledger_path, 3, "2026-11-02")
check("batch 2 appended, 9 new clips; total 15", nrow(l2) == 15)
check("earlier assignments untouched", identical(l2[1:6, c("file", "annotator")], before[, c("file", "annotator")]))
check("cumulative balance is even (5 each)", all(table(l2$annotator) == 5))
check("Night2 label used for 11-03 clips", any(file.exists(file.path(ann, "Annotator_1", "Night2", l2$file))))

tr_none <- file.path(tmp, "tr_none")
dir.create(file.path(tr_none, "GP1"), recursive = TRUE)
invisible(file.create(file.path(tr_none, "GP1", "GP1_2026-11-02_1950-2000.wav")))
assign_clips(tr_none, file.path(tmp, "ann_flat"), file.path(tmp, "ledger_flat.csv"), 2, NULL, seed = 1)
check("no reference date -> no Night subfolder", length(list.files(file.path(tmp, "ann_flat"), pattern = "wav$", recursive = TRUE)) == 1 &&
        !any(grepl("Night", list.files(file.path(tmp, "ann_flat"), recursive = TRUE))))

# config defaults
cfg_path <- file.path(tmp, "c.yml")
writeLines(c("trim:", "  input_root: /a", "assign:", "  n_annotators: 4"), cfg_path)
cfg <- load_config(cfg_path)
check("config defaults filled in", cfg$trim$clip_length_min == 10 && cfg$trim$recorder_regex == "^[A-Za-z]+[0-9]+" &&
        cfg$assign$n_annotators == 4 && is.null(cfg$assign$seed))

unlink(tmp, recursive = TRUE)
cat("\nAll", passed, "checks passed\n")
