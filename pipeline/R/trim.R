library(tuneR)

parse_recording_start <- function(path, ts_regex, ts_format) {
  fname <- basename(path)
  m <- regmatches(fname, regexpr(ts_regex, fname))
  if (length(m) == 0 || m == "") stop("No timestamp matching '", ts_regex, "' in filename: ", fname)
  # tz = "UTC" avoids DST shifts; the stamp is the recorder's own clock, not a real UTC time.
  as.POSIXct(m, format = ts_format, tz = "UTC")
}

list_wav_files <- function(root) {
  files <- list.files(root, pattern = "\\.wav$", recursive = TRUE,
                      full.names = TRUE, ignore.case = TRUE)
  files[!grepl("^\\._", basename(files))]
}

recorder_id_from_path <- function(path, input_root, recorder_regex) {
  root <- sub("/+$", "", input_root)
  rel <- substring(path, nchar(root) + 2)
  top_folder <- strsplit(rel, "/", fixed = TRUE)[[1]][1]
  m <- regmatches(top_folder, regexpr(recorder_regex, top_folder))
  if (length(m) == 0 || m == "") stop("No recorder ID matching '", recorder_regex, "' in folder: ", top_folder)
  m
}

build_file_index <- function(files, input_root, ts_regex, ts_format, recorder_regex) {
  rows <- lapply(files, function(f) {
    hdr <- tuneR::readWave(f, header = TRUE)
    start_time <- parse_recording_start(f, ts_regex, ts_format)
    duration_sec <- hdr$samples / hdr$sample.rate
    data.frame(path = f, recorder = recorder_id_from_path(f, input_root, recorder_regex),
               start_time = start_time, duration_sec = duration_sec,
               end_time = start_time + duration_sec, stringsAsFactors = FALSE)
  })
  idx <- do.call(rbind, rows)
  idx[order(idx$recorder, idx$start_time), ]
}

clip_windows <- function(start_time, duration_sec, clip_length_sec, target_hours = NULL) {
  end_time <- start_time + duration_sec
  empty <- data.frame(clip_start = as.POSIXct(character(), tz = "UTC"),
                      clip_end = as.POSIXct(character(), tz = "UTC"))

  first_hour <- as.POSIXct(format(start_time, "%Y-%m-%d %H:00:00"),
                           format = "%Y-%m-%d %H:%M:%S", tz = "UTC")
  if (first_hour < start_time) first_hour <- first_hour + 3600
  if (first_hour > end_time) return(empty)

  clip_end <- seq(first_hour, end_time, by = 3600)
  clip_start <- clip_end - clip_length_sec

  keep <- clip_start >= start_time & clip_end <= end_time
  if (!is.null(target_hours)) keep <- keep & (as.integer(format(clip_end, "%H")) %in% target_hours)

  data.frame(clip_start = clip_start[keep], clip_end = clip_end[keep])
}

build_output_path <- function(output_root, recorder_id, clip_start, clip_end) {
  date_str <- format(clip_start, "%Y-%m-%d")
  time_str <- paste0(format(clip_start, "%H%M"), "-", format(clip_end, "%H%M"))
  file.path(output_root, recorder_id, paste0(recorder_id, "_", date_str, "_", time_str, ".wav"))
}

# Prefers one file that fully contains the window; otherwise stitches every file that starts inside it
# (duty-cycled recorders, e.g. 55 s on / 5 s off).
extract_window_audio <- function(recorder_files, clip_start, clip_end) {
  clip_length_sec <- as.numeric(difftime(clip_end, clip_start, units = "secs"))
  overlapping <- recorder_files[recorder_files$end_time > clip_start &
                                  recorder_files$start_time < clip_end, ]
  if (nrow(overlapping) == 0) return(NULL)

  full <- overlapping[overlapping$start_time <= clip_start & overlapping$end_time >= clip_end, ]
  if (nrow(full) >= 1) {
    from_sec <- as.numeric(difftime(clip_start, full$start_time[1], units = "secs"))
    wave <- tuneR::readWave(full$path[1], from = from_sec, to = from_sec + clip_length_sec, units = "seconds")
    return(list(wave = wave, n_segments = 1, actual_duration_sec = clip_length_sec,
                method = "single_file", source_files = basename(full$path[1])))
  }

  segs <- overlapping[overlapping$start_time >= clip_start & overlapping$start_time < clip_end, ]
  if (nrow(segs) == 0) return(NULL)

  waves <- lapply(segs$path, tuneR::readWave)
  combined <- waves[[1]]
  for (w in waves[-1]) combined <- tuneR::bind(combined, w)

  list(wave = combined, n_segments = nrow(segs), actual_duration_sec = sum(segs$duration_sec),
       method = "stitched", source_files = paste(basename(segs$path), collapse = ";"))
}

log_row <- function(recorder, clip_start = NA, clip_end = NA, output_file = NA, source_files = NA,
                    n_segments = NA, actual_duration_sec = NA, status) {
  fmt <- function(x) if (is.na(x)) NA else format(x, "%Y-%m-%d %H:%M:%S")
  data.frame(recorder = recorder, clip_start = fmt(clip_start), clip_end = fmt(clip_end),
             output_file = output_file, source_files = source_files, n_segments = n_segments,
             actual_duration_sec = actual_duration_sec, status = status, stringsAsFactors = FALSE)
}

process_recorder <- function(recorder_files, output_root, clip_length_sec, target_hours = NULL) {
  recorder_id <- recorder_files$recorder[1]
  group_start <- min(recorder_files$start_time)
  group_duration <- as.numeric(difftime(max(recorder_files$end_time), group_start, units = "secs"))

  windows <- clip_windows(group_start, group_duration, clip_length_sec, target_hours)
  if (nrow(windows) == 0) return(log_row(recorder_id, status = "skipped_no_matching_window"))

  dir.create(file.path(output_root, recorder_id), recursive = TRUE, showWarnings = FALSE)

  rows <- lapply(seq_len(nrow(windows)), function(i) {
    clip_start <- windows$clip_start[i]
    clip_end <- windows$clip_end[i]
    result <- extract_window_audio(recorder_files, clip_start, clip_end)
    if (is.null(result)) {
      return(log_row(recorder_id, clip_start, clip_end, n_segments = 0, actual_duration_sec = 0,
                     status = "skipped_no_data"))
    }
    out_path <- build_output_path(output_root, recorder_id, clip_start, clip_end)
    # extensible = FALSE writes a canonical PCM header, which more tools can read.
    tuneR::writeWave(result$wave, out_path, extensible = FALSE)
    log_row(recorder_id, clip_start, clip_end, out_path, result$source_files, result$n_segments,
            round(result$actual_duration_sec, 3), paste0("extracted_", result$method))
  })
  do.call(rbind, rows)
}

trim_recordings <- function(input_root, output_root, log_path = NULL, clip_length_min = 10,
                            target_hours = NULL, timestamp_regex = "\\d{8}_\\d{6}",
                            timestamp_format = "%Y%m%d_%H%M%S", recorder_regex = "^[A-Za-z]+[0-9]+") {
  files <- list_wav_files(input_root)
  if (length(files) == 0) {
    warning("No WAV files found under: ", input_root)
    return(invisible(NULL))
  }

  idx <- build_file_index(files, input_root, timestamp_regex, timestamp_format, recorder_regex)

  logs <- lapply(split(idx, idx$recorder), function(recorder_files) {
    message("Processing recorder: ", recorder_files$recorder[1], " (", nrow(recorder_files), " file(s))")
    tryCatch(
      process_recorder(recorder_files, output_root, clip_length_min * 60, target_hours),
      error = function(e) log_row(recorder_files$recorder[1], status = paste0("ERROR: ", conditionMessage(e)))
    )
  })
  full_log <- do.call(rbind, logs)

  if (!is.null(log_path)) {
    dir.create(dirname(log_path), recursive = TRUE, showWarnings = FALSE)
    write.csv(full_log, log_path, row.names = FALSE)
    message("Log written to: ", log_path)
  }
  full_log
}
