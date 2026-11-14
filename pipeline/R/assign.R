LEDGER_COLS <- c("file", "path", "recorder", "annotator", "assigned_at")

load_ledger <- function(ledger_path) {
  if (file.exists(ledger_path)) return(read.csv(ledger_path, stringsAsFactors = FALSE))
  setNames(data.frame(matrix(nrow = 0, ncol = length(LEDGER_COLS))), LEDGER_COLS)
}

# path is stored relative to trimmed_root so the ledger survives the folder moving or being mounted elsewhere.
find_all_clips <- function(trimmed_root) {
  root <- sub("/+$", "", trimmed_root)
  paths <- list.files(root, pattern = "\\.wav$", recursive = TRUE, full.names = TRUE, ignore.case = TRUE)
  paths <- paths[!grepl("^\\._", basename(paths))]
  rel <- substring(paths, nchar(root) + 2)
  data.frame(file = basename(rel), path = rel, recorder = basename(dirname(rel)),
             stringsAsFactors = FALSE)
}

# Each new clip goes to the annotator with the fewest clips so far (ties: fewest from that clip's
# recorder, then random), keeping each batch and the running total as even as integers allow.
assign_new_clips <- function(all_clips, ledger, n_annotators) {
  new_clips <- all_clips[!(all_clips$path %in% ledger$path), ]
  if (nrow(new_clips) == 0) return(ledger[0, ])

  new_clips <- new_clips[sample(nrow(new_clips)), ]

  totals <- table(factor(ledger$annotator, levels = 1:n_annotators))
  recorder_totals <- table(factor(ledger$annotator, levels = 1:n_annotators),
                           factor(ledger$recorder, levels = unique(c(ledger$recorder, new_clips$recorder))))

  assigned <- integer(nrow(new_clips))
  for (i in seq_len(nrow(new_clips))) {
    rec <- new_clips$recorder[i]
    candidates <- which(totals == min(totals))
    rec_counts <- recorder_totals[candidates, rec]
    candidates <- candidates[rec_counts == min(rec_counts)]
    pick <- if (length(candidates) == 1) candidates else sample(candidates, 1)

    assigned[i] <- pick
    totals[pick] <- totals[pick] + 1
    recorder_totals[pick, rec] <- recorder_totals[pick, rec] + 1
  }

  data.frame(file = new_clips$file, path = new_clips$path, recorder = new_clips$recorder,
             annotator = assigned, assigned_at = format(Sys.time(), "%Y-%m-%d %H:%M:%S"),
             stringsAsFactors = FALSE)
}

# Clip names look like "<recorder>_<YYYY-MM-DD>_<HHMM-HHMM>.wav"; Night 1 is reference_date.
night_label_for_file <- function(file, reference_date) {
  date_str <- regmatches(file, regexpr("\\d{4}-\\d{2}-\\d{2}", file))
  paste0("Night", as.integer(as.Date(date_str) - as.Date(reference_date)) + 1)
}

copy_to_annotator_folders <- function(new_rows, trimmed_root, annotator_root, reference_date = NULL) {
  for (i in seq_len(nrow(new_rows))) {
    dest_dir <- file.path(annotator_root, paste0("Annotator_", new_rows$annotator[i]))
    if (!is.null(reference_date)) {
      dest_dir <- file.path(dest_dir, night_label_for_file(new_rows$file[i], reference_date))
    }
    dir.create(dest_dir, recursive = TRUE, showWarnings = FALSE)
    dest_path <- file.path(dest_dir, new_rows$file[i])
    if (file.exists(dest_path)) {
      warning("Destination already exists, skipping copy: ", dest_path)
      next
    }
    file.copy(file.path(trimmed_root, new_rows$path[i]), dest_path)
  }
}

assign_clips <- function(trimmed_root, annotator_root, ledger_path, n_annotators = 6,
                         reference_date = NULL, seed = NULL) {
  if (!is.null(seed)) set.seed(seed)

  ledger <- load_ledger(ledger_path)
  new_rows <- assign_new_clips(find_all_clips(trimmed_root), ledger, n_annotators)

  if (nrow(new_rows) == 0) {
    message("No new clips to assign.")
    return(ledger)
  }

  copy_to_annotator_folders(new_rows, trimmed_root, annotator_root, reference_date)

  full_ledger <- rbind(ledger, new_rows)
  dir.create(dirname(ledger_path), recursive = TRUE, showWarnings = FALSE)
  write.csv(full_ledger, ledger_path, row.names = FALSE)

  message("Assigned ", nrow(new_rows), " new clip(s). Running totals per annotator:")
  print(table(factor(full_ledger$annotator, levels = 1:n_annotators)))
  full_ledger
}
