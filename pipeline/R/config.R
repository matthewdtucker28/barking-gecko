load_config <- function(path) {
  cfg <- yaml::read_yaml(path)
  defaults <- list(
    trim = list(clip_length_min = 10, target_hours = NULL,
                timestamp_regex = "\\d{8}_\\d{6}", timestamp_format = "%Y%m%d_%H%M%S",
                recorder_regex = "^[A-Za-z]+[0-9]+"),
    assign = list(n_annotators = 6, reference_night1_date = NULL, seed = NULL)
  )
  for (section in names(defaults)) {
    for (key in names(defaults[[section]])) {
      if (is.null(cfg[[section]][[key]])) cfg[[section]][key] <- list(defaults[[section]][[key]])
    }
  }
  cfg
}
