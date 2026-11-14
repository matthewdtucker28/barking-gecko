DATA_FILE <- Sys.getenv("BGA_DATA", "data/BarkingGecko_Final_Data_Sheet.xlsx")
FIG_DIR <- "analysis/output/figures"
TABLE_DIR <- "analysis/output/tables"
dir.create(FIG_DIR, recursive = TRUE, showWarnings = FALSE)
dir.create(TABLE_DIR, recursive = TRUE, showWarnings = FALSE)

# Reads columns first_col..last_col (1-based) of a sheet, starting at header_row, as a plain data.frame
# with the given names. Rows that are entirely empty are dropped.
read_tab <- function(sheet, first_col, last_col, col_names, header_row = 1) {
  if (!file.exists(DATA_FILE)) stop("Data file not found: ", DATA_FILE, " (run from the repo root, or set BGA_DATA)")
  d <- readxl::read_excel(DATA_FILE, sheet = sheet,
                          range = readxl::cell_limits(c(header_row, first_col), c(NA, last_col)))
  d <- as.data.frame(d[rowSums(!is.na(d)) > 0, ])
  names(d) <- col_names
  d
}

# Excel dates come back as date-times; scripts label nights as "month/day", e.g. "11/2".
night_label <- function(x) paste0(as.integer(format(x, "%m")), "/", as.integer(format(x, "%d")))
