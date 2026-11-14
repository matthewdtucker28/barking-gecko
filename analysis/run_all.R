# Usage (from the repo root): Rscript analysis/run_all.R
# Reads data/BarkingGecko_Final_Data_Sheet.xlsx; writes figures and tables to analysis/output/.
scripts <- c("burrow_figures", "species_figures", "temp_figures", "playback_figures",
             "tukey_table", "tukey_table_latex", "temp_range_table_latex", "temp_range_table",
             "temp_stats")
for (s in scripts) {
  message("== ", s)
  source(file.path("analysis", paste0(s, ".R")), local = new.env())
}
