

source("analysis/R/common.R")
d <- read_tab("HOBO", 1, 4, c("Habitat", "Date", "Time", "Temp"))[, c("Habitat", "Date", "Temp")]
d$Date <- night_label(d$Date)

rng <- aggregate(Temp ~ Habitat + Date, d, function(x) c(Min = min(x), Max = max(x)))
rng <- data.frame(rng[, 1:2], rng$Temp)
nights <- unique(rng$Date)
habs <- c("GP", "RB", "SS")
get <- function(h, n, col) rng[[col]][rng$Habitat == h & rng$Date == n]

wide <- data.frame(Night = nights)
for (h in habs) { wide[[paste0(h, "_Min")]] <- sapply(nights, get, h = h, col = "Min")
                  wide[[paste0(h, "_Max")]] <- sapply(nights, get, h = h, col = "Max") }
write.csv(wide, file.path(TABLE_DIR, "table2_temp_range.csv"), row.names = FALSE)

num <- function(x) paste0("$", sprintf("%.1f", x), "$")
rows <- sapply(seq_along(nights), function(i)
  paste0(nights[i], " & ", paste(num(unlist(wide[i, -1])), collapse = " & "), " \\\\"))

body <- c(
  "\\begin{table}[H]",
  "\\centering",
  paste0("\\caption{Minimum and maximum logger temperature ($^{\\circ}$F) recorded in each habitat on each night ",
         "(GP = Gravel Plains, RB = River Bed, SS = Sand Sea). Values are the lowest and highest logger readings ",
         "recorded each night.}"),
  "\\label{tab:temp_range}",
  "\\begin{tabular}{@{}lrrrrrr@{}}",
  "\\toprule",
  " & \\multicolumn{2}{c}{Gravel Plains} & \\multicolumn{2}{c}{River Bed} & \\multicolumn{2}{c}{Sand Sea} \\\\",
  "\\cmidrule(lr){2-3} \\cmidrule(lr){4-5} \\cmidrule(l){6-7}",
  "Night & Min ($^{\\circ}$F) & Max ($^{\\circ}$F) & Min ($^{\\circ}$F) & Max ($^{\\circ}$F) & Min ($^{\\circ}$F) & Max ($^{\\circ}$F) \\\\",
  "\\midrule",
  rows,
  "\\bottomrule",
  "\\end{tabular}",
  "\\end{table}"
)
writeLines(body, file.path(TABLE_DIR, "table2_temp_range.tex"))

writeLines(c(
  "\\documentclass[10pt]{article}",
  "\\usepackage[utf8]{inputenc}",
  "\\usepackage[T1]{fontenc}",
  "\\usepackage[letterpaper,margin=1in]{geometry}",
  "\\usepackage{mathptmx}",
  "\\usepackage{amsmath,amssymb}",
  "\\usepackage{float}",
  "\\usepackage{booktabs}",
  "\\usepackage[labelfont=bf,font=small,labelsep=period]{caption}",
  "\\begin{document}",
  body,
  "\\end{document}"
), file.path(TABLE_DIR, "table2_temp_range_standalone.tex"))
