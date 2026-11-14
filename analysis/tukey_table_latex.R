

source("analysis/R/common.R")
d <- read_tab("HOBO", 1, 4, c("Habitat", "Date", "Time", "Temp"))[, c("Date", "Temp")]
d$Date <- factor(night_label(d$Date))
fit <- aov(Temp ~ Date, data = d)
tk <- TukeyHSD(fit)$Date
a <- summary(fit)[[1]]

# TukeyHSD reports later - earlier; flip so every row is earlier - later
nights <- strsplit(rownames(tk), "-")
num <- function(x) paste0("$", sprintf("%.2f", x), "$")
rows <- sprintf("%s--%s & %s & $[%.2f,\\ %.2f]$ & %s \\\\",
                sapply(nights, `[`, 2), sapply(nights, `[`, 1),
                num(-tk[, "diff"]), -tk[, "upr"], -tk[, "lwr"],
                ifelse(tk[, "p adj"] < 0.0001, "$<0.0001$", sprintf("%.3f", tk[, "p adj"])))

body <- c(
  "\\begin{table}[H]",
  "\\centering",
  sprintf(paste0("\\caption{Tukey HSD pairwise comparisons of logger temperature ($^{\\circ}$F) between nights, ",
                 "pooled across habitats ($n = %d$ readings per night). Each difference is the earlier night minus ",
                 "the later night, so a positive value means the earlier night was warmer. ",
                 "Overall ANOVA: $F(%d, %d) = %.2f$, $p < 0.0001$.}"),
          nrow(d) / nlevels(d$Date), a$Df[1], a$Df[2], a$`F value`[1]),
  "\\label{tab:tukey_temp}",
  "\\begin{tabular}{@{}lrrr@{}}",
  "\\toprule",
  "Dates compared & Difference ($^{\\circ}$F) & 95\\% CI & $p$ \\\\",
  "\\midrule",
  rows,
  "\\bottomrule",
  "\\end{tabular}",
  "\\end{table}"
)
writeLines(body, file.path(TABLE_DIR, "table1_tukey_temperature.tex"))

# compile-ready wrapper using the same preamble as QSS20_Final_Report.tex
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
), file.path(TABLE_DIR, "table1_tukey_temperature_standalone.tex"))
