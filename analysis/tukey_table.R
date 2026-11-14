suppressMessages(library(dplyr))
suppressMessages(library(ggplot2))

source("analysis/R/common.R")
d <- read_tab("HOBO", 1, 4, c("Habitat", "Date", "Time", "Temp"))[, c("Date", "Temp")]
d$Date <- factor(night_label(d$Date))

fit <- aov(Temp ~ Date, data = d)
tk <- TukeyHSD(fit)$Date

# TukeyHSD reports later - earlier; flip so every row is earlier - later
nights <- strsplit(rownames(tk), "-")
fmt <- function(x) sub("-", "−", sprintf("%.2f", x))
tab <- data.frame(
  Comparison = sapply(nights, function(n) paste0(n[2], " – ", n[1])),
  Diff = fmt(-tk[, "diff"]),
  CI = paste(fmt(-tk[, "upr"]), "to", fmt(-tk[, "lwr"])),
  P = ifelse(tk[, "p adj"] < 0.0001, "<0.0001", sprintf("%.3f", tk[, "p adj"])),
  Sig = tk[, "p adj"] < 0.05
)
write.csv(tab[, 1:4] |> setNames(c("Dates Compared", "Mean Difference (°F)", "95% Confidence Interval", "P value")),
          file.path(TABLE_DIR, "table1_tukey_temperature.csv"), row.names = FALSE)

aov_row <- summary(fit)[[1]]
note <- sprintf(paste0("Tukey HSD on logger temperature (°F), pooled across habitats (n = %d readings per night).\n",
                       "Differences are earlier night minus later night; positive values mean the earlier night was warmer.\n",
                       "Shaded rows are significant at a family-wise α of 0.05. Overall ANOVA: F(%d, %d) = %.2f, p < 0.0001."),
                nrow(d) / nlevels(d$Date), aov_row$Df[1], aov_row$Df[2], aov_row$`F value`[1])

n <- nrow(tab)
cells <- bind_rows(
  data.frame(y = 1, x = 1:4, label = c("Dates Compared", "Mean Difference (°F)", "95% Confidence Interval", "P value"),
             face = "bold", stringsAsFactors = FALSE),
  data.frame(y = rep(seq_len(n) + 1, 4), x = rep(1:4, each = n),
             label = c(tab$Comparison, tab$Diff, tab$CI, tab$P),
             face = c(rep("plain", 3 * n), ifelse(tab$Sig, "bold", "plain")), stringsAsFactors = FALSE)
)
xpos <- c(0.9, 2.5, 4.3, 5.9)
cells$xp <- xpos[cells$x]
bands <- data.frame(y = which(tab$Sig) + 1)

tbl <- ggplot() +
  geom_rect(data = bands, aes(xmin = 0.2, xmax = 6.6, ymin = y - 0.5, ymax = y + 0.5), fill = "#f1f0ea") +
  geom_segment(aes(x = 0.2, xend = 6.6, y = c(0.5, 1.5, n + 1.5), yend = c(0.5, 1.5, n + 1.5)),
               color = "#0b0b0b", linewidth = c(0.8, 0.4, 0.8)) +
  geom_text(data = cells, aes(x = xp, y = y, label = label, fontface = face),
            family = "Helvetica", size = 3.7, color = "#0b0b0b") +
  scale_y_reverse(expand = expansion(add = 0.3)) +
  scale_x_continuous(limits = c(0.2, 6.6), expand = c(0, 0)) +
  labs(title = "Table 1. Tukey HSD Comparisons of Temperature Between Nights", caption = note) +
  theme_void(base_family = "Helvetica") +
  theme(plot.title = element_text(face = "bold", size = 12.5, hjust = 0, margin = margin(b = 8)),
        plot.caption = element_text(color = "#52514e", size = 8.5, hjust = 0, lineheight = 1.2, margin = margin(t = 8)),
        plot.title.position = "plot", plot.caption.position = "plot",
        plot.margin = margin(14, 16, 12, 16))

ggsave(file.path(TABLE_DIR, "table1_tukey_temperature.png"), tbl, width = 7.2, height = 5.6, dpi = 300, bg = "white")
