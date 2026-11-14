suppressMessages(library(dplyr))
suppressMessages(library(ggplot2))
source("analysis/R/common.R")

wide <- read.csv(file.path(TABLE_DIR, "table2_temp_range.csv"), stringsAsFactors = FALSE)

n <- nrow(wide)
xpos <- c(0.9, 2.2, 3.1, 4.4, 5.3, 6.6, 7.5)
vals <- sprintf("%.1f", unlist(wide[, -1]))
cells <- bind_rows(
  data.frame(y = 2, x = 1:7, label = c("Night", rep(c("Min (°F)", "Max (°F)"), 3)), face = "bold"),
  data.frame(y = 1, x = c(2.65, 4.85, 7.05), label = c("Gravel Plains", "River Bed", "Sand Sea"), face = "bold"),
  data.frame(y = rep(seq_len(n) + 2, 7), x = rep(1:7, each = n),
             label = c(wide$Night, vals), face = "plain")
)
cells$xp <- ifelse(cells$y == 1, cells$x, xpos[cells$x])
rules <- data.frame(x = c(1.8, 4.0, 6.2), xend = c(3.5, 5.7, 7.9))

tbl <- ggplot() +
  geom_segment(aes(x = 0.2, xend = 8.2, y = c(0.5, 2.5, n + 2.5), yend = c(0.5, 2.5, n + 2.5)),
               color = "#0b0b0b", linewidth = c(0.8, 0.4, 0.8)) +
  geom_segment(data = rules, aes(x = x, xend = xend, y = 1.5, yend = 1.5), color = "#0b0b0b", linewidth = 0.3) +
  geom_text(data = cells, aes(x = xp, y = y, label = label, fontface = face),
            family = "Helvetica", size = 3.7, color = "#0b0b0b") +
  scale_y_reverse(expand = expansion(add = 0.3)) +
  scale_x_continuous(limits = c(0.2, 8.2), expand = c(0, 0)) +
  labs(title = "Table 2. Minimum and Maximum Temperature per Habitat per Night",
       caption = paste0("Logger temperature (°F). Values are the lowest and highest readings recorded in each habitat each night.")) +
  theme_void(base_family = "Helvetica") +
  theme(plot.title = element_text(face = "bold", size = 12.5, hjust = 0, margin = margin(b = 8)),
        plot.caption = element_text(color = "#52514e", size = 8.5, hjust = 0, margin = margin(t = 8)),
        plot.title.position = "plot", plot.caption.position = "plot",
        plot.margin = margin(14, 16, 12, 16))

ggsave(file.path(TABLE_DIR, "table2_temp_range.png"), tbl, width = 7.2, height = 3.4, dpi = 300, bg = "white")
