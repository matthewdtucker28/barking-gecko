suppressMessages(library(dplyr))
suppressMessages(library(ggplot2))
source("analysis/R/common.R")

raw <- read_tab("Burrow_Density", 1, 4, c("Habitat", "Location", "Direction", "Burrows"))
raw <- raw[!is.na(raw$Burrows) & raw$Location != "", ]

habitat_labels <- c(GP = "Gravel Plains", RB = "River Bed", SS = "Sand Sea")
pal <- c("Gravel Plains" = "#2a78d6", "River Bed" = "#eb6834", "Sand Sea" = "#21bb85")

proc <- raw %>%
  group_by(Habitat, Location) %>%
  summarise(Burrows = sum(Burrows), .groups = "drop") %>%
  mutate(HabitatLabel = factor(habitat_labels[Habitat], levels = habitat_labels))

summary_df <- proc %>%
  group_by(HabitatLabel) %>%
  summarise(mean = mean(Burrows), se = sd(Burrows) / sqrt(n()), .groups = "drop")

# shared theme -----------------------------------------------------------
theme_burrow <- theme_minimal(base_size = 13, base_family = "Helvetica") +
  theme(
    panel.grid.minor = element_blank(),
    panel.grid.major.x = element_blank(),
    panel.grid.major.y = element_line(color = "#e1e0d9", linewidth = 0.4),
    axis.line.x = element_line(color = "#c3c2b7", linewidth = 0.5),
    axis.ticks = element_blank(),
    axis.text = element_text(color = "#52514e"),
    axis.title = element_text(color = "#0b0b0b"),
    plot.title = element_text(color = "#0b0b0b", face = "bold", size = 15),
    plot.subtitle = element_text(color = "#52514e", size = 11, margin = margin(b = 22)),
    plot.tag = element_text(color = "#898781", size = 8.5, hjust = 0),
    plot.tag.position = c(0.076, 0.885),
    legend.position = "none",
    plot.margin = margin(12, 16, 12, 12)
  )

# ---- FIGURE 1 (primary): location-level burrow counts by habitat -------
set.seed(42)
fig1 <- ggplot(proc, aes(x = HabitatLabel, y = Burrows)) +
  geom_jitter(aes(fill = HabitatLabel), width = 0.08, height = 0, size = 2.6,
              alpha = 0.5, stroke = 0.4, shape = 21, color = "white") +
  geom_errorbar(data = summary_df, aes(y = mean, ymin = mean - se, ymax = mean + se),
                width = 0.15, color = "#52514e", linewidth = 0.6) +
  geom_point(data = summary_df, aes(y = mean, fill = HabitatLabel), size = 4.2,
             shape = 21, color = "#0b0b0b", stroke = 0.6) +
  scale_fill_manual(values = pal) +
  scale_y_continuous(limits = c(0, NA), expand = expansion(mult = c(0.02, 0.08))) +
  labs(
    title = "Burrow Counts by Habitat Type",
    subtitle = "Burrows summed across the 4 cardinal-direction transects at each location",
    x = "Habitat",
    y = "Number of Burrows per Location",
    tag = "* Small points show individual locations; black-outlined points and error bars show mean ± SE per habitat."
  ) +
  theme_burrow

ggsave(file.path(FIG_DIR, "fig1_burrows_by_habitat.png"), fig1,
       width = 6.5, height = 5.2, dpi = 300, bg = "white")
