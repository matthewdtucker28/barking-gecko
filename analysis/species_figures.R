suppressMessages(library(dplyr))
suppressMessages(library(ggplot2))
source("analysis/R/common.R")

raw <- read_tab("PAM_Processed", 5, 7, c("Location", "Species", "Calls"), header_row = 2)

habitat_labels <- c(GP = "Gravel Plains", RB = "River Bed", SS = "Sand Sea")
species_labels <- c(c = "P. circumsyrticus", k = "P. kochi", m = "P. maculatus")
pal <- c("P. circumsyrticus" = "#2a78d6", "P. kochi" = "#eb6834", "P. maculatus" = "#21bb85")

# pooled calls per habitat x species; position = "fill" below converts to proportions
proc <- raw %>%
  mutate(HabitatLabel = factor(habitat_labels[substr(Location, 1, 2)], levels = habitat_labels),
         SpeciesLabel = factor(species_labels[Species], levels = species_labels)) %>%
  group_by(HabitatLabel, SpeciesLabel) %>%
  summarise(Calls = sum(Calls), .groups = "drop")

# callouts for slices too thin to see (<2% of a bar); levels stack top-down in factor order
tiny <- proc %>%
  group_by(HabitatLabel) %>%
  arrange(SpeciesLabel, .by_group = TRUE) %>%
  mutate(prop = Calls / sum(Calls), ymid = 1 - (cumsum(prop) - prop) - prop / 2) %>%
  ungroup() %>%
  filter(Calls > 0, prop < 0.02) %>%
  mutate(label = sprintf("%.1f%%* (n = %d)", 100 * prop, Calls), xpos = as.numeric(HabitatLabel),
         lo = ifelse(ymid > 0.5, ymid + prop / 2 - 0.01, ymid - prop / 2))  # min-height marker, anchored at true edge

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
    plot.subtitle = element_text(color = "#52514e", size = 11, margin = margin(b = 10)),
    legend.title = element_blank(),
    legend.text = element_text(color = "#52514e", face = "italic"),
    plot.margin = margin(12, 16, 12, 12)
  )

fig2 <- ggplot(proc, aes(x = HabitatLabel, y = Calls, fill = SpeciesLabel)) +
  geom_col(position = "fill", width = 0.6) +
  geom_rect(data = tiny, aes(xmin = xpos - 0.3, xmax = xpos + 0.3, ymin = lo, ymax = lo + 0.01, fill = SpeciesLabel),
            inherit.aes = FALSE) +
  geom_segment(data = tiny, aes(x = xpos + 0.42, xend = xpos + 0.31, y = lo + 0.045, yend = lo + 0.005),
               inherit.aes = FALSE, color = "black", linewidth = 0.3,
               arrow = arrow(length = unit(0.07, "in"), type = "closed")) +
  geom_text(data = tiny, aes(x = xpos + 0.44, y = lo + 0.045, label = label, color = SpeciesLabel),
            inherit.aes = FALSE, hjust = 0, size = 2.4, show.legend = FALSE) +
  scale_fill_manual(values = pal) +
  scale_color_manual(values = pal, guide = "none") +
  scale_x_discrete(expand = expansion(add = 0.6)) +
  coord_cartesian(clip = "off") +
  scale_y_continuous(expand = expansion(mult = c(0, 0.06))) +
  labs(
    title = "Species Composition of Calls by Habitat Type",
    subtitle = "Calls pooled across the 4 locations per habitat, nights 1-6",
    x = "Habitat",
    y = "Proportion of Calls",
    tag = "* Slices under 2% are drawn at a minimum visible height; labels give exact values."
  ) +
  theme_burrow +
  theme(plot.tag = element_text(color = "#898781", size = 8.5, hjust = 0),
        plot.tag.position = c(0.095, 0.885),
        legend.box.spacing = unit(30, "pt"),
        plot.subtitle = element_text(color = "#52514e", size = 11, margin = margin(b = 22)))

ggsave(file.path(FIG_DIR, "fig2_species_calls_by_habitat.png"), fig2,
       width = 6.5, height = 5.2, dpi = 300, bg = "white")

# ---- FIGURE 3: same data, not pooled (one bar per location) -------------
by_loc <- raw %>%
  mutate(HabitatLabel = factor(habitat_labels[substr(Location, 1, 2)], levels = habitat_labels),
         SpeciesLabel = factor(species_labels[Species], levels = species_labels))

fig3 <- ggplot(by_loc, aes(x = Location, y = Calls, fill = SpeciesLabel)) +
  geom_col(position = "fill", width = 0.7, color = "white", linewidth = 0.4) +
  facet_grid(~HabitatLabel, scales = "free_x", space = "free_x") +
  scale_fill_manual(values = pal) +
  scale_y_continuous(expand = expansion(mult = c(0, 0.02))) +
  labs(
    title = "Species Composition of Calls by Location",
    subtitle = "Each bar is one location, nights 1-6 (GP1 recorded no calls)",
    x = "Location",
    y = "Proportion of Calls"
  ) +
  theme_burrow +
  theme(strip.text = element_text(color = "#0b0b0b", face = "bold"),
        panel.spacing = unit(1, "lines"))

ggsave(file.path(FIG_DIR, "other_figure_1.png"), fig3,
       width = 8.5, height = 5.2, dpi = 300, bg = "white")
