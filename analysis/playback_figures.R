suppressMessages(library(dplyr))
suppressMessages(library(ggplot2))
source("analysis/R/common.R")

raw <- read_tab("Playback", 2, 8, c("Habitat", "Trail", "Degree", "Order", "Sound", "First", "Time"))[, c("Habitat", "Sound", "First", "Time")]

habitat_labels <- c(GP = "Gravel Plains", RB = "River Bed")
species_labels <- c(Maculatus = "P. maculatus", Kochi = "P. kochi")
pal <- c("P. maculatus" = "#21bb85", "P. kochi" = "#eb6834")

proc <- raw %>%
  mutate(HabitatLabel = factor(habitat_labels[Habitat], levels = habitat_labels),
         Sound = factor(Sound, levels = c("Silence", "Kochi", "Maculatus"),
                        labels = c("Silence", "P. kochi", "P. maculatus")),
         First = factor(species_labels[First], levels = species_labels))

summary_df <- proc %>%
  group_by(HabitatLabel, Sound, First) %>%
  summarise(mean = mean(Time), se = sd(Time) / sqrt(n()), .groups = "drop")

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
    plot.subtitle = element_text(color = "#52514e", size = 11, margin = margin(b = 34)),
    plot.tag = element_text(color = "#898781", size = 8.5, hjust = 0),
    plot.tag.position = c(0.058, 0.865),
    strip.text = element_text(color = "#0b0b0b", face = "bold", size = 12),
    panel.spacing = unit(1.5, "lines"),
    legend.title = element_blank(),
    legend.text = element_text(color = "#52514e", face = "italic"),
    plot.margin = margin(12, 16, 12, 12)
  )

# ---- FIGURE 5: time to first response by playback treatment ------------
dodge <- position_dodge(width = 0.7)

# x-axis labels drawn under each panel so the species names can be italic with tight line spacing
x_labels <- expand.grid(Sound = levels(proc$Sound), HabitatLabel = levels(proc$HabitatLabel)) %>%
  mutate(HabitatLabel = factor(HabitatLabel, levels = levels(proc$HabitatLabel)),
         Sound = factor(Sound, levels = levels(proc$Sound))) %>%
  bind_rows(mutate(., y = -3.5, label = c("plain(Silence)", "italic('P. kochi')", "italic('P. maculatus')")[as.integer(Sound)]),
            mutate(filter(., Sound != "Silence"), y = -5.9, label = "plain(call)")) %>%
  filter(!is.na(y))

set.seed(42)
fig5 <- ggplot(proc, aes(x = Sound, y = Time, fill = First)) +
  geom_point(shape = 21, color = "white", size = 2.6, alpha = 0.5, stroke = 0.4,
             position = position_jitterdodge(jitter.width = 0.1, jitter.height = 0, dodge.width = 0.7)) +
  geom_errorbar(data = summary_df, aes(y = mean, ymin = mean - se, ymax = mean + se),
                width = 0.15, color = "#52514e", linewidth = 0.6, position = dodge) +
  geom_point(data = summary_df, aes(y = mean), size = 4.2, shape = 21,
             color = "#0b0b0b", stroke = 0.6, position = dodge) +
  facet_wrap(~HabitatLabel) +
  geom_text(data = x_labels, aes(x = Sound, y = y, label = label), inherit.aes = FALSE, parse = TRUE,
            family = "Helvetica", size = 3.65, color = "#52514e") +
  coord_cartesian(clip = "off") +
  scale_fill_manual(values = pal) +
  scale_y_continuous(limits = c(0, NA), oob = scales::oob_keep, expand = expansion(mult = c(0.02, 0.08))) +
  labs(
    title = "Time to First Response by Playback Treatment",
    subtitle = "Colors show the species that responded first",
    x = "Treatment",
    y = "Time to First Response (s)",
    tag = "* Small points show individual trials; black-outlined points and error bars show mean ± SE.\nOne-way ANOVAs on square-root time were not significant in any group (all p > 0.1)."
  ) +
  theme_burrow +
  theme(axis.text.x = element_blank(), axis.title.x = element_text(margin = margin(t = 38)))

ggsave(file.path(FIG_DIR, "fig5_response_time_by_treatment.png"), fig5,
       width = 8.5, height = 5.2, dpi = 300, bg = "white")
