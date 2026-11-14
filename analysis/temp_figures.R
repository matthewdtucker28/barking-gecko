suppressMessages(library(dplyr))
suppressMessages(library(ggplot2))
source("analysis/R/common.R")

raw <- read_tab("HOBO_Processed", 1, 5, c("Habitat", "Location", "Date", "Temp", "Calls"), header_row = 2)

habitat_labels <- c(GP = "Gravel Plains", RB = "River Bed", SS = "Sand Sea")
pal <- c("Hot" = "#eb6834", "Cold" = "#2a78d6")

proc <- raw %>%
  mutate(Temp = factor(Temp, levels = c("Hot", "Cold")),
         HabitatLabel = factor(habitat_labels[Habitat], levels = habitat_labels),
         LogCalls = log(Calls + 1))

# one-way ANOVA on log(calls + 1); the Location-blocked model is a check on repeated nights per site
print(summary(aov(LogCalls ~ Temp, data = proc)))
print(summary(aov(LogCalls ~ Location + Temp, data = proc)))

summary_df <- proc %>%
  group_by(Temp) %>%
  summarise(mean = mean(Calls), se = sd(Calls) / sqrt(n()), .groups = "drop")

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
    plot.tag.position = c(0.085, 0.885),
    legend.position = "none",
    plot.margin = margin(12, 16, 12, 12)
  )

# ---- FIGURE 3: calls per location-night, hot vs cold nights ------------
set.seed(42)
fig3 <- ggplot(proc, aes(x = Temp, y = Calls)) +
  geom_jitter(aes(fill = Temp), width = 0.1, height = 0, size = 2.6,
              alpha = 0.5, stroke = 0.4, shape = 21, color = "white") +
  geom_errorbar(data = summary_df, aes(y = mean, ymin = mean - se, ymax = mean + se),
                width = 0.15, color = "#52514e", linewidth = 0.6) +
  geom_point(data = summary_df, aes(y = mean, fill = Temp), size = 4.2,
             shape = 21, color = "#0b0b0b", stroke = 0.6) +
  scale_fill_manual(values = pal) +
  scale_x_discrete(labels = c(Hot = "Hot Nights", Cold = "Cold Nights")) +
  scale_y_continuous(limits = c(0, NA), expand = expansion(mult = c(0.02, 0.08))) +
  labs(
    title = "Calling Activity on Hot vs Cold Nights",
    subtitle = "Total calls per location per night, locations with more than 10 calls",
    x = "Temperature Category",
    y = "Number of Calls per Location per Night",
    tag = "* Small points show individual location-nights; black-outlined points and error bars show mean ± SE."
  ) +
  theme_burrow

ggsave(file.path(FIG_DIR, "fig3_calls_by_temperature.png"), fig3,
       width = 6.5, height = 5.2, dpi = 300, bg = "white")

# ---- FIGURE 4: same, split by habitat ----------------------------------
summary_hab <- proc %>%
  group_by(HabitatLabel, Temp) %>%
  summarise(mean = mean(Calls), se = sd(Calls) / sqrt(n()), .groups = "drop")

dodge <- position_dodge(width = 0.7)

set.seed(42)
fig4 <- ggplot(proc, aes(x = HabitatLabel, y = Calls, fill = Temp)) +
  geom_vline(xintercept = c(1.5, 2.5), color = "black", linewidth = 0.4) +
  geom_point(shape = 21, color = "white", size = 2.6, alpha = 0.5, stroke = 0.4,
             position = position_jitterdodge(jitter.width = 0.1, jitter.height = 0, dodge.width = 0.7)) +
  geom_errorbar(data = summary_hab, aes(y = mean, ymin = mean - se, ymax = mean + se),
                width = 0.15, color = "#52514e", linewidth = 0.6, position = dodge) +
  geom_point(data = summary_hab, aes(y = mean), size = 4.2, shape = 21,
             color = "#0b0b0b", stroke = 0.6, position = dodge) +
  scale_fill_manual(values = pal) +
  scale_y_continuous(limits = c(0, NA), expand = expansion(mult = c(0.02, 0.08))) +
  labs(
    title = "Calling Activity on Hot vs Cold Nights by Habitat Type",
    subtitle = "Total calls per location per night, locations with more than 10 calls",
    x = "Habitat",
    y = "Number of Calls per Location per Night",
    tag = "* Small points show individual location-nights; black-outlined points and error bars show mean ± SE."
  ) +
  theme_burrow +
  theme(legend.position = "right", legend.title = element_blank(),
        legend.text = element_text(color = "#52514e"))

ggsave(file.path(FIG_DIR, "fig4_calls_by_temperature_habitat.png"), fig4,
       width = 7.5, height = 5.2, dpi = 300, bg = "white")
