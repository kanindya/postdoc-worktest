# Load libraries
library(haven)
library(dplyr)
library(tidycmprsk)
library(ggsurvfit)
library(ggplot2)
library(patchwork)

# ── Panel A: KM overall survival ──────────────────────────────────────────────
km_plot <- km_fit %>%
  ggsurvfit(linewidth = 0.7) +
  add_confidence_interval() +
  add_risktable() +
  
  scale_y_continuous(
    limits = c(0, 1), 
    breaks = seq(0, 1, 0.25),
    labels = function(x) x * 100
  ) +
  scale_x_continuous(
    breaks = seq(0, 120, by = 12),
    limits = c(0, 120)
  ) +
  
  scale_color_manual(
    name = "Treatment group",  # same name in both plots
    values = c("#157B8C", "#E15759"),
    labels = c("Standard treatment", "Standard + adjunctive treatment")
  ) +
  scale_fill_manual(
    name = "Treatment group",  # same name in both plots
    values = c("#157B8C", "#E15759"),
    labels = c("Standard treatment", "Standard + adjunctive treatment")
  ) +
  
  labs(title = "A", x = "Months of follow-up", y = "Overall survival (%)") +
  
  theme(
    legend.position = "bottom",
    plot.title = element_text(face = "bold", hjust = 0),
    panel.background = element_rect(fill = "#F2F2F2", color = NA),
    panel.grid.major = element_line(color = "white", linewidth = 0.5),
    axis.title.x = element_text(size = 10)
  )

# ── Panel B: CIF complications ────────────────────────────────────────────────
cif_plot <- cif_fit %>%
  ggcuminc(outcome = "Complication", linewidth = 0.7) +
  
  add_confidence_interval() +
  add_risktable() +
  
  scale_y_continuous(
    limits = c(0, 0.21), 
    breaks = seq(0, 0.20, 0.05),
    labels = function(x) x * 100
  ) +
  scale_x_continuous(
    breaks = seq(0, 120, by = 12),
    limits = c(0, 120)
  ) +
  
  scale_color_manual(
    name = "Treatment group",  # same name as Panel A
    values = c("#157B8C", "#E15759"),
    labels = c("Standard treatment", "Standard + adjunctive treatment")
  ) +
  scale_fill_manual(
    name = "Treatment group",  # same name as Panel A
    values = c("#157B8C", "#E15759"),
    labels = c("Standard treatment", "Standard + adjunctive treatment")
  ) +
  
  labs(title = "B", x = "Months of follow-up", y = "Cumulative incidence of complications (%)") +
  
  theme(
    legend.position = "bottom",
    plot.title = element_text(face = "bold", hjust = 0),
    panel.background = element_rect(fill = "#F2F2F2", color = NA),
    panel.grid.major = element_line(color = "white", linewidth = 0.5),
    axis.title.x = element_text(size = 10)
  )

print(cif_plot)

# ── Combine ───────────────────────────────────────────────────────────────────
combined_plot <- (km_plot | plot_spacer() | cif_plot) + 
  plot_layout(widths = c(1, 0.05, 1), guides = "collect") & 
  theme(legend.position = "bottom")

print(combined_plot)

ggsave(
  filename = "/Users/kanyaanindya/Documents/11. Interview/02. KI - MASLD/Work test/04. Result/Figure 1",
  plot = combined_plot, # Use the new object name
  width = 14,
  height = 7,
  dpi = 300
)