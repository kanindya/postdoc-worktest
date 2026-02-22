# Load libraries
library(haven)
library(dplyr)
library(tidycmprsk)
library(ggsurvfit)
library(ggplot2)
library(patchwork)

# ── Load data ─────────────────────────────────────────────────────────────────
df <- read_dta("/Users/kanyaanindya/Documents/11. Interview/02. KI - MASLD/Work test/04. Result/worktest_v01.dta")

df <- df %>%
  mutate(
    time_months = cr_time / 30.4375,
    os_months   = uppfoljningstid / 30.4375,
    treatment = factor(behandlingsgrupp1,
                       levels = c(0, 1),  
                       labels = c("Standard treatment", 
                                  "Standard + adjunctive treatment")),
    status = factor(failtype,
                    levels = c(0, 1, 2),
                    labels = c("Censored", "Complication", "Death"))
  )
# ── Fit models ────────────────────────────────────────────────────────────────
km_fit  <- survfit(Surv(os_months, dod) ~ treatment, data = df)
cif_fit <- cuminc(Surv(time_months, status) ~ treatment, data = df)

# Check levelling
levels(df$treatment)
cif_fit$tidy$strata |> unique()

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
    values = c("#157B8C", "#E15759"),
    labels = c("Standard treatment", "Standard + adjunctive treatment")
  ) +
  scale_fill_manual(
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

print(km_plot)
km_fit

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
  
  scale_color_manual(values = c(
    "Standard treatment" = "#157B8C",               
    "Standard + adjunctive treatment" = "#E15759" 
  )) +
  scale_fill_manual(values = c(
    "Standard treatment" = "#157B8C", 
    "Standard + adjunctive treatment" = "#E15759"
  )) +
  
  labs(title = "B", x = "Months of follow-up", y = "Cumulative incidence of complications (%)") +
  
  theme(
    legend.position = "bottom",
    plot.title = element_text(face = "bold", hjust = 0),
    panel.background = element_rect(fill = "#F2F2F2", color = NA),
    panel.grid.major = element_line(color = "white", linewidth = 0.5),
    axis.title.x = element_text(size = 10)
  )

print(cif_plot)
cif_fit 

# ── Combine ───────────────────────────────────────────────────────────────────
combined_plot <- (km_plot | plot_spacer() | cif_plot) +
  plot_layout(widths = c(1, 0.05, 1))
print(combined_plot)

ggsave(
  filename = "/Users/kanyaanindya/Documents/11. Interview/02. KI - MASLD/Work test/07. Figure/Figure 1.png",
  plot = combined_plot,
  width = 14,
  height = 7,
  dpi = 300
)