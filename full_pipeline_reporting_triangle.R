# ============================================================
# FULL PIPELINE: Reporting Triangle
# Outcomes: Diagnoses, Positive Tests, Inpatient Hospitalizations
# ============================================================

library(dplyr)
library(tidyr)
library(ggplot2)

# ------------------------------------------------------------
# STEP 1: Add report_week to each dataset (max Week in that snapshot)
# ------------------------------------------------------------

data1$report_week <- max(as.Date(data1$Week))
data2$report_week <- max(as.Date(data2$Week))
data3$report_week <- max(as.Date(data3$Week))
data4$report_week <- max(as.Date(data4$Week))
data5$report_week <- max(as.Date(data5$Week))
data6$report_week <- max(as.Date(data6$Week))
data7$report_week <- max(as.Date(data7$Week))
data8$report_week <- max(as.Date(data8$Week))

# ------------------------------------------------------------
# STEP 2: Combine all datasets
# ------------------------------------------------------------

combined_data <- bind_rows(data1, data2, data3, data4,
                           data5, data6, data7, data8) %>%
  mutate(reference_week = as.Date(Week),
         report_week    = as.Date(report_week))

cat("Combined rows:", nrow(combined_data), "\n")
cat("Unique report_weeks:", paste(sort(unique(combined_data$report_week)), collapse = ", "), "\n")
cat("Unique reference_weeks:", length(unique(combined_data$reference_week)), "\n")

# ------------------------------------------------------------
# STEP 3: Helper function — build reporting triangle for any outcome
# ------------------------------------------------------------

build_triangle <- function(data, outcome_col) {

  df <- data %>%
    select(State, reference_week, report_week, value = all_of(outcome_col)) %>%
    filter(reference_week >= as.Date("2025-11-08")) %>%
    mutate(delay_weeks = as.integer(
      as.numeric(difftime(report_week, reference_week, units = "weeks"))
    ))

  # Aggregate across states
  triangle_long <- df %>%
    group_by(reference_week, delay_weeks) %>%
    summarise(value = sum(value, na.rm = TRUE), .groups = "drop")

  # Pivot wide — columns are delay weeks
  triangle_wide <- triangle_long %>%
    pivot_wider(names_from  = delay_weeks,
                values_from = value) %>%
    arrange(reference_week)

  # Sort columns numerically: reference_week, 0, 1, 2, 3, ...
  delay_cols <- setdiff(colnames(triangle_wide), "reference_week")
  delay_cols_sorted <- as.character(sort(as.integer(delay_cols)))

  triangle_wide <- triangle_wide %>%
    select(reference_week, all_of(delay_cols_sorted))

  return(triangle_wide)
}

# ------------------------------------------------------------
# STEP 4: Build the three reporting triangles
# ------------------------------------------------------------

reporting_triangle_Diag <- build_triangle(combined_data, "Weekly_Diagnoses")
reporting_triangle_PT   <- build_triangle(combined_data, "Weekly_Positive_Tests")
reporting_triangle_Hos  <- build_triangle(combined_data, "Weekly_Inpatient_Hospitalizations")

# Quick check
cat("\n--- Diagnoses Triangle ---\n")
print(reporting_triangle_Diag)

cat("\n--- Positive Tests Triangle ---\n")
print(reporting_triangle_PT)

cat("\n--- Hospitalizations Triangle ---\n")
print(reporting_triangle_Hos)

# ------------------------------------------------------------
# STEP 5: Helper function — plot reporting triangle heatmap
# ------------------------------------------------------------

plot_triangle <- function(triangle_wide, title_label, output_file) {

  # Reshape to long
  plot_data <- triangle_wide %>%
    pivot_longer(cols      = -reference_week,
                 names_to  = "delay_weeks",
                 values_to = "count") %>%
    mutate(delay_weeks    = as.integer(delay_weeks),
           reference_week = as.Date(reference_week)) %>%
    filter(!is.na(count))

  if (nrow(plot_data) == 0) {
    warning("No data to plot for: ", title_label)
    return(NULL)
  }

  max_delay  <- max(plot_data$delay_weeks)
  all_weeks  <- sort(unique(plot_data$reference_week))

  # Factor levels — numeric delay order, newest week on top
  plot_data <- plot_data %>%
    mutate(
      delay_label = factor(
        paste0("Delay_", delay_weeks, "_Weeks"),
        levels = paste0("Delay_", 0:max_delay, "_Weeks")
      ),
      week_label = factor(
        format(reference_week, "%Y-%m-%d"),
        levels = rev(format(all_weeks, "%Y-%m-%d"))
      )
    )

  p <- ggplot(plot_data,
              aes(x = delay_label, y = week_label, fill = count)) +

    geom_tile(color = "white", linewidth = 0.6) +

    geom_text(aes(label = count),
              color    = "white",
              fontface = "bold",
              size     = 3.2) +

    scale_fill_gradientn(
      colours  = c("#c8e6c0", "#81c784", "#388e3c", "#1b5e20"),
      na.value = "#eeeeee",
      name     = "Count",
      guide    = guide_colorbar(
        barwidth       = 0.8,
        barheight      = 8,
        ticks          = FALSE,
        title.position = "top"
      )
    ) +

    scale_x_discrete(
      labels = function(x) gsub("_", "\n", x)
    ) +

    labs(
      title    = paste("Reporting Triangle:", title_label),
      subtitle = "Snapshot of the last 12 weeks showing reporting lag",
      x        = "Reporting Delay (Weeks)",
      y        = "Week of Occurrence"
    ) +

    theme_minimal(base_size = 12) +
    theme(
      plot.title        = element_text(face = "bold", size = 13, margin = margin(b = 4)),
      plot.subtitle     = element_text(size = 10, color = "grey40", margin = margin(b = 12)),
      axis.title.x      = element_text(size = 10, margin = margin(t = 10)),
      axis.title.y      = element_text(size = 10, margin = margin(r = 10)),
      axis.text.x       = element_text(size = 7.5, color = "grey20", lineheight = 1.3),
      axis.text.y       = element_text(size = 9,   color = "grey20"),
      axis.ticks        = element_blank(),
      panel.grid        = element_blank(),
      legend.position   = "right",
      legend.title      = element_text(size = 9, face = "bold"),
      legend.text       = element_text(size = 8),
      plot.background   = element_rect(fill = "white", color = NA),
      panel.background  = element_rect(fill = "#f5f5f5", color = NA),
      plot.margin       = margin(16, 16, 16, 16)
    )

  ggsave(output_file, plot = p,
         width = 13, height = 7, dpi = 180, bg = "white")

  cat("Saved:", output_file, "\n")
  return(p)
}

# ------------------------------------------------------------
# STEP 6: Generate and save all 3 plots
# ------------------------------------------------------------

p1 <- plot_triangle(reporting_triangle_Diag,
                    "Weekly Diagnoses",
                    "triangle_diagnoses.png")

p2 <- plot_triangle(reporting_triangle_PT,
                    "Weekly Positive Tests",
                    "triangle_positive_tests.png")

p3 <- plot_triangle(reporting_triangle_Hos,
                    "Weekly Inpatient Hospitalizations",
                    "triangle_hospitalizations.png")

# View in RStudio plot pane
print(p1)
print(p2)
print(p3)

# ------------------------------------------------------------
# STEP 7: Delay analysis for Diagnoses (median reporting delay)
# ------------------------------------------------------------

delay_analysis <- combined_data %>%
  select(State, reference_week, report_week, diagnoses = Weekly_Diagnoses) %>%
  filter(reference_week >= as.Date("2025-11-08")) %>%
  mutate(delay_weeks = as.integer(
    as.numeric(difftime(report_week, reference_week, units = "weeks"))
  )) %>%
  group_by(delay_weeks) %>%
  summarise(total = sum(diagnoses, na.rm = TRUE), .groups = "drop") %>%
  arrange(delay_weeks) %>%
  mutate(
    cumulative  = cumsum(total),
    percentage  = cumulative / sum(total) * 100
  )

cat("\n--- Delay Analysis (Diagnoses) ---\n")
print(delay_analysis)

median_delay <- delay_analysis %>%
  filter(percentage >= 50) %>%
  slice(1)

cat("\nMedian reporting delay (50% data complete by week):\n")
print(median_delay)
