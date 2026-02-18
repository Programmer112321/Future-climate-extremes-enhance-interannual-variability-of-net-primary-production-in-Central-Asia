# ---- Libraries ----
library(data.table)
library(ggplot2)
library(ggpubr)   # for stat_cor()
library(tidyr)    # for pivot_longer()
library(dplyr)    # just for the final left_join (tidyverse join convenience)

# Climate sensitivity analysis -----------

# ---- Load data ----
yeardata_growing_density_frame <- fread("datatable/ca_growing_density_year.csv")
yeardata_density_frame         <- fread("datatable/ca_density_year.csv")

# Ensure data.table types
setDT(yeardata_growing_density_frame)
setDT(yeardata_density_frame)

# ---- Drop early years (2015:2020) from the "growing" table ----
cols_drop <- paste0("y_", 2015:2020)
cols_drop <- intersect(cols_drop, names(yeardata_growing_density_frame))  # only those that exist
if (length(cols_drop)) {
  yeardata_growing_density_frame[, (cols_drop) := NULL]
}

# ---- Define the year columns you want to average across models ----
year_cols <- paste0("y_", 2021:2100)
year_cols_growing <- intersect(year_cols, names(yeardata_growing_density_frame))
year_cols_normal  <- intersect(year_cols, names(yeardata_density_frame))

# Basic guardrails
stopifnot(all(c("var", "scenario") %in% names(yeardata_density_frame)))
stopifnot(all(c("var", "scenario") %in% names(yeardata_growing_density_frame)))
stopifnot(length(year_cols_growing) > 0, length(year_cols_normal) > 0)

# ---- Aggregate: mean across records by var, scenario (retain per-year columns) ----
# Result is: one row per (var, scenario), with columns y_2021: y_2100 as means
res_normal <- yeardata_density_frame[
  ,
  lapply(.SD, function(x) mean(x, na.rm = TRUE)),
  by = .(var, scenario),
  .SDcols = year_cols_normal
]

res_growing <- yeardata_growing_density_frame[
  ,
  lapply(.SD, function(x) mean(x, na.rm = TRUE)),
  by = .(var, scenario),
  .SDcols = year_cols_growing
]

# ---- Long format for plotting & joining ----
# growing: keep a value column named "growing"
growing_long <- res_growing |>
  pivot_longer(
    cols = all_of(year_cols_growing),
    names_to = "year",
    values_to = "growing"
  )

# normal: keep a value column named "value"
normal_long <- res_normal |>
  pivot_longer(
    cols = all_of(year_cols_normal),
    names_to = "year",
    values_to = "value"
  )

# ---- Join on var, scenario, year ----
# This ensures we compare the SAME (var, scenario, year) across the two datasets
test_data <- left_join(
  growing_long,
  normal_long,
  by = c("var", "scenario", "year")
)

# Optional: remove any rows where one side is missing
test_data <- test_data |> filter(is.finite(growing) & is.finite(value))

# ---- Plot: scatter with correlation (per facet) ----
# Choose correlation method: "pearson" or "spearman"
corr_method <- "pearson"


cor


p <- ggplot(test_data, aes(x = growing, y = value)) +
  geom_point(alpha = 0.6, size = 1.6, color = "#2c7fb8") +
  
  # 1:1 reference line
  geom_abline(slope = 1, intercept = 0, 
              color = "red", linetype = "dashed", size = 0.7) +
  
  # linear regression fit
  geom_smooth(method = "lm", se = FALSE, 
              color = "black", size = 0.8) +
  
  # correlation + p-value label
  stat_cor(
    method = corr_method,
    label.x.npc = "left",
    label.y.npc = "top",
    size = 3.5,
    output.type = "text"   # ensures both R and p are shown
  )+
  
  facet_wrap(~ var, scales = "free") +
  theme_bw() +
  labs(
    x = "Growing (per-year mean)",
    y = "Value (per-year mean)",
    title = "Growing vs Value with Linear Fit and 1:1 Line"
  )


print(p)
