


library(data.table)


density_out <- fread("datatable/density_detrend.csv")
gross_out <- fread("datatable/density_gross.csv")
# Compute quantiles by var/model/scenario from a wide table like density_out
compute_quantiles <- function(dt,
                              value_regex = "^iav_.*_y_\\d{4}$",  # matches iav_*_y_YYYY columns
                              probs = c(0.05, 0.10, 0.90, 0.95),
                              keep_cols = c("var","model","scenario","x","y")) {
  stopifnot(is.data.table(dt))
  
  # Identify measure (value) columns
  val_cols <- grep(value_regex, names(dt), value = TRUE)
  if (length(val_cols) == 0L) stop("No columns match value_regex: ", value_regex)
  
  # Melt to long: one row per cell-year value
  long <- melt(
    dt,
    id.vars = intersect(keep_cols, names(dt)), # keep these; won't be used in the aggregation
    measure.vars = val_cols,
    variable.name = "metric_year",
    value.name = "value",
    variable.factor = FALSE
  )
  
  # Extract metric (e.g., iav_linear) and numeric year
  long[, c("metric", "year") := tstrsplit(metric_year, "_y_", fixed = TRUE)]
  long[, year := as.integer(year)]
  long[, metric_year := NULL]
  
  # Helper to emit named quantile columns
  qnames <- paste0("q", sprintf("%02d", probs * 100))
  calc_q <- function(v) {
    qs <- quantile(v, probs = probs, na.rm = TRUE, names = FALSE)
    as.list(setNames(as.numeric(qs), qnames))
  }
  

  # Quantiles pooled across all years — by (var, model, scenario, metric)
  all_years <- long[, calc_q(value), by = .(var, model, scenario, metric)]
  setcolorder(all_years, c("var","model","scenario","metric", qnames))
  setorder(all_years, var, model, scenario, metric)
  
  list(all_years = all_years)
}

q_density <- compute_quantiles(density_out)
q_gross <- compute_quantiles(gross_out)


fwrite(q_density$all_years,"datatable/ca_threshold_density.csv")
fwrite(q_gross$all_years,"datatable/ca_threshold_gross.csv")
