library(data.table)
library(parallel)


yeardata_density_frame <- fread("datatable/ca_density_year.csv")
# --- setup functions as before ----------------------------------------------

# --- detrending functions (exactly as you defined) --------------------------
detrend_series <- function(x,
                           method = c("linear", "loess", "poly"),
                           span = 0.35,
                           loess_degree = 1,
                           poly_deg = 2,
                           min_obs = 5) {
  n <- length(x)
  t <- seq_len(n)
  
  ok <- is.finite(x)
  if (sum(ok) < min_obs) return(rep(NA_real_, n))
  
  method <- match.arg(method)
  
  if (method == "linear") {
    fit <- stats::lm(x[ok] ~ t[ok])
    trend <- stats::predict(fit, newdata = data.frame(t = t))
  } else if (method == "poly") {
    fit <- stats::lm(x[ok] ~ stats::poly(t[ok], degree = poly_deg, raw = TRUE))
    trend <- stats::predict(fit, newdata = data.frame(t = t))
  } else if (method == "loess") {
    fit <- stats::loess(x[ok] ~ t[ok],
                        span   = span,
                        degree = loess_degree,
                        control = stats::loess.control(surface = "direct",
                                                       trace.hat = "approx"))
    trend <- stats::predict(fit, newdata = data.frame(t = t))
  }
  
  x - trend
}

detrend_linear <- function(v) detrend_series(v, method = "linear")
detrend_loess  <- function(v) detrend_series(v, method = "loess", span = 0.35, loess_degree = 1)
detrend_poly2  <- function(v) detrend_series(v, method = "poly", poly_deg = 2)
detrend_poly3  <- function(v) detrend_series(v, method = "poly", poly_deg = 3)

# --- Convert to data.table --------------------------------------------------
dt <- as.data.table(yeardata_density_frame)
year_cols <- grep("^y_\\d{4}$", names(dt), value = TRUE)

# --- Parallel processing with data.table ------------------------------------
ncores <- max(1L, parallel::detectCores() - 1L)

# Split data into chunks for parallel processing
n_rows <- nrow(dt)
chunk_size <- ceiling(n_rows / ncores)
dt[, chunk_id := rep(1:ncores, each = chunk_size, length.out = n_rows)]

# Function to detrend a chunk - returns ONLY detrended columns
detrend_chunk <- function(chunk_dt, year_cols, detrend_fun, method_name) {
  # Extract year columns as matrix
  vals_matrix <- as.matrix(chunk_dt[, ..year_cols])
  
  # Apply detrending row-wise
  detrended_matrix <- t(apply(vals_matrix, 1, detrend_fun))
  
  # Convert to data.table and set column names
  detrended_dt <- as.data.table(detrended_matrix)
  setnames(detrended_dt, paste0(method_name, "_", year_cols))
  
  return(detrended_dt)
}

# Setup cluster
cl <- parallel::makeCluster(ncores, type = "PSOCK")
parallel::clusterExport(cl, c("detrend_series", "detrend_linear", "detrend_loess", 
                              "detrend_poly2", "year_cols"), envir = environment())
parallel::clusterEvalQ(cl, library(data.table))

# Split data by chunks
dt_list <- split(dt, dt$chunk_id)

# Process each method in parallel - these return ONLY the new columns
dt_list_linear <- parallel::parLapply(cl, dt_list, detrend_chunk, 
                                      year_cols = year_cols, 
                                      detrend_fun = detrend_linear,
                                      method_name = "iav_linear")

dt_list_loess <- parallel::parLapply(cl, dt_list, detrend_chunk,
                                     year_cols = year_cols,
                                     detrend_fun = detrend_loess,
                                     method_name = "iav_loess")

dt_list_poly2 <- parallel::parLapply(cl, dt_list, detrend_chunk,
                                     year_cols = year_cols,
                                     detrend_fun = detrend_poly2,
                                     method_name = "iav_poly2")

parallel::stopCluster(cl)

# Combine chunks for each method
dt_iav_linear <- rbindlist(dt_list_linear)
dt_iav_loess <- rbindlist(dt_list_loess)
dt_iav_poly2 <- rbindlist(dt_list_poly2)

# Remove chunk_id from original data




# Sensitivity analysis  ---------


## --- 0) Ensure DT format and a common ID column -------------------------------
setDT(dt_iav_linear); setDT(dt_iav_loess); setDT(dt_iav_poly2)

# Detect an existing ID column or create one
detect_id_col <- function(DT) {
  cand <- intersect(names(DT), c("cell_id","grid_id","id","index",".cell_id"))
  if (length(cand)) cand[1] else NULL
}

ensure_id <- function(DT) {
  idc <- detect_id_col(DT)
  if (is.null(idc)) {
    DT[, .cell_id := .I]
    idc <- ".cell_id"
  }
  # Rename to a common name "id" for all three tables
  if (idc != "id") setnames(DT, idc, "id")
  invisible(NULL)
}

ensure_id(dt_iav_linear); ensure_id(dt_iav_loess); ensure_id(dt_iav_poly2)

# Keep rows aligned by 'id' (assumes the same grid and order). If not aligned,
# do a merge/join instead—here we assume alignment is OK.

## --- 1) Melt to long with method tag ------------------------------------------
year_cols <- function(DT) grep("_y_\\d{4}$", names(DT), value = TRUE)

melt_long <- function(DT, method_name) {
  yc <- year_cols(DT)
  stopifnot(length(yc) > 0)
  ans <- melt(
    DT[, c("id", yc), with = FALSE],
    id.vars = "id", measure.vars = yc,
    variable.name = "var", value.name = "residual",
    variable.factor = FALSE
  )
  ans[, `:=`(
    method = method_name,
    year   = as.integer(sub(".*_y_(\\d{4})$", "\\1", var))
  )][, var := NULL][]
}

long_lin   <- melt_long(dt_iav_linear, "linear")
long_loe   <- melt_long(dt_iav_loess,  "loess")
long_poly2 <- melt_long(dt_iav_poly2,  "poly2")

long_all <- rbindlist(list(long_lin, long_loe, long_poly2), use.names = TRUE)

## --- 2) Per-cell metrics: mean ~0, SD (IAV magnitude), ACF1 -------------------
acf1_vec <- function(v) {
  v <- v[is.finite(v)]
  if (length(v) < 3) return(NA_real_)
  as.numeric(stats::acf(v, lag.max = 1, plot = FALSE)$acf[2])
}

cell_metrics <- long_all[
  , .(
    mean_res = mean(residual, na.rm = TRUE),
    sd_res   =  sd(residual,   na.rm = TRUE),
    acf1     =  acf1_vec(residual)
  ),
  by = .(id, method)
]

# Wide table: columns like mean_res.linear, sd_res.loess, acf1.poly2
cell_wide <- dcast(
  cell_metrics, id ~ method,
  value.var = c("mean_res","sd_res","acf1")
)

## --- 3) Sanity check: residual means should be ~0 -----------------------------
summary(cell_wide[, .(mean_res_linear, mean_res_loess, mean_res_poly2)])

## --- 4) Spatial pattern robustness: Spearman of SD maps -----------------------
spearman_sd <- function(a, b) stats::cor(a, b, method = "spearman", use = "complete.obs")

rho_lin_loess   <- spearman_sd(cell_wide$sd_res_linear, cell_wide$sd_res_loess)
rho_lin_poly2   <- spearman_sd(cell_wide$sd_res_linear, cell_wide$sd_res_poly2)
rho_loess_poly2 <- spearman_sd(cell_wide$sd_res_loess,  cell_wide$sd_res_poly2)

cat(sprintf("Spearman( SD ): lin~loess=%.3f, lin~poly2=%.3f, loess~poly2=%.3f\n",
            rho_lin_loess, rho_lin_poly2, rho_loess_poly2))

## --- 5) Sign agreement and year-wise correlations -----------------------------
# Cast residuals wide by method for each (id, year)
res_wide <- dcast(
  long_all, id + year ~ method, value.var = "residual"
)

# Overall sign agreement (all years, all cells)
sign_eq <- function(x, y) {
  ok <- is.finite(x) & is.finite(y)
  if (!any(ok)) return(NA_real_)
  mean(sign(x[ok]) == sign(y[ok]))
}
sign_lin_loess <- sign_eq(res_wide$linear, res_wide$loess)



# Output the final data ------

library(data.table)
library(parallel)



run_parallel_detrend <- function(dt,
                                 year_cols_regex = "^y_\\d{4}$",
                                 detrend_fun = detrend_linear,
                                 method_name = "iav_linear",
                                 id_cols = 1:5,
                                 ncores = max(1L, detectCores() - 1L),
                                 export_funs = c("detrend_series", "detrend_linear",
                                                 "detrend_loess", "detrend_poly2",
                                                 "detrend_chunk"),
                                 return_only = FALSE,
                                 outfile = NULL) {
  stopifnot(is.data.table(dt))
  
  # Identify year columns
  year_cols <- grep(year_cols_regex, names(dt), value = TRUE)
  if (length(year_cols) == 0L) {
    stop("No year columns matched regex: ", year_cols_regex)
  }
  
  # ID columns handling
  if (is.numeric(id_cols)) {
    stopifnot(all(id_cols >= 1L & id_cols <= ncol(dt)))
    id_cols_names <- names(dt)[id_cols]
  } else {
    stopifnot(all(id_cols %in% names(dt)))
    id_cols_names <- id_cols
  }
  
  n_rows <- nrow(dt)
  if (n_rows == 0L) {
    warning("Input data.table has 0 rows; returning empty result.")
    res <- copy(dt[, ..id_cols_names])
    if (!return_only && !is.null(outfile)) fwrite(res, outfile)
    return(res)
  }
  
  # Add a stable row id to preserve global order when recombining
  dt[, `..row_id` := .I]
  
  # Chunking
  chunk_size <- ceiling(n_rows / ncores)
  dt[, chunk_id := rep(seq_len(ncores), each = chunk_size, length.out = n_rows)]
  
  # Cluster
  cl <- makeCluster(ncores, type = "PSOCK")
  on.exit({ try(stopCluster(cl), silent = TRUE) }, add = TRUE)
  
  # Export and attach packages
  clusterExport(cl,
                varlist = c(export_funs, "year_cols", "method_name"),
                envir = environment())
  clusterEvalQ(cl, library(data.table))
  
  # Split and process
  dt_list <- split(dt, by = "chunk_id", keep.by = FALSE)
  
  dt_list_newcols <- parLapply(
    cl, dt_list,
    function(chunk) {
      # IMPORTANT: pass the first argument *positionally* to match your detrend_chunk signature
      detrend_chunk(
        chunk,
        year_cols = year_cols,
        detrend_fun = detrend_fun,
        method_name = method_name
      )
    }
  )
  
  # Combine new columns
  newcols <- rbindlist(dt_list_newcols, use.names = TRUE, fill = TRUE)
  
  # Reorder by the original row order if the newcols carries ..row_id; else rely on rbind order
  # If your detrend_chunk returns only transformed year columns, you may need to
  # carry ..row_id through detrend_chunk. If it does not, we can merge by ..row_id from dt.
  if ("..row_id" %in% names(newcols)) {
    setorder(newcols, ..row_id)
    newcols[, `..row_id` := NULL]
  } else {
    # If newcols does not have ..row_id, align by row bind order—works if detrend_chunk preserves order
    # Otherwise, better approach: have detrend_chunk return ..row_id so we can sort reliably.
  }
  
  result <- cbind(dt[order(..row_id), ..id_cols_names], newcols)
  
  # Save
  if (!return_only && !is.null(outfile)) {
    outdir <- dirname(outfile)
    if (!dir.exists(outdir)) dir.create(outdir, recursive = TRUE, showWarnings = FALSE)
    fwrite(result, outfile)
  }
  
  # Cleanup scratch cols
  dt[, `:=`(chunk_id = NULL, `..row_id` = NULL)]
  
  result
}


## Save and run data -------------

# Convert to data.table if not already
density <- as.data.table(yeardata_density_frame)
gross   <- as.data.table(yeardata_gross_frame)

# Choose detrending method(s) — here: linear
# You can call the same function twice with different inputs/outputs

density_out <- run_parallel_detrend(
  dt            = density,
  year_cols_regex = "^y_\\d{4}$",
  detrend_fun   = detrend_linear,
  method_name   = "iav_linear",
  id_cols       = 1:5,
  ncores        = max(1L, parallel::detectCores() - 1L),
  outfile       = "datatable/density_detrend.csv"
)

gross_out <- run_parallel_detrend(
  dt            = gross,
  year_cols_regex = "^y_\\d{4}$",
  detrend_fun   = detrend_linear,
  method_name   = "iav_linear",
  id_cols       = 1:5,
  ncores        = max(1L, parallel::detectCores() - 1L),
  outfile       = "datatable/gross_detrend.csv"
)


