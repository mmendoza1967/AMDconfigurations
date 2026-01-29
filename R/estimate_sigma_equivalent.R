#' Estimate the sigma-equivalent compactness of a dataset
#'
#' This function calibrates the observed AMD peak of real data against synthetic
#' datasets generated with varying levels of isotropic Gaussian noise (\eqn{\sigma}).
#' For each candidate \eqn{\sigma}, synthetic data are generated with the same
#' number of samples, dimensionality, and number of clusters as the real data.
#' The AMD peak of each synthetic dataset is computed, and the \emph{sigma-equivalent}
#' value is defined as the \eqn{\sigma} whose synthetic AMD peak best matches the
#' real AMD peak (either by interpolation or nearest match).
#'
#' @param real_data A numeric matrix or data frame of samples × features.
#' @param its Number of random initialisations per AMD computation.
#' @param nin Minimum number of clusters to evaluate.
#' @param nsp Maximum number of clusters to evaluate.
#' @param k_opt Optional; the optimal number of clusters for the real data.
#'   If \code{NULL}, it is estimated internally.
#' @param sigmas Numeric vector of candidate \eqn{\sigma} values to evaluate.
#' @param iter_max Maximum number of iterations for fuzzy c-means.
#' @param make_plot Logical; if \code{TRUE}, produce a comparative plot of
#'   \eqn{\sigma} vs synthetic AMD peaks.
#' @param return_plot Logical; if \code{TRUE}, return the comparative plot object.
#' @param quiet Logical; suppress console output from synthetic data generation.
#' @param open_device_each Logical; if \code{TRUE}, open a new graphics device
#'   for each sigma-curve plot (when \code{plot_sigma_curves = TRUE}).
#' @param device_width,device_height Size of graphics device for sigma-curve plots.
#' @param cube_size Side length of the hypercube used to place synthetic centroids.
#' @param method Method for estimating sigma-equivalent: \code{"interpolate"} or
#'   \code{"nearest"}.
#' @param standardize Logical; if \code{TRUE}, standardise synthetic data.
#' @param seed_base Base seed for reproducibility.
#' @param plot_sigma_curves Logical; if \code{TRUE}, plot the AMD curve for each
#'   candidate \eqn{\sigma}.
#'
#' @return A list containing:
#' \describe{
#'   \item{amd_real_peak}{AMD peak of the real dataset.}
#'   \item{k_opt}{Optimal number of clusters for the real data.}
#'   \item{table_sigma_amd}{Data frame of \eqn{\sigma} vs synthetic AMD peaks.}
#'   \item{sigma_equivalent}{Interpolated sigma-equivalent value.}
#'   \item{sigma_eq}{Nearest-match sigma on the explored grid.}
#'   \item{extrapolated}{Logical; whether interpolation required extrapolation.}
#'   \item{plot_comparative}{Comparative plot object (if requested).}
#'   \item{best_i}{Index of best-matching sigma.}
#'   \item{best_sigma}{Best-matching sigma value.}
#'   \item{best_res_syn}{Full AMD results for the best synthetic dataset.}
#'   \item{best_df_curve}{Data frame of the AMD curve for the best sigma.}
#' }
#'
#' @examples
#' \dontrun{
#' set.seed(1)
#' X <- matrix(rnorm(1000), nrow = 100, ncol = 10)
#' out <- estimate_sigma_equivalent(
#'   real_data = X,
#'   its = 5,
#'   nin = 2,
#'   nsp = 6,
#'   sigmas = seq(1, 10, by = 2)
#' )
#' out$sigma_equivalent
#' }
#'
#' @export
estimate_sigma_equivalent <- function(real_data, its, nin, nsp,
                                      k_opt = NULL,
                                      sigmas,
                                      iter_max = 20,
                                      make_plot = FALSE,
                                      return_plot = TRUE,
                                      quiet = TRUE,
                                      open_device_each = FALSE,
                                      device_width = 7, device_height = 5,
                                      cube_size = 100,
                                      method = c("interpolate", "nearest"),
                                      standardize = FALSE,
                                      seed_base = 7,
                                      plot_sigma_curves = FALSE) {

  method <- match.arg(method)

  # --------------------------------------------------------------------------
  # 1. Prepare real data
  # --------------------------------------------------------------------------
  X_real <- as.data.frame(real_data)

  keep <- sapply(X_real, is.numeric)
  if (!all(keep)) X_real <- X_real[, keep, drop = FALSE]

  if (anyNA(X_real))
    stop("There are NAs in 'real_data'. Please impute or remove them first.")

  stopifnot(nin >= 2, nsp >= nin, nsp <= nrow(X_real))
  stopifnot(length(sigmas) >= 2, all(is.finite(sigmas)), all(sigmas > 0))

  # Wrapper for AMD computation with deterministic seeds
  compute_amd_compat <- function(X, its, nin, nsp, scale_data, iter_max, seed_base = NULL) {
    P <- (nsp - nin + 1L)
    seeds_vec <- if (is.null(seed_base)) NULL else (seed_base + seq_len(its * P))

    compute_amd_curve(
      X, its = its, nin = nin, nsp = nsp,
      seeds = seeds_vec,
      verbose = FALSE,
      plot_curve = FALSE,
      open_device = FALSE,
      scale_data = scale_data,
      iter_max = iter_max,
      preselect_top_sd = NULL
    )
  }

  # --------------------------------------------------------------------------
  # 2. Real AMD peak
  # --------------------------------------------------------------------------
  res_real <- compute_amd_compat(
    X_real, its = its, nin = nin, nsp = nsp,
    scale_data = standardize, iter_max = iter_max,
    seed_base = seed_base
  )

  amd_real_peak <- suppressWarnings(max(res_real$max, na.rm = TRUE))

  if (!is.finite(amd_real_peak))
    stop("Real AMD peak is not finite. Check the real AMD computation.")

  if (is.null(k_opt))
    k_opt <- res_real$k_opt

  # --------------------------------------------------------------------------
  # 3. Sweep over sigma values
  # --------------------------------------------------------------------------
  peaks <- rep(NA_real_, length(sigmas))

  n_samples <- nrow(X_real)
  n_dim     <- ncol(X_real)

  best_i        <- NA_integer_
  best_delta    <- Inf
  best_res_syn  <- NULL
  best_df_curve <- NULL

  for (i in seq_along(sigmas)) {

    set.seed(seed_base + i)

    # Generate synthetic dataset
    if (quiet) {
      invisible(capture.output(
        syn <- create_synthetic_samples(
          n_samples, k_opt,
          std_dev = sigmas[i],
          n_dim = n_dim,
          cube_size = cube_size,
          standardize = standardize
        )
      ))
    } else {
      syn <- create_synthetic_samples(
        n_samples, k_opt,
        std_dev = sigmas[i],
        n_dim = n_dim,
        cube_size = cube_size,
        standardize = standardize
      )
    }

    # Compute synthetic AMD curve
    res_syn <- compute_amd_compat(
      syn, its = its, nin = nin, nsp = nsp,
      scale_data = standardize, iter_max = iter_max,
      seed_base = seed_base + i * 10000L
    )

    # Extract synthetic AMD peak
    peaks_i <- suppressWarnings(max(res_syn$max, na.rm = TRUE))
    if (!is.finite(peaks_i)) peaks_i <- NA_real_
    peaks[i] <- peaks_i

    # Track best match
    if (is.finite(peaks_i)) {
      delta <- abs(peaks_i - amd_real_peak)

      if (delta < best_delta) {
        best_delta <- delta
        best_i     <- i
        best_res_syn <- res_syn

        ks <- suppressWarnings(as.numeric(colnames(res_syn$raw)))
        if (anyNA(ks)) ks <- nin:nsp

        mu  <- as.numeric(res_syn$mean)
        sdv <- if (its <= 1) rep(0, length(mu)) else apply(res_syn$raw, 2, sd, na.rm = TRUE)

        keep_curve <- is.finite(ks) & is.finite(mu)

        best_df_curve <- data.frame(
          k    = ks[keep_curve],
          mean = mu[keep_curve],
          sd   = sdv[keep_curve]
        )

        if (nrow(best_df_curve) == 0) {
          best_df_curve <- data.frame(k = numeric(0), mean = numeric(0), sd = numeric(0))
        }
      }
    }

    # Optional: plot AMD curve for each sigma
    if (isTRUE(plot_sigma_curves)) {
      if (open_device_each) {
        if (.Platform$OS.type == "windows") windows(width = device_width, height = device_height)
        else if (Sys.info()[["sysname"]] == "Darwin") quartz(width = device_width, height = device_height)
        else x11(width = device_width, height = device_height)
      }

      ks <- suppressWarnings(as.numeric(colnames(res_syn$raw)))
      if (anyNA(ks)) ks <- nin:nsp

      mu  <- as.numeric(res_syn$mean)
      sdv <- if (its <= 1) rep(0, length(mu)) else apply(res_syn$raw, 2, sd, na.rm = TRUE)

      keep_curve <- is.finite(ks) & is.finite(mu)
      df_plot <- data.frame(k = ks[keep_curve], mean = mu[keep_curve], sd = sdv[keep_curve])

      if (nrow(df_plot) > 0) {
        p_sigma <- ggplot2::ggplot(df_plot, ggplot2::aes(x = k, y = mean)) +
          ggplot2::geom_line() +
          ggplot2::geom_point() +
          ggplot2::geom_ribbon(ggplot2::aes(ymin = mean - sd, ymax = mean + sd), alpha = 0.15) +
          ggplot2::geom_vline(xintercept = k_opt, linetype = 3) +
          ggplot2::geom_hline(yintercept = amd_real_peak, linetype = 2, alpha = 0.6) +
          ggplot2::labs(
            x = "Number of clusters (k)",
            y = "AMD (mean ± sd)",
            title = sprintf("Synthetic AMD curve — σ = %s | AMD peak = %s",
                            sigmas[i],
                            ifelse(is.finite(peaks_i), sprintf("%.3f", peaks_i), "NA")),
            subtitle = sprintf("k_opt (real): %s", k_opt)
          ) +
          ggplot2::theme_minimal()

        print(p_sigma)
      }
    }
  }

  # --------------------------------------------------------------------------
  # 4. Sigma vs AMD peak table
  # --------------------------------------------------------------------------
  table_sigma_amd <- data.frame(
    sigma   = as.numeric(sigmas),
    AMD_max = as.numeric(peaks)
  )

  # --------------------------------------------------------------------------
  # 5. Estimate sigma-equivalent
  # --------------------------------------------------------------------------
  finite_tbl <- table_sigma_amd[is.finite(table_sigma_amd$AMD_max), , drop = FALSE]

  extrapolated      <- NA
  sigma_equivalent  <- NA_real_
  sigma_eq          <- NA_real_

  # Nearest match
  if (nrow(finite_tbl) >= 1) {
    idx_nearest <- which.min(abs(finite_tbl$AMD_max - amd_real_peak))
    sigma_eq <- finite_tbl$sigma[idx_nearest]
  }

  # Interpolated sigma-equivalent
  if (nrow(finite_tbl) >= 2) {
    ord <- order(finite_tbl$AMD_max)
    amd_x <- finite_tbl$AMD_max[ord]
    sig_y <- finite_tbl$sigma[ord]

    extrapolated <- (amd_real_peak < min(amd_x)) || (amd_real_peak > max(amd_x))

    sigma_equivalent <- if (method == "interpolate") {
      approx(x = amd_x, y = sig_y, xout = amd_real_peak, rule = 2)$y
    } else {
      sig_y[which.min(abs(amd_x - amd_real_peak))]
    }
  }

  # Ensure best_df_curve is valid
  if (is.na(best_i) || is.null(best_df_curve) || nrow(best_df_curve) == 0) {
    best_i        <- NA_integer_
    best_res_syn  <- NULL
    best_df_curve <- data.frame(k = numeric(0), mean = numeric(0), sd = numeric(0))
  }

  # --------------------------------------------------------------------------
  # 6. Optional comparative plot
  # --------------------------------------------------------------------------
  p_comp <- NULL

  if (isTRUE(make_plot)) {
    p_comp <- ggplot2::ggplot(table_sigma_amd, ggplot2::aes(x = sigma, y = AMD_max)) +
      ggplot2::geom_line() +
      ggplot2::geom_point() +
      ggplot2::geom_hline(yintercept = amd_real_peak, linetype = 2) +
      ggplot2::geom_vline(xintercept = sigma_equivalent, linetype = 3, na.rm = TRUE) +
      ggplot2::labs(
        x = expression(sigma),
        y = "Synthetic AMD peak",
        title = expression(paste("Compactness: ", sigma, "-equivalent estimate")),
        subtitle = sprintf("k_opt = %s", k_opt)
      ) +
      ggplot2::theme_minimal()

    print(p_comp)
  }

  # --------------------------------------------------------------------------
  # 7. Output
  # --------------------------------------------------------------------------
  list(
    amd_real_peak    = amd_real_peak,
    k_opt            = k_opt,
    table_sigma_amd  = table_sigma_amd,
    sigma_equivalent = sigma_equivalent,
    sigma_eq         = sigma_eq,
    extrapolated     = extrapolated,
    plot_comparative = if (return_plot) p_comp else NULL,
    best_i           = best_i,
    best_sigma       = if (is.na(best_i)) NA_real_ else as.numeric(sigmas[best_i]),
    best_res_syn     = best_res_syn,
    best_df_curve    = best_df_curve
  )
}
