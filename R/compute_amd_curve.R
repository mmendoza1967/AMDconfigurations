#' Compute an Average Membership Degree (AMD) curve
#'
#' For every candidate number of configurations, this function runs fuzzy
#' c-means from several random initializations. Random starts are computational
#' attempts to estimate the same solution, not data replicates. Consequently,
#' the fit with the smallest fuzzy c-means objective among the converged starts
#' supplies the membership matrix evaluated by `amd_score()` at each `c`.
#'
#' AMD itself is defined independently of fuzzy c-means. Fuzzy c-means is the
#' membership-estimation backend used by this function.
#'
#' The returned `c_peak` (and its compatibility alias `c_opt`) is only the
#' numerical maximum of the evaluated curve. In particular, a maximum at the
#' boundary of the examined range is flagged and must not by itself be treated
#' as evidence for that number of groups. If several evaluated values are
#' exactly tied for the maximum, the smallest `c` is returned.
#'
#' Vector seeds, including seeds generated when `seeds = NULL`, preserve the
#' historical start-major mapping and therefore depend on the number of
#' evaluated candidates. Use a named seed matrix when comparing nested search
#' ranges. Extra named columns are allowed and ignored when their `c` is not in
#' the current range. Duplicate seeds within one candidate `c` reduce the
#' number of unique random starts and generate a warning.
#'
#' Every curve contains a `manifest_id` linking it to the process-level record
#' returned by [amd_reproducibility_manifest()]. Retrieve the manifest
#' immediately after each curve and save one copy of every distinct identifier.
#' Curves sharing an identifier can share the saved copy.
#'
#' @param data Numeric matrix or data frame. Rows are observations and columns
#'   are variables. Rows with missing values are removed with a warning and
#'   recorded in the returned row map.
#' @param its Positive integer number of random starts for every candidate `c`.
#' @param nin Minimum candidate number of configurations. Must be at least 2.
#' @param nsp Maximum candidate number of configurations.
#' @param seeds Optional positive integer vector or matrix. Vectors retain the
#'   historical start-major order; unused trailing values are ignored. If
#'   `NULL`, seeds are sampled from the caller's active random-number generator
#'   and advance its state. A matrix must have one row per start and
#'   uniquely named columns for every evaluated `c`; this form preserves the
#'   seeds of common candidates when the upper search limit is extended.
#' @param verbose Logical; print progress information when `TRUE`.
#' @param plot_curve Logical; plot the primary AMD curve when `TRUE`.
#' @param open_device Logical; when `plot_curve = TRUE`, open a new graphics
#'   device before plotting. The default is `TRUE` for compatibility with the
#'   public 0.1.0 function. It has no effect when `plot_curve = FALSE`.
#' @param scale_data Logical; standardize variables before fitting.
#' @param iter_max Positive integer maximum number of fuzzy c-means iterations.
#' @param m Fuzziness parameter; must be greater than one.
#' @param preselect_top_sd Optional positive integer. If supplied, retain only
#'   the variables with the largest standard deviations before optional
#'   scaling. Retained variables are ordered by decreasing standard deviation;
#'   their original indices and names are recorded in `settings`.
#'
#' @return An object of class `"amd_curve"`. `schema_version` identifies the
#'   public object layout. The primary `curve` contains AMD
#'   values from the minimum-objective converged fit at each `c`. Components
#'   `mean`, `max`, and `raw` retain the historical restart summaries for
#'   diagnostics and compatibility. These historical summaries include all
#'   starts, including nonconverged starts, and are not the primary curve.
#'   `selected_fits` contains the exact fitted
#'   memberships that produced the primary curve. `complete_rows` and
#'   `omitted_rows` map the fitted observations back to the input rows. The
#'   `settings` component records the input and retained variables and the
#'   exact centering and scaling vectors used to construct the fitted data.
#'   `manifest_id` links the curve to its process-level reproducibility
#'   manifest. Additional public components are `c_peak`, its historical alias
#'   `c_opt`, `peak_at_boundary`, the per-start `objectives`, `iterations` and
#'   `converged` matrices, selected-start diagnostics, the seed matrix,
#'   `coordinates` (the selected membership matrix at `c_peak`), `backend`, and
#'   `selection_rule`. `selected_fit` is the element of `selected_fits`
#'   corresponding to `c_peak`.
#'
#' @references
#' Mendoza, M. and Araújo, M. B. (2022). Biogeography of bird and mammal
#' trophic structures. *Ecography*, 2022(5), e06289.
#' \doi{10.1111/ecog.06289}
#'
#' @examples
#' set.seed(42)
#' X <- rbind(
#'   matrix(rnorm(40, mean = -2), ncol = 2),
#'   matrix(rnorm(40, mean = 2), ncol = 2)
#' )
#' curve <- compute_amd_curve(
#'   X, its = 2, nin = 2, nsp = 3, seeds = 1:4, verbose = FALSE
#' )
#' curve$curve
#' @export
compute_amd_curve <- function(data,
                              its,
                              nin,
                              nsp,
                              seeds = NULL,
                              verbose = TRUE,
                              plot_curve = FALSE,
                              open_device = TRUE,
                              scale_data = FALSE,
                              iter_max = 100,
                              m = 2,
                              preselect_top_sd = NULL) {
  if (!is.matrix(data) && !is.data.frame(data)) {
    stop("data must be a numeric matrix or data frame.", call. = FALSE)
  }

  X <- as.matrix(data)
  if (!is.numeric(X)) {
    stop("data must contain only numeric variables.", call. = FALSE)
  }
  input_column_count <- ncol(X)
  input_column_names <- colnames(X)
  retained_columns <- seq_len(input_column_count)
  complete_mask <- stats::complete.cases(X)
  complete_rows <- which(complete_mask)
  omitted_rows <- which(!complete_mask)
  X <- X[complete_rows, , drop = FALSE]
  storage.mode(X) <- "double"

  as_positive_integer <- function(value, name) {
    if (length(value) != 1L || !is.finite(value) || value < 1 ||
        value != floor(value) || value > .Machine$integer.max) {
      stop(name, " must be one positive integer.", call. = FALSE)
    }
    as.integer(value)
  }

  as_logical_flag <- function(value, name) {
    if (length(value) != 1L || is.na(value) || !is.logical(value)) {
      stop(name, " must be TRUE or FALSE.", call. = FALSE)
    }
    value
  }

  its <- as_positive_integer(its, "its")
  nin <- as_positive_integer(nin, "nin")
  nsp <- as_positive_integer(nsp, "nsp")
  iter_max <- as_positive_integer(iter_max, "iter_max")
  verbose <- as_logical_flag(verbose, "verbose")
  plot_curve <- as_logical_flag(plot_curve, "plot_curve")
  open_device <- as_logical_flag(open_device, "open_device")
  scale_data <- as_logical_flag(scale_data, "scale_data")

  if (nin < 2L) {
    stop("AMD curves compare alternatives and must begin at c >= 2.",
         call. = FALSE)
  }
  if (nin > nsp) {
    stop("nin cannot be greater than nsp.", call. = FALSE)
  }
  if (nrow(X) < 3L || ncol(X) < 1L || nsp >= nrow(X)) {
    stop(
      "data must contain enough complete rows for every requested c.",
      call. = FALSE
    )
  }
  if (any(!is.finite(X))) {
    stop("data contains non-finite values.", call. = FALSE)
  }
  if (length(m) != 1L || !is.finite(m) || m <= 1) {
    stop("m must be one finite number greater than one.", call. = FALSE)
  }

  if (length(omitted_rows) > 0L) {
    warning(
      length(omitted_rows),
      " row(s) with missing values were omitted before fitting.",
      call. = FALSE
    )
  }

  if (!is.null(preselect_top_sd)) {
    preselect_top_sd <- as_positive_integer(preselect_top_sd,
                                             "preselect_top_sd")
    if (ncol(X) > preselect_top_sd) {
      sds <- apply(X, 2L, stats::sd)
      keep <- order(sds, decreasing = TRUE)[seq_len(preselect_top_sd)]
      retained_columns <- retained_columns[keep]
      X <- X[, keep, drop = FALSE]
    }
  }

  retained_column_names <- if (is.null(input_column_names)) {
    NULL
  } else {
    input_column_names[retained_columns]
  }
  scale_center <- NULL
  scale_scale <- NULL
  if (scale_data) {
    sds <- apply(X, 2L, stats::sd)
    if (any(!is.finite(sds) | sds == 0)) {
      stop("cannot scale data with zero or non-finite variable variance.",
           call. = FALSE)
    }
    scaled_X <- scale(X)
    scale_center <- attr(scaled_X, "scaled:center")
    scale_scale <- attr(scaled_X, "scaled:scale")
    X <- scaled_X
  }

  manifest_id <- amd_reproducibility_manifest()$manifest_id
  ks <- seq.int(nin, nsp)
  n_k <- length(ks)
  seed_input_format <- if (is.null(seeds)) {
    "generated_vector"
  } else if (is.matrix(seeds)) {
    "named_matrix"
  } else {
    "vector"
  }
  seed_matrix <- .prepare_amd_curve_seeds(seeds, its, ks)
  .warn_duplicate_start_seeds(seed_matrix)

  dim_names <- list(
    paste0("start_", seq_len(its)),
    as.character(ks)
  )
  raw_scores <- matrix(NA_real_, nrow = its, ncol = n_k,
                       dimnames = dim_names)
  objectives <- matrix(NA_real_, nrow = its, ncol = n_k,
                       dimnames = dim_names)
  iterations <- matrix(NA_integer_, nrow = its, ncol = n_k,
                       dimnames = dim_names)
  converged <- matrix(FALSE, nrow = its, ncol = n_k,
                      dimnames = dim_names)

  best_objectives <- rep(Inf, n_k)
  best_starts <- rep(NA_integer_, n_k)
  best_seeds <- rep(NA_integer_, n_k)
  best_fits <- vector("list", n_k)

  for (start in seq_len(its)) {
    if (verbose) cat("Start", start, "of", its, "\n")

    for (j in seq_along(ks)) {
      c_value <- ks[[j]]
      run_seed <- seed_matrix[start, j]

      fit <- .with_preserved_seed(
        run_seed,
        e1071::cmeans(
          X,
          centers = c_value,
          iter.max = iter_max,
          verbose = FALSE,
          m = m
        )
      )

      score <- amd_score(fit$membership)
      objective <- as.numeric(fit$withinerror)
      did_converge <- is.finite(objective) && fit$iter < iter_max

      raw_scores[start, j] <- score
      objectives[start, j] <- objective
      iterations[start, j] <- as.integer(fit$iter)
      converged[start, j] <- did_converge

      if (did_converge && objective < best_objectives[[j]]) {
        best_objectives[[j]] <- objective
        best_starts[[j]] <- start
        best_seeds[[j]] <- run_seed
        best_fits[[j]] <- fit
      }
    }
  }

  unavailable <- vapply(best_fits, is.null, logical(1))
  if (any(unavailable)) {
    stop(
      "no converged fit was found for c = ",
      paste(ks[unavailable], collapse = ", "),
      "; increase iter_max or its.",
      call. = FALSE
    )
  }

  nonconverged_count <- sum(!converged)
  if (nonconverged_count > 0L) {
    warning(
      nonconverged_count,
      " fit(s) reached iter_max and were excluded from fit selection.",
      call. = FALSE
    )
  }

  selected_curve <- vapply(
    best_fits,
    function(fit) amd_score(fit$membership),
    numeric(1)
  )
  names(selected_curve) <- as.character(ks)
  names(best_objectives) <- as.character(ks)
  names(best_starts) <- as.character(ks)
  names(best_seeds) <- as.character(ks)
  names(best_fits) <- as.character(ks)

  amd_max <- apply(raw_scores, 2L, max)
  amd_mean <- colMeans(raw_scores)
  c_peak <- as.integer(ks[[which.max(selected_curve)]])
  peak_index <- match(c_peak, ks)
  peak_at_boundary <- c_peak %in% range(ks)

  selected_fits <- lapply(best_fits, function(fit) {
    list(
      cluster = fit$cluster,
      membership = fit$membership,
      centers = fit$centers,
      objective = as.numeric(fit$withinerror),
      iterations = as.integer(fit$iter)
    )
  })

  if (plot_curve) {
    if (open_device) grDevices::dev.new()
    graphics::plot(
      ks,
      selected_curve,
      type = "b",
      pch = 16,
      xlab = "Number of configurations (c)",
      ylab = "AMD"
    )
    graphics::abline(v = c_peak, col = "blue", lty = 3)
  }

  out <- list(
    schema_version = 2L,
    c_opt = c_peak,
    c_peak = c_peak,
    peak_at_boundary = peak_at_boundary,
    curve = selected_curve,
    max = amd_max,
    mean = amd_mean,
    raw = raw_scores,
    objectives = objectives,
    iterations = iterations,
    converged = converged,
    coordinates = selected_fits[[peak_index]]$membership,
    selected_starts = best_starts,
    selected_seeds = best_seeds,
    selected_objectives = best_objectives,
    selected_fits = selected_fits,
    selected_fit = selected_fits[[peak_index]],
    complete_rows = complete_rows,
    omitted_rows = omitted_rows,
    manifest_id = manifest_id,
    seeds = seed_matrix,
    backend = "fuzzy_c_means",
    selection_rule = "minimum_objective_among_converged_starts",
    settings = list(
      candidate_c = ks,
      starts = its,
      iter_max = iter_max,
      m = m,
      scaled = scale_data,
      preselect_top_sd = preselect_top_sd,
      input_column_count = input_column_count,
      input_column_names = input_column_names,
      retained_columns = retained_columns,
      retained_column_names = retained_column_names,
      scale_center = scale_center,
      scale_scale = scale_scale,
      seed_input_format = seed_input_format
    )
  )

  class(out) <- "amd_curve"
  out
}
