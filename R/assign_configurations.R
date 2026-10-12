#' Assign observations to a user-selected number of configurations
#'
#' The official workflow supplies an `amd_curve` object. The function then
#' extracts the exact fit that produced the AMD value at the explicitly
#' requested `c`; no new clustering is performed.
#'
#' A provisional compatibility mode accepts a numeric matrix or data frame and
#' independently estimates memberships with repeated fuzzy c-means starts. It
#' is not the package's recommended scientific workflow and may be deprecated
#' or removed in a future version.
#'
#' The function never chooses `c` automatically. In particular, passing an AMD
#' curve still requires an explicit `c`, so the user can incorporate the shape
#' of the curve rather than being forced to accept its numerical maximum.
#'
#' Future automation may assist with interpreting the curve and proposing `c`,
#' but it must be validated against known truth and must not silently replace
#' inspection of the complete curve.
#'
#' AMD is defined independently of fuzzy c-means. Fuzzy c-means is only the
#' membership-estimation backend used by the current implementation of
#' [compute_amd_curve()] and by the provisional compatibility mode.
#'
#' @param data Preferably an object returned by [compute_amd_curve()]. A numeric
#'   matrix or data frame with observations in rows and variables in columns is
#'   accepted only through the provisional compatibility mode.
#' @param c One integer greater than or equal to 2. The number of
#'   configurations to extract or fit. It must have been evaluated when `data`
#'   is an AMD curve.
#' @param its Positive integer number of random starts in independent fitting
#'   mode (default: 50). Ignored when extracting from an AMD curve.
#' @param iter_max Positive integer maximum number of fuzzy c-means iterations
#'   in independent fitting mode (default: 100). Ignored when extracting from
#'   an AMD curve.
#' @param m Fuzziness parameter greater than one in independent fitting mode
#'   (default: 2). Ignored when extracting from an AMD curve.
#' @param scale_data Logical; standardize variables before independent fitting.
#'   Ignored when extracting from an AMD curve.
#' @param verbose Logical; print progress information in independent fitting
#'   mode. Ignored when extracting from an AMD curve.
#' @param seeds Optional vector of positive integer seeds, one per random start
#'   in independent fitting mode. Supplied seeds make the result reproducible
#'   and do not alter the caller's random-number state. Ignored when extracting
#'   from an AMD curve. If `NULL`, seeds are sampled from the caller's active
#'   random-number generator and advance its state. Unused trailing seeds are
#'   ignored. Duplicate seeds reduce the number of unique starts and generate a
#'   warning.
#'
#' @return A list of class `"amd_assignment"`. `schema_version` identifies the
#'   public object layout. The object contains the selected `c`
#'   (also in the compatibility alias `c_opt`), hard assignments, memberships,
#'   centers, AMD and objective values, selection metadata, and a `starts` data
#'   frame with the diagnostics for every random start. `source` distinguishes
#'   exact extraction from an AMD curve from an independent fit. Components
#'   `backend` and `selection_rule` identify how memberships were estimated and
#'   how the retained fit was selected.
#'   `complete_rows` and `omitted_rows` map fitted observations back to the
#'   input rows when incomplete rows were removed. `settings` records the
#'   fitting and preprocessing contract, while `manifest_id` links the object
#'   to [amd_reproducibility_manifest()]. In curve-extraction mode, `settings`
#'   retains the complete candidate range used to create the curve; in
#'   independent fitting mode it records the single fitted `c`. If converged
#'   starts have exactly tied minimum objectives, the first start is selected.
#'   The selected fit is always converged under the stated iteration rule.
#'
#' @examples
#' \donttest{
#' set.seed(1)
#' X <- rbind(
#'   matrix(rnorm(100, mean = -2), ncol = 2),
#'   matrix(rnorm(100, mean = 2), ncol = 2)
#' )
#' curve <- compute_amd_curve(
#'   X, its = 5, nin = 2, nsp = 4, seeds = 1:15, verbose = FALSE
#' )
#'
#' # Exact fit underlying the selected point of the inspected curve
#' extracted <- assign_configurations(curve, c = 2)
#'
#' }
#'
#' @export
assign_configurations <- function(data,
                                  c,
                                  its = 50,
                                  iter_max = 100,
                                  m = 2,
                                  scale_data = FALSE,
                                  verbose = TRUE,
                                  seeds = NULL) {
  as_positive_integer <- function(value, name) {
    if (length(value) != 1L || !is.finite(value) || value < 1 ||
        value != floor(value) || value > .Machine$integer.max) {
      stop(name, " must be one positive integer.", call. = FALSE)
    }
    as.integer(value)
  }

  c <- as_positive_integer(c, "c")
  if (c < 2L) {
    stop("c must be at least 2.", call. = FALSE)
  }

  if (inherits(data, "amd_curve")) {
    schema <- .amd_curve_schema(data)
    if (schema == "invalid") {
      stop(
        "the AMD curve has an incomplete or unrecognised schema; recompute it with the current package version.",
        call. = FALSE
      )
    }
    if (is.null(data$selected_fits)) {
      stop(
        "the AMD curve does not contain selected_fits; recompute it with the current package version.",
        call. = FALSE
      )
    }

    c_key <- as.character(c)
    available_c <- names(data$selected_fits)
    if (is.null(available_c) || !c_key %in% available_c) {
      stop(
        "c = ", c, " was not evaluated in the supplied AMD curve.",
        call. = FALSE
      )
    }

    fit <- data$selected_fits[[c_key]]
    selected_start <- unname(data$selected_starts[[c_key]])
    selected_seed <- unname(data$selected_seeds[[c_key]])

    starts <- data.frame(
      start = seq_len(nrow(data$objectives)),
      seed = as.integer(data$seeds[, c_key]),
      objective = as.numeric(data$objectives[, c_key]),
      amd = as.numeric(data$raw[, c_key]),
      iterations = as.integer(data$iterations[, c_key]),
      converged = as.logical(data$converged[, c_key])
    )

    out <- list(
      schema_version = 2L,
      c = c,
      c_opt = c,
      cluster = fit$cluster,
      membership = fit$membership,
      centers = fit$centers,
      objective = as.numeric(fit$objective),
      amd = amd_score(fit$membership),
      iterations = as.integer(fit$iterations),
      converged = TRUE,
      selected_start = as.integer(selected_start),
      selected_seed = as.integer(selected_seed),
      starts = starts,
      source = "amd_curve",
      backend = data$backend,
      selection_rule = data$selection_rule,
      complete_rows = data$complete_rows,
      omitted_rows = data$omitted_rows,
      manifest_id = data$manifest_id,
      settings = data$settings
    )

    class(out) <- "amd_assignment"
    return(out)
  }

  if (!is.matrix(data) && !is.data.frame(data)) {
    stop("data must be an amd_curve object or a numeric matrix or data frame.",
         call. = FALSE)
  }

  X <- as.matrix(data)
  if (!is.numeric(X)) {
    stop("data must contain only numeric variables.", call. = FALSE)
  }

  input_column_count <- ncol(X)
  input_column_names <- colnames(X)

  complete_rows <- which(stats::complete.cases(X))
  omitted_rows <- which(!stats::complete.cases(X))
  X <- X[complete_rows, , drop = FALSE]
  storage.mode(X) <- "double"

  its <- as_positive_integer(its, "its")
  iter_max <- as_positive_integer(iter_max, "iter_max")

  if (nrow(X) < 3L || ncol(X) < 1L || c >= nrow(X)) {
    stop("data must contain enough complete rows for the requested c.",
         call. = FALSE)
  }
  if (any(!is.finite(X))) {
    stop("data contains non-finite values.", call. = FALSE)
  }
  if (length(m) != 1L || !is.finite(m) || m <= 1) {
    stop("m must be one finite number greater than one.", call. = FALSE)
  }
  if (length(scale_data) != 1L || is.na(scale_data) || !is.logical(scale_data)) {
    stop("scale_data must be TRUE or FALSE.", call. = FALSE)
  }
  if (length(verbose) != 1L || is.na(verbose) || !is.logical(verbose)) {
    stop("verbose must be TRUE or FALSE.", call. = FALSE)
  }

  if (length(omitted_rows) > 0L) {
    warning(
      length(omitted_rows),
      " row(s) with missing values were omitted before fitting.",
      call. = FALSE
    )
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

  seed_input_format <- if (is.null(seeds)) "generated_vector" else "vector"
  if (is.null(seeds)) {
    seeds <- sample.int(.Machine$integer.max, its)
  } else {
    seeds <- .validate_amd_seeds(seeds, its)
  }
  .warn_duplicate_start_seeds(matrix(
    seeds,
    ncol = 1L,
    dimnames = list(paste0("start_", seq_len(its)), as.character(c))
  ))

  manifest_id <- amd_reproducibility_manifest()$manifest_id

  if (verbose) {
    cat("Assigning configurations with c =", c, "\n")
    cat("Random starts:", its, "\n")
  }

  objectives <- rep(NA_real_, its)
  amd_values <- rep(NA_real_, its)
  iterations <- rep(NA_integer_, its)
  converged <- rep(FALSE, its)
  fits <- vector("list", its)

  for (i in seq_len(its)) {
    if (verbose) cat("Start", i, "of", its, "\n")

    fit <- .with_preserved_seed(
      seeds[[i]],
      e1071::cmeans(
        X,
        centers = c,
        iter.max = iter_max,
        verbose = FALSE,
        m = m
      )
    )

    objective <- as.numeric(fit$withinerror)
    did_converge <- is.finite(objective) && fit$iter < iter_max

    fits[[i]] <- fit
    objectives[[i]] <- objective
    amd_values[[i]] <- amd_score(fit$membership)
    iterations[[i]] <- as.integer(fit$iter)
    converged[[i]] <- did_converge
  }

  if (!any(converged)) {
    stop(
      "no converged fit was found; increase iter_max or its.",
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

  eligible_objectives <- objectives
  eligible_objectives[!converged] <- Inf
  selected_start <- which.min(eligible_objectives)
  selected_fit <- fits[[selected_start]]

  starts <- data.frame(
    start = seq_len(its),
    seed = as.integer(seeds),
    objective = objectives,
    amd = amd_values,
    iterations = iterations,
    converged = converged
  )

  out <- list(
    schema_version = 2L,
    c = c,
    c_opt = c,
    cluster = selected_fit$cluster,
    membership = selected_fit$membership,
    centers = selected_fit$centers,
    objective = objectives[[selected_start]],
    amd = amd_values[[selected_start]],
    iterations = iterations[[selected_start]],
    converged = TRUE,
    selected_start = as.integer(selected_start),
    selected_seed = as.integer(seeds[[selected_start]]),
    starts = starts,
    source = "standalone",
    backend = "fuzzy_c_means",
    selection_rule = "minimum_objective_among_converged_starts",
    complete_rows = complete_rows,
    omitted_rows = omitted_rows,
    manifest_id = manifest_id,
    settings = list(
      candidate_c = c,
      starts = its,
      iter_max = iter_max,
      m = m,
      scaled = scale_data,
      preselect_top_sd = NULL,
      input_column_count = input_column_count,
      input_column_names = input_column_names,
      retained_columns = seq_len(input_column_count),
      retained_column_names = input_column_names,
      scale_center = scale_center,
      scale_scale = scale_scale,
      seed_input_format = seed_input_format
    )
  )

  class(out) <- "amd_assignment"
  out
}
