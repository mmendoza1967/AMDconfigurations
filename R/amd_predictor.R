# Optional model-assisted reader; corrected, versioned feature contract ------

.amd_predictor_required_fields <- c(
  "schema", "feature_contract", "amd_package_version", "ranger_version", "created_utc",
  "rf_seeds", "models", "raw_predictors", "processed_predictors",
  "preprocessing", "calibrators", "contract", "provenance", "validation"
)

.amd_predictor_distance_limit <- 2000L

.amd_validate_predictor_bundle <- function(bundle) {
  if (!is.list(bundle)) {
    stop("predictor bundle must be a list.", call. = FALSE)
  }
  missing <- setdiff(.amd_predictor_required_fields, names(bundle))
  if (length(missing)) {
    stop(
      "predictor bundle lacks required fields: ",
      paste(missing, collapse = ", "), ".",
      call. = FALSE
    )
  }
  if (!identical(bundle$schema, "amd_predictor_bundle_v2") ||
      !identical(bundle$feature_contract, "amd_modal_features_left_pair_v2")) {
    stop("unsupported predictor contract: this interface requires the corrected v2 bundle; historical v1 bundles remain separate.",
         call. = FALSE)
  }
  if (!is.character(bundle$created_utc) || length(bundle$created_utc) != 1L ||
      is.na(bundle$created_utc) || !nzchar(bundle$created_utc) ||
      !identical(bundle$contract$resolved_target, "numerically_resolved_density_mode_count")) {
    stop("predictor bundle must declare its creation time and numerically resolved density-mode target.",
         call. = FALSE)
  }
  if (!identical(bundle$contract$left_pair_missing_margin, 0) ||
      !identical(bundle$validation$confidence_transfer_assessment,
                 "retrospective_final_bank_20261012") ||
      !isTRUE(bundle$validation$confidence_transfer_reference_gates_passed)) {
    stop("corrected bundle lacks its boundary convention or completed confidence-transfer assessment.",
         call. = FALSE)
  }
  if (!identical(as.integer(bundle$contract$candidates), 1:14) ||
      !identical(as.integer(bundle$contract$numeric_candidates), 2:14)) {
    stop("predictor bundle has an unsupported candidate range.", call. = FALSE)
  }
  if (!identical(as.integer(bundle$contract$n_validation_grid), c(150L, 600L)) ||
      !identical(as.integer(bundle$contract$d_validation_grid), c(2L, 6L, 15L)) ||
      !identical(as.integer(bundle$contract$observed_validation_output_support), 1:8)) {
    stop("corrected bundle does not declare the assessed numerical and output support.",
         call. = FALSE)
  }
  for (field in c("n_validation_range", "d_validation_range")) {
    values <- as.integer(bundle$contract[[field]])
    if (length(values) != 2L || any(!is.finite(values)) ||
        values[[1L]] > values[[2L]]) {
      stop("predictor bundle has an invalid validation range.", call. = FALSE)
    }
  }
  grids <- list(
    n = bundle$contract$n_validation_grid,
    d = bundle$contract$d_validation_grid
  )
  n_grid <- sort(unique(as.integer(grids$n)))
  d_grid <- sort(unique(as.integer(grids$d)))
  n_range <- as.integer(bundle$contract$n_validation_range)
  d_range <- as.integer(bundle$contract$d_validation_range)
  if (any(n_grid < n_range[[1L]]) || any(n_grid > n_range[[2L]]) ||
      any(d_grid < d_range[[1L]]) || any(d_grid > d_range[[2L]])) {
    stop("predictor bundle has an invalid numerical validation grid.", call. = FALSE)
  }
  if (!identical(as.integer(bundle$contract$curve_starts), 20L) ||
      !identical(as.integer(bundle$contract$stability_starts), 5L) ||
      !identical(as.integer(bundle$contract$iter_max), 2000L) ||
      !isTRUE(all.equal(as.numeric(bundle$contract$m), 2)) ||
      !identical(bundle$contract$scale_data, FALSE) ||
      !is.null(bundle$contract$preselect_top_sd) ||
      !identical(
        bundle$contract$selected_fit_rule,
        "minimum_objective_among_converged_starts"
      )) {
    stop("predictor bundle has an unsupported AMD fitting contract.",
         call. = FALSE)
  }
  if (!identical(as.integer(bundle$rf_seeds), c(101L, 202L, 303L, 404L, 505L)) ||
      length(bundle$models) != length(bundle$rf_seeds) ||
      anyDuplicated(names(bundle$models)) ||
      !all(as.character(bundle$rf_seeds) %in% names(bundle$models))) {
    stop("predictor bundle has inconsistent RF models and seeds.",
         call. = FALSE)
  }
  if (!all(c("structure", "exact") %in% names(bundle$calibrators))) {
    stop("predictor bundle lacks one or both confidence calibrators.",
         call. = FALSE)
  }
  preprocessing <- bundle$preprocessing
  needed_preprocessing <- c(
    "variable", "replacement", "training_missing", "value_retained"
  )
  if (!is.data.frame(preprocessing) ||
      !all(needed_preprocessing %in% names(preprocessing)) ||
      !identical(as.character(preprocessing$variable),
                 as.character(bundle$raw_predictors))) {
    stop("predictor bundle has an invalid preprocessing contract.",
         call. = FALSE)
  }
  if (anyDuplicated(bundle$raw_predictors) || anyDuplicated(bundle$processed_predictors) ||
      !is.numeric(preprocessing$replacement) || any(!is.finite(preprocessing$replacement)) ||
      !is.logical(preprocessing$training_missing) || anyNA(preprocessing$training_missing) ||
      !is.logical(preprocessing$value_retained) || anyNA(preprocessing$value_retained)) {
    stop("predictor preprocessing has duplicate names, invalid replacements or missing flags.",
         call. = FALSE)
  }
  if (!all(vapply(bundle$calibrators[c("structure", "exact")], function(model) {
    inherits(model, "glm") && identical(model$family$family, "binomial") &&
      all(is.finite(stats::coef(model)))
  }, logical(1)))) {
    stop("confidence calibrators must be finite binomial GLMs.", call. = FALSE)
  }
  expected_terms <- list(structure = c("structure_vote_fraction", "structure_margin_norm"),
    exact = c("exact_vote_fraction", "consensus_margin_norm", "entropy_clarity"))
  if (!all(vapply(names(expected_terms), function(task) {
    model <- bundle$calibrators[[task]]
    identical(model$family$link, "logit") &&
      identical(attr(stats::terms(model), "term.labels"), expected_terms[[task]])
  }, logical(1)))) {
    stop("confidence calibrator formulas do not match the frozen descriptor contract.",
         call. = FALSE)
  }
  forest_predictors <- lapply(bundle$models, function(model) {
    model$forest$independent.variable.names
  })
  if (any(vapply(forest_predictors, is.null, logical(1L))) ||
      !all(vapply(forest_predictors, identical, logical(1L),
                  bundle$processed_predictors))) {
    stop("predictor bundle RFs do not match the predictor contract.",
         call. = FALSE)
  }
  invisible(TRUE)
}

#' Load the optional corrected AMD reader
#'
#' Loads the external model bundle used by [predict_amd()]. The large fitted
#' random forests are deliberately not embedded in the base package. Loading
#' verifies a caller-supplied SHA-256, validates the model and preprocessing
#' contracts, and never downloads or refits a model. Historical v1 bundles
#' cannot be paired with the corrected extractor in this interface.
#'
#' @param path Path to a corrected `amd_predictor_bundle_v2` RDS file.
#' @param sha256 Expected 64-character SHA-256 from the matching bundle manifest.
#'   The optional package `digest` is required for this integrity check.
#'
#' @return An object of class `"amd_predictor"`.
#' @export
load_amd_predictor <- function(path, sha256) {
  if (!is.character(path) || length(path) != 1L || is.na(path) || !nzchar(path) ||
      !file.exists(path)) {
    stop("path must identify an existing predictor bundle.", call. = FALSE)
  }
  if (missing(sha256) || !is.character(sha256) || length(sha256) != 1L ||
      is.na(sha256) || !grepl("^[[:xdigit:]]{64}$", sha256)) {
    stop("sha256 must be the expected 64-character bundle checksum.", call. = FALSE)
  }
  if (!requireNamespace("digest", quietly = TRUE)) {
    stop("load_amd_predictor() requires the suggested package 'digest'.",
         call. = FALSE)
  }
  actual_sha256 <- digest::digest(file = path, algo = "sha256")
  if (!identical(tolower(sha256), actual_sha256)) {
    stop("predictor bundle SHA-256 mismatch; the bundle was not loaded.",
         call. = FALSE)
  }
  bundle <- readRDS(path)
  .amd_validate_predictor_bundle(bundle)
  attr(bundle, "source_path") <- normalizePath(
    path, winslash = "/", mustWork = TRUE
  )
  attr(bundle, "sha256") <- actual_sha256
  class(bundle) <- c("amd_predictor", "list")
  bundle
}

.amd_with_seed <- function(seed, expression) {
  had_seed <- exists(".Random.seed", envir = .GlobalEnv, inherits = FALSE)
  if (had_seed) old_seed <- get(".Random.seed", envir = .GlobalEnv)
  on.exit({
    if (had_seed) {
      assign(".Random.seed", old_seed, envir = .GlobalEnv)
    } else if (exists(".Random.seed", envir = .GlobalEnv, inherits = FALSE)) {
      rm(".Random.seed", envir = .GlobalEnv)
    }
  }, add = TRUE)
  set.seed(as.integer(seed))
  force(expression)
}

.amd_predictor_seed_plan <- function(seed, numeric_candidates,
                                     curve_starts, stability_starts) {
  if (length(seed) != 1L || !is.finite(seed) || seed < 1 ||
      seed != floor(seed) || seed > .Machine$integer.max) {
    stop("seed must be one positive integer.", call. = FALSE)
  }
  count_curve <- length(numeric_candidates) * curve_starts
  count_stability <- length(numeric_candidates) * stability_starts
  values <- .amd_with_seed(seed, sample.int(
    .Machine$integer.max,
    count_curve + count_stability + 1L,
    replace = FALSE
  ))
  curve_values <- values[seq_len(count_curve)]
  stability_values <- values[count_curve + seq_len(count_stability)]
  list(
    curve = matrix(
      curve_values, nrow = curve_starts,
      dimnames = list(
        paste0("start_", seq_len(curve_starts)),
        as.character(numeric_candidates)
      )
    ),
    stability = matrix(
      stability_values, nrow = stability_starts,
      dimnames = list(
        paste0("start_", seq_len(stability_starts)),
        as.character(numeric_candidates)
      )
    ),
    data = values[[length(values)]]
  )
}

.amd_predictor_data <- function(data) {
  if (!is.matrix(data) && !is.data.frame(data)) {
    stop("data must be a numeric matrix or data frame.", call. = FALSE)
  }
  X <- as.matrix(data)
  if (!is.numeric(X)) {
    stop("data must contain only numeric variables.", call. = FALSE)
  }
  complete <- stats::complete.cases(X)
  X <- X[complete, , drop = FALSE]
  storage.mode(X) <- "double"
  if (nrow(X) < 15L || ncol(X) < 1L || any(!is.finite(X))) {
    stop(
      "data must contain at least 15 complete rows and one finite numeric variable.",
      call. = FALSE
    )
  }
  list(
    data = X,
    complete_rows = which(complete),
    omitted_rows = which(!complete),
    input_rows = nrow(as.matrix(data)),
    input_columns = ncol(as.matrix(data))
  )
}

.amd_validate_predictor_curve <- function(curve, prepared, contract) {
  if (.amd_curve_schema(curve) != "current") {
    stop("curve must be a current amd_curve object.", call. = FALSE)
  }
  expected_c <- as.integer(contract$numeric_candidates)
  if (!identical(as.integer(names(curve$curve)), expected_c) ||
      !identical(as.integer(curve$settings$candidate_c), expected_c) ||
      !identical(as.integer(curve$settings$starts),
                 as.integer(contract$curve_starts)) ||
      !identical(as.integer(curve$settings$iter_max),
                 as.integer(contract$iter_max)) ||
      !isTRUE(all.equal(as.numeric(curve$settings$m),
                        as.numeric(contract$m))) ||
      !identical(curve$settings$scaled, FALSE) ||
      !is.null(curve$settings$preselect_top_sd) ||
      !identical(curve$selection_rule, contract$selected_fit_rule)) {
    stop(
      "curve was not computed with the predictor's frozen AMD contract.",
      call. = FALSE
    )
  }
  if (!identical(as.integer(curve$complete_rows),
                 as.integer(prepared$complete_rows)) ||
      !identical(as.integer(curve$omitted_rows),
                 as.integer(prepared$omitted_rows)) ||
      !identical(as.integer(curve$settings$input_column_count),
                 as.integer(prepared$input_columns))) {
    stop("curve does not correspond to the supplied data dimensions or rows.",
         call. = FALSE)
  }
  if (is.null(curve$selected_fits) ||
      !identical(names(curve$selected_fits), as.character(expected_c))) {
    stop("curve lacks the fitted partitions required by the predictor.",
         call. = FALSE)
  }
  expected_dimensions <- c(as.integer(contract$curve_starts),
                           length(expected_c))
  matrix_fields <- c("raw", "objectives", "converged")
  valid_matrices <- vapply(matrix_fields, function(field) {
    value <- curve[[field]]
    is.matrix(value) && identical(dim(value), expected_dimensions) &&
      identical(colnames(value), as.character(expected_c))
  }, logical(1L))
  vector_fields <- c("selected_starts", "selected_objectives")
  valid_vectors <- vapply(vector_fields, function(field) {
    value <- curve[[field]]
    length(value) == length(expected_c) &&
      identical(names(value), as.character(expected_c))
  }, logical(1L))
  if (!all(valid_matrices) || !all(valid_vectors) ||
      any(!is.finite(curve$raw[curve$converged])) ||
      any(!is.finite(curve$objectives[curve$converged]))) {
    stop("curve diagnostics are incomplete or inconsistent.", call. = FALSE)
  }
  X <- prepared$data
  for (candidate in expected_c) {
    fit <- curve$selected_fits[[as.character(candidate)]]
    membership <- as.matrix(fit$membership)
    centers <- as.matrix(fit$centers)
    if (!identical(dim(membership), c(nrow(X), candidate)) ||
        !identical(dim(centers), c(candidate, ncol(X)))) {
      stop("curve fitted partitions do not match the supplied data.",
           call. = FALSE)
    }
    .amd_diagnostic_validate_fit(membership, fit$cluster, candidate, nrow(X))
    if (any(!is.finite(centers))) {
      stop("curve centers must be finite.", call. = FALSE)
    }
    distances <- vapply(seq_len(candidate), function(group) {
      rowSums(sweep(X, 2L, centers[group, ], FUN = "-")^2)
    }, numeric(nrow(X)))
    # e1071::cmeans() reports the fuzzy within-objective per observation.
    objective <- sum((membership ^ contract$m) * distances) / nrow(X)
    tolerance <- 1e-7 * max(1, abs(as.numeric(fit$objective)))
    if (!is.finite(objective) ||
        abs(objective - as.numeric(fit$objective)) > tolerance) {
      stop("curve fitted partitions do not reproduce on the supplied data.",
           call. = FALSE)
    }
    selected_start <- as.integer(
      curve$selected_starts[[as.character(candidate)]]
    )
    if (length(selected_start) != 1L || !is.finite(selected_start) ||
        selected_start < 1L || selected_start > expected_dimensions[[1L]] ||
        !isTRUE(curve$converged[selected_start, as.character(candidate)])) {
      stop("curve selected-start diagnostics are inconsistent.",
           call. = FALSE)
    }
    selected_objective <- as.numeric(
      curve$objectives[selected_start, as.character(candidate)]
    )
    converged_objectives <- curve$objectives[
      curve$converged[, as.character(candidate)], as.character(candidate)
    ]
    selected_score <- as.numeric(
      curve$raw[selected_start, as.character(candidate)]
    )
    curve_score <- as.numeric(curve$curve[[as.character(candidate)]])
    if (!isTRUE(all.equal(
      selected_objective,
      as.numeric(curve$selected_objectives[[as.character(candidate)]]),
      tolerance = 1e-12
    )) || !isTRUE(all.equal(
      selected_objective, as.numeric(fit$objective), tolerance = 1e-12
    )) || abs(selected_objective - min(converged_objectives)) >
      1e-10 * max(1, abs(selected_objective)) ||
      !isTRUE(all.equal(selected_score, curve_score, tolerance = 1e-12)) ||
      !isTRUE(all.equal(
        amd_score(membership), curve_score, tolerance = 1e-12
      ))) {
      stop("curve selection, objectives and AMD values are inconsistent.",
           call. = FALSE)
    }
  }
  invisible(TRUE)
}

.amd_predictor_candidate_features <- function(X, curve, seed_plan, bundle) {
  candidates <- as.integer(bundle$contract$candidates)
  numeric_candidates <- as.integer(bundle$contract$numeric_candidates)
  assignments <- vector("list", length(candidates))
  names(assignments) <- as.character(candidates)
  assignments[["1"]] <- analytical_single_partition(X)
  for (candidate in numeric_candidates) {
    assignments[[as.character(candidate)]] <- assign_configurations(
      curve, c = candidate
    )
  }

  y_values <- as.numeric(curve$curve[as.character(numeric_candidates)])
  global_features <- candidate_global_features(numeric_candidates, y_values)
  local_features <- do.call(rbind, lapply(
    candidates, candidate_local_features,
    c_values = numeric_candidates, y_values = y_values
  ))
  advanced_features <- advanced_candidate_curve_features(
    candidates, numeric_candidates, y_values,
    raw_scores = curve$raw, converged = curve$converged
  )
  partition_features <- do.call(rbind, lapply(assignments, function(x) {
    describe_candidate_partition(X, x, m = bundle$contract$m)
  }))
  transition_features <- add_candidate_transition_features(
    assignments, candidates
  )
  stability_features <- do.call(rbind, lapply(candidates, function(candidate) {
    seeds <- if (candidate == 1L) integer() else {
      seed_plan$stability[, as.character(candidate)]
    }
    describe_partition_refit_stability(
      X,
      assignments[[as.character(candidate)]]$cluster,
      candidate,
      seeds = seeds,
      iter_max = bundle$contract$iter_max,
      m = bundle$contract$m
    )
  }))
  data_features <- describe_unlabelled_data(X, seed = seed_plan$data,
    distance_limit = .amd_predictor_distance_limit)

  rows <- data.frame(
    scenario_id = rep("application", length(candidates)),
    bank = rep("application", length(candidates)),
    candidate_c = candidates,
    N = rep(nrow(X), length(candidates)),
    D = rep(ncol(X), length(candidates)),
    stringsAsFactors = FALSE
  )
  rows <- cbind(
    rows,
    local_features,
    global_features[rep(1L, length(candidates)), , drop = FALSE],
    advanced_features,
    data_features[rep(1L, length(candidates)), , drop = FALSE],
    partition_features,
    transition_features,
    stability_features,
    stringsAsFactors = FALSE
  )
  forbidden <- grep(
    paste0(
      "^(truth__|ari_truth|K$|geometry$|resolution$|",
      "center_separation$|size_profile$|shape_level$)"
    ),
    names(rows), value = TRUE
  )
  if (length(forbidden)) {
    stop("truth or generator variables entered application predictors.",
         call. = FALSE)
  }
  absent <- setdiff(bundle$raw_predictors, names(rows))
  if (length(absent)) {
    stop(
      "application feature contract lacks: ",
      paste(absent, collapse = ", "), ".",
      call. = FALSE
    )
  }
  list(features = rows, assignments = assignments)
}

.amd_apply_predictor_preprocessing <- function(data, bundle) {
  result <- data.frame(row.names = seq_len(nrow(data)))
  contract <- bundle$preprocessing
  diagnostics <- vector("list", nrow(contract))
  for (row in seq_len(nrow(contract))) {
    variable <- contract$variable[[row]]
    value <- as.numeric(data[[variable]])
    finite <- is.finite(value)
    if (isTRUE(contract$training_missing[[row]])) {
      result[[paste0("missing__", variable)]] <- as.integer(!finite)
    }
    value[!finite] <- contract$replacement[[row]]
    if (isTRUE(contract$value_retained[[row]])) {
      result[[variable]] <- value
    }
    diagnostics[[row]] <- data.frame(
      variable = variable,
      nonfinite_count = sum(!finite),
      training_missing = isTRUE(contract$training_missing[[row]]),
      missing_indicator_included = isTRUE(
        contract$training_missing[[row]]
      ),
      replacement = as.numeric(contract$replacement[[row]]),
      value_retained = isTRUE(contract$value_retained[[row]]),
      unassessed_constant_count = if (!isTRUE(contract$value_retained[[row]])) {
        original <- as.numeric(data[[variable]])
        sum(is.finite(original) & abs(original - contract$replacement[[row]]) >
              1e-12 * max(1, abs(contract$replacement[[row]])))
      } else 0L,
      stringsAsFactors = FALSE
    )
  }
  if (!setequal(names(result), bundle$processed_predictors)) {
    stop("processed application predictors do not match the RF contract.",
         call. = FALSE)
  }
  result <- result[, bundle$processed_predictors, drop = FALSE]
  diagnostics <- do.call(rbind, diagnostics)
  affected <- diagnostics[diagnostics$nonfinite_count > 0L, , drop = FALSE]
  unexpected <- affected[!affected$training_missing, , drop = FALSE]
  changed_constants <- diagnostics[diagnostics$unassessed_constant_count > 0L, , drop = FALSE]
  attr(result, "amd_imputation_diagnostics") <- list(
    total_nonfinite_values = sum(diagnostics$nonfinite_count),
    variables_with_nonfinite = as.character(affected$variable),
    unexpected_nonfinite_values = sum(unexpected$nonfinite_count),
    unexpected_variables = as.character(unexpected$variable),
    unassessed_constant_values = sum(diagnostics$unassessed_constant_count),
    unassessed_constant_predictors = as.character(changed_constants$variable),
    details = affected
  )
  result
}

.amd_mode_smallest <- function(values) {
  frequency <- table(values)
  as.integer(min(as.numeric(names(frequency)[frequency == max(frequency)])))
}

.amd_rank_candidate_scores <- function(candidate, probability) {
  maximum <- max(probability)
  min(candidate[abs(probability - maximum) <= 1e-12])
}

.amd_prediction_descriptors <- function(candidate_scores, seed_predictions) {
  candidate_scores <- candidate_scores[order(candidate_scores$candidate_c), ]
  consensus <- .amd_mode_smallest(seed_predictions$prediction)
  score_range <- diff(range(candidate_scores$mean_score))
  consensus_score <- candidate_scores$mean_score[
    candidate_scores$candidate_c == consensus
  ]
  alternatives <- candidate_scores$candidate_c != consensus
  best_alternative_score <- max(candidate_scores$mean_score[alternatives])
  best_alternative <- min(candidate_scores$candidate_c[
    alternatives & abs(
      candidate_scores$mean_score - best_alternative_score
    ) <= 1e-12
  ])
  consensus_margin <- consensus_score - best_alternative_score
  consensus_margin_norm <- if (score_range > 0) {
    consensus_margin / score_range
  } else 0
  nonnegative <- pmax(candidate_scores$mean_score, 0)
  weights <- if (sum(nonnegative) > 0) {
    nonnegative / sum(nonnegative)
  } else rep(1 / nrow(candidate_scores), nrow(candidate_scores))
  nonzero <- weights > 0
  entropy <- -sum(weights[nonzero] * log(weights[nonzero]))
  entropy_clarity <- 1 - entropy / log(nrow(candidate_scores))
  score_one <- candidate_scores$mean_score[
    candidate_scores$candidate_c == 1L
  ]
  best_structured_score <- max(candidate_scores$mean_score[
    candidate_scores$candidate_c > 1L
  ])
  predicted_structure <- consensus > 1L
  structure_margin <- if (predicted_structure) {
    best_structured_score - score_one
  } else score_one - best_structured_score
  structure_margin_norm <- if (score_range > 0) {
    structure_margin / score_range
  } else 0

  data.frame(
    prediction = consensus,
    predicted_structure = as.integer(predicted_structure),
    exact_vote_fraction = mean(seed_predictions$prediction == consensus),
    structure_vote_fraction = mean(
      (seed_predictions$prediction > 1L) == predicted_structure
    ),
    consensus_score = consensus_score,
    best_alternative = best_alternative,
    consensus_margin = consensus_margin,
    consensus_margin_norm = consensus_margin_norm,
    entropy_clarity = entropy_clarity,
    structure_margin = structure_margin,
    structure_margin_norm = structure_margin_norm,
    mean_score_c1 = score_one,
    best_structured_score = best_structured_score,
    stringsAsFactors = FALSE
  )
}

.amd_predictor_applicability <- function(X, contract) {
  n_range <- as.integer(contract$n_validation_range)
  d_range <- as.integer(contract$d_validation_range)
  n_grid <- sort(unique(as.integer(contract$n_validation_grid)))
  d_grid <- sort(unique(as.integer(contract$d_validation_grid)))
  n_in_range <- nrow(X) >= n_range[[1L]] && nrow(X) <= n_range[[2L]]
  d_in_range <- ncol(X) >= d_range[[1L]] && ncol(X) <= d_range[[2L]]
  n_on_grid <- nrow(X) %in% n_grid
  d_on_grid <- ncol(X) %in% d_grid
  exact_grid <- n_on_grid && d_on_grid
  within_envelope <- n_in_range && d_in_range
  reasons <- character()
  if (!n_in_range) reasons <- c(reasons, sprintf(
    "N=%d is outside the validated range [%d, %d]",
    nrow(X), n_range[[1L]], n_range[[2L]]
  ))
  if (!d_in_range) reasons <- c(reasons, sprintf(
    "D=%d is outside the validated range [%d, %d]",
    ncol(X), d_range[[1L]], d_range[[2L]]
  ))
  if (n_in_range && !n_on_grid) reasons <- c(reasons, sprintf(
    "N=%d lies inside the envelope but is not on the validated grid {%s}",
    nrow(X), paste(n_grid, collapse = ", ")
  ))
  if (d_in_range && !d_on_grid) reasons <- c(reasons, sprintf(
    "D=%d lies inside the envelope but is not on the validated grid {%s}",
    ncol(X), paste(d_grid, collapse = ", ")
  ))
  numerical_status <- if (exact_grid) {
    "validated_grid_point"
  } else if (within_envelope) {
    "unvalidated_interpolation"
  } else {
    "unvalidated_extrapolation"
  }
  list(
    in_validated_numeric_domain = exact_grid,
    on_validated_grid = exact_grid,
    within_validation_envelope = within_envelope,
    numerical_status = numerical_status,
    N = nrow(X),
    D = ncol(X),
    validated_N = n_range,
    validated_D = d_range,
    validated_N_grid = n_grid,
    validated_D_grid = d_grid,
    reasons = reasons,
    geometry_note = paste0(
      "Numerical range is necessary but not sufficient: real-data geometry ",
      "cannot be proven to belong to the six simulated training families."
    )
  )
}

.amd_predictor_cost <- function(X, max_working_bytes) {
  if (!is.numeric(max_working_bytes) || length(max_working_bytes) != 1L ||
      !is.finite(max_working_bytes) || max_working_bytes <= 0) {
    stop("max_working_bytes must be one finite positive number.", call. = FALSE)
  }
  n <- as.double(nrow(X))
  d <- as.double(ncol(X))
  # Risk guard, not a certified upper bound on total R-process memory.
  estimate <- 8 * (6 * n^2 + 6 * d^2 + 5 * n * d + 3 * n * sum(2:14))
  if (!is.finite(estimate) || estimate > max_working_bytes) {
    stop(sprintf(paste0(
      "exact reader diagnostics exceed the working-memory guard (estimate %.1f MiB; limit %.1f MiB). ",
      "No AMD fits or sample distance matrices were computed. Use the manual curve workflow, ",
      "or explicitly raise max_working_bytes after assessing available resources. ",
      "No additional approximation is substituted to bypass this guard; ",
      "the frozen global-distance descriptor cap is 2000 rows."),
      estimate / 1024^2, max_working_bytes / 1024^2), call. = FALSE)
  }
  list(estimated_working_bytes = estimate, limit_bytes = max_working_bytes,
       exact_sample_distances = n <= .amd_predictor_distance_limit,
       exact_partition_distances = TRUE,
       global_distance_limit = .amd_predictor_distance_limit,
       global_distance_sample_n = as.integer(min(n, .amd_predictor_distance_limit)),
       global_distance_subsampled = n > .amd_predictor_distance_limit,
       estimate_is_memory_guarantee = FALSE)
}

#' Propose a modal resolution from an AMD analysis
#'
#' Optional assistance for interpreting a complete AMD curve. It computes
#' the frozen `c = 2,...,14` AMD analysis (unless a compatible curve is
#' supplied), derives curve, data, and candidate-partition descriptors, and
#' applies five externally loaded random forests. Reliability estimates the
#' chance that the emitted one-versus-several-modes decision, or the exact
#' proposed K, is correct within the assessed simulation domain. It is not
#' directly the probability that groups exist. The target is the numerically
#' resolved number of density modes used as the simulation reference, not
#' necessarily the number of generating components. AMD itself does not
#' estimate a density.
#'
#' This function does not replace inspection of the AMD curve, does not claim
#' that its proposal is ground truth, and does not assign observations. When
#' `K > 1` is proposed, use [assign_configurations()] with the returned curve
#' and a scientifically justified `c`.
#'
#' @param data Numeric matrix or data frame, with observations in rows.
#' @param predictor An object returned by [load_amd_predictor()].
#' @param curve Optional compatible `amd_curve`. Supplying it avoids repeating
#'   the principal AMD fits; it is checked against `data` and the frozen model
#'   contract.
#' @param seed Positive integer controlling AMD starts, stability refits, and
#'   any large-data sampling used by descriptors. The caller's random-number
#'   state is preserved.
#' @param max_working_bytes Positive finite working-memory guard for the exact
#'   descriptors (default 512 MiB). The estimate is not a guarantee of total
#'   process memory and excludes the externally loaded forests. The frozen
#'   global-distance summaries use a seeded sample of 2000 rows when N > 2000;
#'   partition distances remain exact. This distinction is recorded in the
#'   resource diagnostics. No additional approximation bypasses the guard.
#' @param allow_extrapolation Logical. Away from the validated numerical grid,
#'   retain calibrated numerical values but label interpolation or
#'   extrapolation as unvalidated instead of suppressing them.
#' @param verbose Logical; report progress while computing a new curve.
#'
#' @return An object of class `"amd_prediction"` containing the AMD curve,
#'   proposed numerically resolved density-mode count `K`, separate structure
#'   and exact-modal-count reliability estimates,
#'   candidate scores, five-seed proposals, diagnostics, and applicability.
#' @details
#' The corrected forests use the documented missing-left-pair convention.
#' Their unchanged historical calibrators were reassessed retrospectively on
#' the previously examined final bank, not on a new prospective validation.
#' Certainties are suppressed by default outside the numerical validation
#' grid N = 150 or 600, D = 2, 6 or 15. Matching this grid does not certify
#' the geometry of new data. The observed validation output support was K=1:8,
#' although the mechanical candidate range is 1:14.
#' Constant predictors dropped during training include the zero-variance
#' fraction (always zero) and the minimum covariance-rank fraction (always one).
#' A departure from those constants suppresses certainty, even on the numeric
#' grid. This safeguard does not certify all other aspects of new-data geometry.
#'
#' Supplying a compatible curve avoids its 260 principal fits. The additional
#' five computational restarts per c required by the reader are still run;
#' these are not a bootstrap or other sample-level stability analysis. Curve
#' checks establish row/dimension and objective consistency with the supplied
#' data, not a cryptographic identity of a separately created data file.
#' @export
predict_amd <- function(data, predictor, curve = NULL, seed = 1L,
                        allow_extrapolation = FALSE, verbose = TRUE,
                        max_working_bytes = 512 * 1024^2) {
  if (!inherits(predictor, "amd_predictor")) {
    stop("predictor must be returned by load_amd_predictor().",
         call. = FALSE)
  }
  .amd_validate_predictor_bundle(predictor)
  if (!requireNamespace("ranger", quietly = TRUE)) {
    stop("predict_amd() requires the suggested package 'ranger'.",
         call. = FALSE)
  }
  flags <- list(allow_extrapolation = allow_extrapolation, verbose = verbose)
  for (name in names(flags)) {
    value <- flags[[name]]
    if (length(value) != 1L || is.na(value) || !is.logical(value)) {
      stop(name, " must be TRUE or FALSE.", call. = FALSE)
    }
  }

  prepared <- .amd_predictor_data(data)
  resource_check <- .amd_predictor_cost(prepared$data, max_working_bytes)
  if (!is.null(curve) && length(prepared$omitted_rows)) {
    warning(
      length(prepared$omitted_rows),
      " row(s) with missing values were omitted before prediction.",
      call. = FALSE
    )
  }
  contract <- predictor$contract
  computed_curve <- is.null(curve)
  seed_plan <- .amd_predictor_seed_plan(
    seed,
    as.integer(contract$numeric_candidates),
    as.integer(contract$curve_starts),
    as.integer(contract$stability_starts)
  )
  if (is.null(curve)) {
    curve <- compute_amd_curve(
      data,
      its = contract$curve_starts,
      nin = min(contract$numeric_candidates),
      nsp = max(contract$numeric_candidates),
      seeds = seed_plan$curve,
      verbose = verbose,
      plot_curve = FALSE,
      scale_data = FALSE,
      iter_max = contract$iter_max,
      m = contract$m,
      preselect_top_sd = NULL
    )
  }
  .amd_validate_predictor_curve(curve, prepared, contract)

  feature_result <- .amd_predictor_candidate_features(
    prepared$data, curve, seed_plan, predictor
  )
  sampled_n <- unique(feature_result$features$data__distance_sample_n)
  if (length(sampled_n) != 1L || !is.finite(sampled_n) ||
      sampled_n != resource_check$global_distance_sample_n) {
    stop("global-distance sample size violates the frozen feature contract.", call. = FALSE)
  }
  resource_check$global_distance_sample_n <- as.integer(sampled_n)
  resource_check$global_distance_subsampled <- sampled_n < nrow(prepared$data)
  resource_check$exact_sample_distances <- !resource_check$global_distance_subsampled
  processed <- .amd_apply_predictor_preprocessing(
    feature_result$features, predictor
  )
  imputation_diagnostics <- attr(processed, "amd_imputation_diagnostics")
  if (imputation_diagnostics$unexpected_nonfinite_values > 0L) {
    warning(
      paste0(
        "application features contained non-finite values for predictor(s) ",
        "that were always finite during training: ",
        paste(imputation_diagnostics$unexpected_variables, collapse = ", "),
        ". Training replacements were used and recorded in diagnostics."
      ),
      call. = FALSE
    )
  }
  if (length(imputation_diagnostics$unassessed_constant_predictors)) {
    warning("application departs from training-constant predictors: ",
      paste(imputation_diagnostics$unassessed_constant_predictors, collapse = ", "),
      "; the proposal is exploratory and certainty is suppressed.", call. = FALSE)
  }

  # ranger also consumes an R seed when predicting probabilities. Keep that
  # implementation detail out of the caller's random-number stream.
  candidate_scores_by_seed <- .amd_with_seed(seed, lapply(predictor$rf_seeds, function(rf_seed) {
    model <- predictor$models[[as.character(rf_seed)]]
    probability <- stats::predict(
      model, data = processed, num.threads = 1L
    )$predictions[, "yes"]
    data.frame(
      rf_seed = as.integer(rf_seed),
      candidate_c = as.integer(contract$candidates),
      score = as.numeric(probability),
      stringsAsFactors = FALSE
    )
  }))
  score_long <- do.call(rbind, candidate_scores_by_seed)
  rownames(score_long) <- NULL
  seed_predictions <- do.call(rbind, lapply(
    split(score_long, score_long$rf_seed),
    function(rows) data.frame(
      rf_seed = rows$rf_seed[[1L]],
      prediction = .amd_rank_candidate_scores(
        rows$candidate_c, rows$score
      ),
      stringsAsFactors = FALSE
    )
  ))
  rownames(seed_predictions) <- NULL
  candidate_scores <- stats::aggregate(
    score ~ candidate_c, data = score_long, FUN = mean
  )
  names(candidate_scores)[[2L]] <- "mean_score"
  descriptors <- .amd_prediction_descriptors(
    candidate_scores, seed_predictions
  )
  calibrated <- c(
    structure = as.numeric(stats::predict(
      predictor$calibrators$structure,
      newdata = descriptors, type = "response"
    )),
    exact = as.numeric(stats::predict(
      predictor$calibrators$exact,
      newdata = descriptors, type = "response"
    ))
  )

  applicability <- .amd_predictor_applicability(prepared$data, contract)
  supported_k <- descriptors$prediction[[1L]] %in% 1:8
  applicability$within_observed_output_support <- supported_k
  expected_missingness <- imputation_diagnostics$unexpected_nonfinite_values == 0L
  expected_constants <- !length(imputation_diagnostics$unassessed_constant_predictors)
  applicability$missingness_pattern_assessed <- expected_missingness
  applicability$training_constants_assessed <- expected_constants
  validated <- applicability$on_validated_grid && supported_k && expected_missingness && expected_constants
  applicability$proposal_status <- if (validated) "conditional_within_assessed_numeric_grid" else {
    "exploratory_outside_assessed_conditions"
  }
  reliability_status <- if (validated) {
    "retrospectively_assessed_on_numeric_grid"
  } else if (!supported_k) {
    "suppressed_outside_observed_output_support"
  } else if (!expected_missingness) {
    "suppressed_unassessed_missingness_pattern"
  } else if (!expected_constants) {
    "suppressed_unassessed_constant_predictors"
  } else if (allow_extrapolation) {
    applicability$numerical_status
  } else {
    paste0("suppressed_", applicability$numerical_status)
  }
  reported <- if (validated || (allow_extrapolation && supported_k && expected_missingness && expected_constants)) calibrated else {
    c(structure = NA_real_, exact = NA_real_)
  }

  out <- list(
    proposed_k = as.integer(descriptors$prediction[[1L]]),
    has_structure = isTRUE(descriptors$prediction[[1L]] > 1L),
    reliability = list(
      structure = unname(reported[["structure"]]),
      exact_k = unname(reported[["exact"]]),
      status = reliability_status
    ),
    curve = curve,
    numerical_peak = as.integer(curve$c_peak),
    candidate_scores = candidate_scores,
    per_forest = seed_predictions,
    diagnostics = list(
      descriptors = descriptors,
      calibrated_values = calibrated,
      raw_candidate_scores = score_long,
      candidate_features = feature_result$features,
      seed = as.integer(seed),
      seed_plan = if (computed_curve) seed_plan else seed_plan[c("stability", "data")],
      curve_source = if (computed_curve) "computed" else "supplied",
      curve_seeds_used = if (computed_curve) seed_plan$curve else curve$seeds,
      imputation = imputation_diagnostics,
      resources = resource_check,
      refits_are_computational_not_resampling = TRUE,
      omitted_rows = prepared$omitted_rows
    ),
    applicability = applicability,
    predictor = list(
      schema = predictor$schema,
      feature_contract = predictor$feature_contract,
      sha256 = attr(predictor, "sha256"),
      confidence_assessment = predictor$validation$confidence_transfer_assessment,
      created_utc = predictor$created_utc,
      source_path = attr(predictor, "source_path"),
      resolved_target = predictor$contract$resolved_target
    ),
    call = match.call()
  )
  class(out) <- "amd_prediction"
  out
}

#' Print an optional AMD reader result
#' @param x An object returned by [predict_amd()].
#' @param ... Unused.
#' @return Invisibly returns `x`.
#' @export
print.amd_prediction <- function(x, ...) {
  if (!inherits(x, "amd_prediction")) {
    stop("x must be an amd_prediction object.", call. = FALSE)
  }
  cat("Model-assisted AMD interpretation (conditional simulation evidence)\n")
  cat("  Target: numerically resolved density-mode count (simulation reference)\n")
  cat("  Proposed modal count (K):", x$proposed_k, "\n")
  cat("  Proposed modal structure:", if (x$has_structure) "several modes" else "one mode", "\n")
  cat("  Proposal status:", x$applicability$proposal_status, "\n")
  if (!isTRUE(x$applicability$on_validated_grid)) {
    cat("  The proposal itself is exploratory outside the assessed numerical grid.\n")
  }
  cat("  Numerical AMD peak:", x$numerical_peak, "\n")
  if (is.finite(x$reliability$structure)) {
    cat("  Reliability of the one/several-modes decision:",
        sprintf("%.1f%%", 100 * x$reliability$structure), "\n")
    cat("  Reliability, exact modal count:",
        sprintf("%.1f%%", 100 * x$reliability$exact_k), "\n")
  } else {
    cat("  Reliability: not reported outside the assessed numerical/output/feature conditions\n")
  }
  cat("  Reliability status:", x$reliability$status, "\n")
  cat("  Numerical grid membership does not certify new-data geometry.\n")
  invisible(x)
}

#' Summarise an optional AMD reader result
#' @param object An object returned by [predict_amd()].
#' @param ... Unused.
#' @return A list of proposals, conditional reliability, applicability and scores.
#' @export
summary.amd_prediction <- function(object, ...) {
  if (!inherits(object, "amd_prediction")) {
    stop("object must be an amd_prediction object.", call. = FALSE)
  }
  list(
    target = object$predictor$resolved_target,
    proposal_status = object$applicability$proposal_status,
    proposed_k = object$proposed_k,
    has_structure = object$has_structure,
    numerical_peak = object$numerical_peak,
    reliability = object$reliability,
    applicability = object$applicability,
    forest_votes = table(object$per_forest$prediction),
    leading_candidates = object$candidate_scores[
      order(object$candidate_scores$mean_score, decreasing = TRUE),
      , drop = FALSE
    ]
  )
}

#' Plot an optional AMD reader result
#'
#' The fitted AMD curve remains the primary display. The numerical AMD maximum
#' and model-assisted proposal are shown separately. The analytical `c = 1`,
#' `AMD = 0` anchor is optional because including it changes the vertical scale
#' and can make the shape among `c = 2,...,14` less visible.
#'
#' @param x An `amd_prediction` object.
#' @param show_c1 Logical; show the unconnected analytical `c = 1` anchor.
#' @param ... Additional arguments passed to [graphics::plot()].
#' @return Invisibly returns `x`.
#' @export
plot.amd_prediction <- function(x, show_c1 = FALSE, ...) {
  if (!inherits(x, "amd_prediction")) {
    stop("x must be an amd_prediction object.", call. = FALSE)
  }
  if (length(show_c1) != 1L || is.na(show_c1) || !is.logical(show_c1)) {
    stop("show_c1 must be TRUE or FALSE.", call. = FALSE)
  }
  candidates <- as.integer(names(x$curve$curve))
  values <- as.numeric(x$curve$curve)
  x_values <- if (show_c1) c(1L, candidates) else candidates
  y_values <- if (show_c1) c(0, values) else values
  plot_options <- utils::modifyList(list(type = "n",
    xlab = "Number of configurations (c)", ylab = "AMD"), list(...))
  do.call(graphics::plot, c(list(x = x_values, y = y_values), plot_options))
  graphics::lines(candidates, values, type = "b", pch = 16)
  if (show_c1) graphics::points(1, 0, pch = 1, col = "grey40")
  graphics::abline(v = x$numerical_peak, col = "#2C7FB8", lty = 3)
  graphics::abline(v = x$proposed_k, col = "#D7301F", lty = 2)
  graphics::legend(
    "topright",
    legend = c("AMD numerical peak", "model-assisted proposal"),
    col = c("#2C7FB8", "#D7301F"), lty = c(3, 2), bty = "n"
  )
  invisible(x)
}
