candidate_safe_ratio <- function(numerator, denominator) {
  if (is.finite(denominator) && denominator > .Machine$double.eps) {
    numerator / denominator
  } else {
    0
  }
}

candidate_trailing_run <- function(condition) {
  if (!length(condition) || !utils::tail(condition, 1L)) return(0L)
  encoded <- rle(rev(condition))
  as.integer(encoded$lengths[[1L]])
}

candidate_leading_run <- function(condition) {
  if (!length(condition) || !condition[[1L]]) return(0L)
  encoded <- rle(condition)
  as.integer(encoded$lengths[[1L]])
}

candidate_curve_columns <- function(data) {
  columns <- grep("^curve__amd_c[0-9]+$", names(data), value = TRUE)
  c_values <- as.integer(sub("^curve__amd_c", "", columns))
  ordering <- order(c_values)
  columns <- columns[ordering]
  c_values <- c_values[ordering]
  if (!identical(c_values, 2:14)) {
    stop("candidate analysis requires curve points c=2,...,14.",
         call. = FALSE)
  }
  stats::setNames(columns, c_values)
}

candidate_global_features <- function(c_values, y_values) {
  description <- describe_amd_curve(c_values, y_values)
  keep <- c(
    "amplitude", "curve_min", "curve_max", "curve_mean", "curve_sd",
    "net_change", "net_change_over_amplitude", "total_variation",
    "excess_variation", "positive_variation", "negative_variation",
    "monotonicity_index", "positive_step_fraction",
    "negative_step_fraction", "longest_positive_run",
    "longest_negative_run", "maximum_drawdown",
    "maximum_drawdown_over_amplitude", "terminal_drawdown",
    "terminal_drawdown_over_amplitude", "tail_slope",
    "tail_slope_over_amplitude", "terminal_variation_fraction",
    "top_gap", "top_gap_over_amplitude", "top_two_adjacent",
    "strongest_local_apex", "strongest_local_apex_over_amplitude"
  )
  values <- description[1L, keep, drop = FALSE]
  names(values) <- paste0("global__", names(values))
  values
}

candidate_local_features <- function(candidate, c_values, y_values) {
  amplitude <- diff(range(y_values))
  increments <- diff(y_values)
  numeric_candidate <- candidate %in% c_values
  position <- match(candidate, c_values)
  has_left <- numeric_candidate && position > 1L
  has_left2 <- numeric_candidate && position > 2L
  has_right <- numeric_candidate && position < length(y_values)
  has_right2 <- numeric_candidate && position < length(y_values) - 1L

  value <- if (numeric_candidate) y_values[[position]] else 0
  left_step <- if (has_left) value - y_values[[position - 1L]] else 0
  right_drop <- if (has_right) value - y_values[[position + 1L]] else 0
  two_sided <- has_left && has_right
  curvature <- if (two_sided) left_step + right_drop else 0
  local_apex <- if (two_sided) min(left_step, right_drop) else 0
  asymmetry <- if (two_sided) {
    candidate_safe_ratio(left_step - right_drop,
                         abs(left_step) + abs(right_drop))
  } else 0

  before <- if (has_left) y_values[seq_len(position - 1L)] else numeric()
  after <- if (has_right) y_values[(position + 1L):length(y_values)] else numeric()
  incoming_index <- if (has_left) position - 1L else 0L
  outgoing_index <- if (has_right) position else 0L
  left_run <- if (has_left) {
    candidate_trailing_run(increments[seq_len(incoming_index)] > 0)
  } else 0L
  right_run <- if (has_right) {
    candidate_leading_run(increments[outgoing_index:length(increments)] < 0)
  } else 0L

  global_max <- max(y_values)
  relative_height <- if (numeric_candidate) {
    candidate_safe_ratio(value - min(y_values), amplitude)
  } else 0
  gap_to_max <- if (numeric_candidate) global_max - value else 0
  rank_from_top <- if (numeric_candidate) {
    rank(-y_values, ties.method = "min")[[position]]
  } else length(y_values) + 1L

  right_pair_available <- numeric_candidate && has_right
  right_pair_floor <- if (right_pair_available) {
    min(value, y_values[[position + 1L]])
  } else 0
  right_pair_after <- if (has_right2) {
    y_values[(position + 2L):length(y_values)]
  } else numeric()
  right_pair_gap <- if (right_pair_available) {
    abs(value - y_values[[position + 1L]])
  } else 0
  right_pair_left_rise <- if (right_pair_available && has_left) {
    right_pair_floor - y_values[[position - 1L]]
  } else 0
  right_pair_immediate_drop <- if (has_right2) {
    right_pair_floor - y_values[[position + 2L]]
  } else 0

  left_pair_available <- numeric_candidate && has_left
  left_pair_floor <- if (left_pair_available) {
    min(y_values[[position - 1L]], value)
  } else 0
  left_pair_after <- if (has_right) after else numeric()
  left_pair_gap <- if (left_pair_available) {
    abs(y_values[[position - 1L]] - value)
  } else 0
  left_pair_prior_rise <- if (left_pair_available && has_left2) {
    left_pair_floor - y_values[[position - 2L]]
  } else 0
  left_pair_immediate_drop <- if (left_pair_available && has_right) {
    left_pair_floor - y_values[[position + 1L]]
  } else 0

  data.frame(
    cand__candidate_is_one = as.integer(candidate == 1L),
    cand__numeric_point_available = as.integer(numeric_candidate),
    cand__has_left = as.integer(has_left),
    cand__has_left2 = as.integer(has_left2),
    cand__has_right = as.integer(has_right),
    cand__has_right2 = as.integer(has_right2),
    cand__at_lower_numeric_boundary = as.integer(candidate == min(c_values)),
    cand__at_upper_numeric_boundary = as.integer(candidate == max(c_values)),
    cand__value = value,
    cand__relative_height = relative_height,
    cand__is_global_max = as.integer(
      numeric_candidate && value == global_max
    ),
    cand__rank_from_top = rank_from_top,
    cand__gap_to_global_max = gap_to_max,
    cand__gap_to_global_max_over_amplitude = candidate_safe_ratio(
      gap_to_max, amplitude
    ),
    cand__left_step = left_step,
    cand__left_step_over_amplitude = candidate_safe_ratio(left_step, amplitude),
    cand__right_drop = right_drop,
    cand__right_drop_over_amplitude = candidate_safe_ratio(
      right_drop, amplitude
    ),
    cand__curvature = curvature,
    cand__curvature_over_amplitude = candidate_safe_ratio(
      curvature, amplitude
    ),
    cand__local_apex = local_apex,
    cand__local_apex_over_amplitude = candidate_safe_ratio(
      local_apex, amplitude
    ),
    cand__apex_asymmetry = asymmetry,
    cand__both_sides_support_apex = as.integer(
      two_sided && left_step > 0 && right_drop > 0
    ),
    cand__left_positive_run = left_run,
    cand__right_negative_run = right_run,
    cand__pre_mean_rise = if (length(before)) value - mean(before) else 0,
    cand__pre_all_margin = if (length(before)) value - max(before) else 0,
    cand__pre_below_fraction = if (length(before)) mean(before < value) else 0,
    cand__post_mean_drop = if (length(after)) value - mean(after) else 0,
    cand__post_eventual_drop = if (length(after)) value - min(after) else 0,
    cand__post_all_margin = if (length(after)) value - max(after) else 0,
    cand__post_below_fraction = if (length(after)) mean(after < value) else 0,
    cand__right_pair_available = as.integer(right_pair_available),
    cand__right_pair_gap = right_pair_gap,
    cand__right_pair_gap_over_amplitude = candidate_safe_ratio(
      right_pair_gap, amplitude
    ),
    cand__right_pair_left_rise = right_pair_left_rise,
    cand__right_pair_immediate_drop = right_pair_immediate_drop,
    cand__right_pair_post_all_margin = if (length(right_pair_after)) {
      right_pair_floor - max(right_pair_after)
    } else 0,
    cand__right_pair_post_below_fraction = if (length(right_pair_after)) {
      mean(right_pair_after < right_pair_floor)
    } else 0,
    cand__left_pair_available = as.integer(left_pair_available),
    cand__left_pair_gap = left_pair_gap,
    cand__left_pair_gap_over_amplitude = candidate_safe_ratio(
      left_pair_gap, amplitude
    ),
    cand__left_pair_prior_rise = left_pair_prior_rise,
    cand__left_pair_immediate_drop = left_pair_immediate_drop,
    cand__left_pair_post_all_margin = if (left_pair_available && length(left_pair_after)) {
      left_pair_floor - max(left_pair_after)
    } else 0,
    cand__left_pair_post_below_fraction = if (length(left_pair_after)) {
      mean(left_pair_after < left_pair_floor)
    } else 0,
    stringsAsFactors = FALSE
  )
}

build_candidate_curve_data <- function(meta_features,
                                       allowed_banks = c("discovery", "selection"),
                                       candidates = 1:14) {
  required <- c("scenario_id", "bank", "K", "N", "D")
  missing <- setdiff(required, names(meta_features))
  if (length(missing)) {
    stop("candidate input lacks: ", paste(missing, collapse = ", "),
         call. = FALSE)
  }
  observed_banks <- sort(unique(as.character(meta_features$bank)))
  if (!all(observed_banks %in% allowed_banks)) {
    stop("candidate input contains a forbidden bank: ",
         paste(setdiff(observed_banks, allowed_banks), collapse = ", "),
         call. = FALSE)
  }
  if (anyDuplicated(meta_features$scenario_id)) {
    stop("scenario_id must be unique before candidate expansion.",
         call. = FALSE)
  }
  if (!identical(as.integer(candidates), 1:14)) {
    stop("the exploratory candidate support must be c=1,...,14.",
         call. = FALSE)
  }

  curve_columns <- candidate_curve_columns(meta_features)
  c_values <- as.integer(names(curve_columns))
  data_columns <- grep("^data__", names(meta_features), value = TRUE)
  rows <- vector("list", nrow(meta_features))

  for (row_index in seq_len(nrow(meta_features))) {
    source <- meta_features[row_index, , drop = FALSE]
    y_values <- as.numeric(source[1L, unname(curve_columns), drop = TRUE])
    if (any(!is.finite(y_values))) {
      stop("non-finite AMD curve in scenario ", source$scenario_id,
           call. = FALSE)
    }
    global <- candidate_global_features(c_values, y_values)
    carried <- source[1L, data_columns, drop = FALSE]
    local <- do.call(rbind, lapply(
      candidates, candidate_local_features,
      c_values = c_values, y_values = y_values
    ))
    is_true <- candidates == as.integer(source$K)
    metadata <- data.frame(
      scenario_id = rep(as.character(source$scenario_id), length(candidates)),
      bank = rep(as.character(source$bank), length(candidates)),
      true_K = rep(as.integer(source$K), length(candidates)),
      candidate_c = as.integer(candidates),
      true_candidate = factor(
        ifelse(is_true, "yes", "no"), levels = c("no", "yes")
      ),
      candidate_weight = ifelse(is_true, length(candidates) - 1L, 1),
      N = rep(as.integer(source$N), length(candidates)),
      D = rep(as.integer(source$D), length(candidates)),
      stringsAsFactors = FALSE
    )
    global_block <- global[rep(1L, length(candidates)), , drop = FALSE]
    carried_block <- carried[rep(1L, length(candidates)), , drop = FALSE]
    rows[[row_index]] <- cbind(
      metadata, local, global_block, carried_block,
      stringsAsFactors = FALSE
    )
  }
  result <- do.call(rbind, rows)
  rownames(result) <- NULL

  counts <- table(result$scenario_id)
  positives <- tapply(result$true_candidate == "yes", result$scenario_id, sum)
  if (any(counts != length(candidates)) || any(positives != 1L)) {
    stop("each curve must yield 14 candidates and exactly one truth.",
         call. = FALSE)
  }
  if (any(!is.finite(result$candidate_weight)) ||
      any(result$candidate_weight <= 0)) {
    stop("candidate weights must be finite and positive.", call. = FALSE)
  }
  result
}

candidate_predictor_names <- function(data, feature_set) {
  curve <- c(
    grep("^cand__", names(data), value = TRUE),
    grep("^global__", names(data), value = TRUE)
  )
  data_descriptors <- grep("^data__", names(data), value = TRUE)
  candidates <- switch(
    feature_set,
    curve_only = curve,
    curve_plus_ND = c(curve, intersect(c("N", "D"), names(data))),
    curve_plus_data = c(
      curve, intersect(c("N", "D"), names(data)), data_descriptors
    ),
    stop("unknown candidate feature set: ", feature_set, call. = FALSE)
  )
  candidates <- unique(candidates)
  candidates[vapply(data[candidates], is.numeric, logical(1L))]
}
