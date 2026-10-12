safe_ratio <- function(value, denominator) {
  if (is.finite(denominator) && denominator > 0) {
    value / denominator
  } else {
    NA_real_
  }
}

longest_true_run <- function(condition) {
  encoded <- rle(condition)
  if (!any(encoded$values)) return(0L)
  as.integer(max(encoded$lengths[encoded$values]))
}

validate_curve_vectors <- function(c_values, y_values) {
  ordering <- order(c_values)
  c_values <- as.integer(c_values[ordering])
  y_values <- as.numeric(y_values[ordering])
  if (length(y_values) < 3L || any(diff(c_values) != 1L) ||
      any(!is.finite(y_values))) {
    stop("a curve needs at least three finite consecutive points.",
         call. = FALSE)
  }
  list(c = c_values, y = y_values)
}

describe_amd_curve <- function(c_values, y_values) {
  curve <- validate_curve_vectors(c_values, y_values)
  c_values <- curve$c
  y_values <- curve$y
  increments <- diff(y_values)
  amplitude <- diff(range(y_values))
  total_variation <- sum(abs(increments))
  positive_variation <- sum(pmax(increments, 0))
  negative_variation <- sum(pmax(-increments, 0))
  net_change <- utils::tail(y_values, 1L) - y_values[[1L]]

  drop_matrix <- outer(y_values, y_values, "-")
  drop_matrix[lower.tri(drop_matrix, diag = TRUE)] <- NA_real_
  if (max(drop_matrix, na.rm = TRUE) <= 0) {
    maximum_drawdown <- 0
    drawdown_start <- NA_integer_
    drawdown_end <- NA_integer_
  } else {
    index <- which(drop_matrix == max(drop_matrix, na.rm = TRUE),
                   arr.ind = TRUE)[1L, ]
    maximum_drawdown <- drop_matrix[index[[1L]], index[[2L]]]
    drawdown_start <- c_values[[index[[1L]]]]
    drawdown_end <- c_values[[index[[2L]]]]
  }

  peak_index <- which.max(y_values)
  ranked <- order(y_values, decreasing = TRUE)
  top_gap <- y_values[[ranked[[1L]]]] - y_values[[ranked[[2L]]]]

  local_apex <- rep(NA_real_, length(y_values))
  local_apex[2:(length(y_values) - 1L)] <- pmin(
    utils::head(increments, -1L), -utils::tail(increments, -1L)
  )
  positive_apex <- which(is.finite(local_apex) & local_apex > 0)
  if (length(positive_apex)) {
    strongest_index <- positive_apex[[which.max(local_apex[positive_apex])]]
    strongest_local_apex <- local_apex[[strongest_index]]
    left_rise <- increments[[strongest_index - 1L]]
    right_drop <- -increments[[strongest_index]]
    apex_asymmetry <- (left_rise - right_drop) / (left_rise + right_drop)
    strongest_c <- c_values[[strongest_index]]
  } else {
    strongest_local_apex <- 0
    left_rise <- NA_real_
    right_drop <- NA_real_
    apex_asymmetry <- NA_real_
    strongest_c <- NA_integer_
  }

  tail_indices <- utils::tail(
    seq_along(y_values), min(3L, length(y_values))
  )
  tail_slope <- unname(stats::coef(stats::lm(
    y_values[tail_indices] ~ c_values[tail_indices]
  ))[[2L]])
  terminal_steps <- utils::tail(increments, min(2L, length(increments)))

  data.frame(
    c_min = min(c_values),
    c_max = max(c_values),
    evaluated_points = length(c_values),
    amplitude = amplitude,
    curve_min = min(y_values),
    curve_max = max(y_values),
    curve_mean = mean(y_values),
    curve_sd = stats::sd(y_values),
    net_change = net_change,
    net_change_over_amplitude = safe_ratio(net_change, amplitude),
    total_variation = total_variation,
    excess_variation = total_variation - amplitude,
    positive_variation = positive_variation,
    negative_variation = negative_variation,
    monotonicity_index = safe_ratio(net_change, total_variation),
    positive_step_fraction = mean(increments > 0),
    negative_step_fraction = mean(increments < 0),
    zero_step_fraction = mean(increments == 0),
    longest_positive_run = longest_true_run(increments > 0),
    longest_negative_run = longest_true_run(increments < 0),
    maximum_drawdown = maximum_drawdown,
    maximum_drawdown_over_amplitude = safe_ratio(maximum_drawdown, amplitude),
    drawdown_start_c = drawdown_start,
    drawdown_end_c = drawdown_end,
    terminal_drawdown = max(y_values) - utils::tail(y_values, 1L),
    terminal_drawdown_over_amplitude = safe_ratio(
      max(y_values) - utils::tail(y_values, 1L), amplitude
    ),
    tail_slope = tail_slope,
    tail_slope_over_amplitude = safe_ratio(tail_slope, amplitude),
    terminal_variation_fraction = safe_ratio(
      sum(abs(terminal_steps)), total_variation
    ),
    numerical_peak_c = c_values[[peak_index]],
    peak_at_lower_boundary = as.integer(peak_index == 1L),
    peak_at_upper_boundary = as.integer(peak_index == length(y_values)),
    top_gap = top_gap,
    top_gap_over_amplitude = safe_ratio(top_gap, amplitude),
    top_two_adjacent = as.integer(
      abs(c_values[[ranked[[1L]]]] - c_values[[ranked[[2L]]]]) == 1L
    ),
    strongest_local_apex = strongest_local_apex,
    strongest_local_apex_over_amplitude = safe_ratio(
      strongest_local_apex, amplitude
    ),
    strongest_local_apex_c = strongest_c,
    strongest_apex_left_rise = left_rise,
    strongest_apex_right_drop = right_drop,
    strongest_apex_asymmetry = apex_asymmetry,
    stringsAsFactors = FALSE
  )
}

describe_amd_pairs <- function(c_values, y_values) {
  curve <- validate_curve_vectors(c_values, y_values)
  c_values <- curve$c
  y_values <- curve$y
  amplitude <- diff(range(y_values))
  rows <- lapply(seq_len(length(y_values) - 1L), function(index) {
    floor_value <- min(y_values[index:(index + 1L)])
    left_rise <- if (index > 1L) {
      floor_value - y_values[[index - 1L]]
    } else {
      NA_real_
    }
    right_drop <- if (index + 1L < length(y_values)) {
      floor_value - y_values[[index + 2L]]
    } else {
      NA_real_
    }
    sustained_drop <- if (index + 1L < length(y_values)) {
      floor_value - min(y_values[(index + 2L):length(y_values)])
    } else {
      NA_real_
    }
    gap <- abs(y_values[[index + 1L]] - y_values[[index]])
    data.frame(
      c_left = c_values[[index]],
      c_right = c_values[[index + 1L]],
      pair_mean = mean(y_values[index:(index + 1L)]),
      pair_gap = gap,
      pair_gap_over_amplitude = safe_ratio(gap, amplitude),
      pair_left_rise = left_rise,
      pair_left_rise_over_amplitude = safe_ratio(left_rise, amplitude),
      pair_right_drop = right_drop,
      pair_right_drop_over_amplitude = safe_ratio(right_drop, amplitude),
      pair_sustained_drop = sustained_drop,
      pair_sustained_drop_over_amplitude = safe_ratio(
        sustained_drop, amplitude
      ),
      stringsAsFactors = FALSE
    )
  })
  do.call(rbind, rows)
}

wide_amd_curve_features <- function(c_values, y_values) {
  curve <- validate_curve_vectors(c_values, y_values)
  c_values <- curve$c
  y_values <- curve$y
  base <- describe_amd_curve(c_values, y_values)
  raw_names <- sprintf("amd_c%02d", c_values)
  raw <- as.data.frame(as.list(stats::setNames(y_values, raw_names)))

  increments <- diff(y_values)
  increment_names <- sprintf("delta_c%02d_to_c%02d",
                             utils::head(c_values, -1L),
                             utils::tail(c_values, -1L))
  increment_frame <- as.data.frame(as.list(stats::setNames(
    increments, increment_names
  )))

  second <- diff(y_values, differences = 2L)
  second_names <- sprintf("second_difference_c%02d", c_values[-c(1L, length(c_values))])
  second_frame <- as.data.frame(as.list(stats::setNames(second, second_names)))

  pairs <- describe_amd_pairs(c_values, y_values)
  pair_columns <- setdiff(names(pairs), c("c_left", "c_right"))
  pair_values <- list()
  for (row in seq_len(nrow(pairs))) {
    suffix <- sprintf("c%02d_c%02d", pairs$c_left[[row]], pairs$c_right[[row]])
    for (column in pair_columns) {
      pair_values[[paste0(column, "_", suffix)]] <- pairs[[column]][[row]]
    }
  }
  pair_frame <- as.data.frame(pair_values, check.names = FALSE)
  cbind(base, raw, increment_frame, second_frame, pair_frame,
        stringsAsFactors = FALSE)
}

prefix_columns <- function(data, prefix) {
  names(data) <- paste0(prefix, names(data))
  data
}

amd_predictor_names <- function(data, feature_set) {
  curve <- grep("^curve__", names(data), value = TRUE)
  descriptors <- grep("^data__", names(data), value = TRUE)
  candidates <- switch(
    feature_set,
    curve_only = curve,
    data_only = c(intersect(c("N", "D"), names(data)), descriptors),
    curve_plus_ND = c(curve, intersect(c("N", "D"), names(data))),
    curve_plus_data = c(
      curve, intersect(c("N", "D"), names(data)), descriptors
    ),
    stop("unknown feature set: ", feature_set, call. = FALSE)
  )
  unique(candidates[vapply(data[candidates], is.numeric, logical(1L))])
}
