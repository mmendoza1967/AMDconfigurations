advanced_curve_safe_ratio <- function(numerator, denominator,
                                      fallback = 0) {
  if (length(denominator) != 1L || !is.finite(denominator) ||
      abs(denominator) <= .Machine$double.eps) {
    return(rep_len(fallback, length(numerator)))
  }
  numerator / denominator
}

advanced_linear_fit <- function(x, y) {
  if (length(x) < 2L || length(unique(x)) < 2L) {
    return(c(slope = NA_real_, fitted_ss = NA_real_, residual_ss = NA_real_))
  }
  fit <- stats::lm.fit(cbind(1, x), y)
  fitted <- as.numeric(cbind(1, x) %*% fit$coefficients)
  c(
    slope = fit$coefficients[[2L]],
    fitted_ss = sum((fitted - mean(y))^2),
    residual_ss = sum((y - fitted)^2)
  )
}

advanced_local_peak_positions <- function(y) {
  if (length(y) < 3L) return(integer())
  which(vapply(2:(length(y) - 1L), function(index) {
    y[[index]] >= y[[index - 1L]] && y[[index]] >= y[[index + 1L]] &&
      (y[[index]] > y[[index - 1L]] || y[[index]] > y[[index + 1L]])
  }, logical(1L))) + 1L
}

advanced_topographic_prominence <- function(y, position) {
  if (position <= 1L || position >= length(y) ||
      y[[position]] < y[[position - 1L]] ||
      y[[position]] < y[[position + 1L]]) {
    return(0)
  }
  higher_left <- which(y[seq_len(position - 1L)] > y[[position]])
  higher_right_relative <- which(
    y[(position + 1L):length(y)] > y[[position]]
  )
  left_boundary <- if (length(higher_left)) max(higher_left) else 1L
  right_boundary <- if (length(higher_right_relative)) {
    position + min(higher_right_relative)
  } else length(y)
  left_valley <- min(y[left_boundary:position])
  right_valley <- min(y[position:right_boundary])
  max(0, y[[position]] - max(left_valley, right_valley))
}

advanced_near_max_topology <- function(y, amplitude, tolerance_fraction) {
  tolerance <- tolerance_fraction * amplitude
  selected <- y >= max(y) - tolerance
  runs <- rle(selected)
  selected_runs <- which(runs$values)
  positions <- which(selected)
  c(
    count = sum(selected),
    fraction = mean(selected),
    components = length(selected_runs),
    span = if (length(positions)) max(positions) - min(positions) + 1L else 0L
  )
}

advanced_secondary_peak <- function(c_values, y, amplitude) {
  positions <- advanced_local_peak_positions(y)
  if (length(positions) < 2L) {
    return(c(
      height_ratio = 0, distance = 0, valley_depth_over_amplitude = 0
    ))
  }
  ordered <- positions[order(y[positions], decreasing = TRUE)]
  primary <- ordered[[1L]]
  secondary <- ordered[[2L]]
  interval <- seq.int(min(primary, secondary), max(primary, secondary))
  valley <- min(y[interval])
  c(
    height_ratio = if (abs(y[[primary]]) > .Machine$double.eps) {
      y[[secondary]] / y[[primary]]
    } else 0,
    distance = abs(c_values[[primary]] - c_values[[secondary]]),
    valley_depth_over_amplitude = advanced_curve_safe_ratio(
      min(y[[primary]], y[[secondary]]) - valley, amplitude
    )
  )
}

advanced_restart_features <- function(c_values, raw_scores, converged) {
  if (is.null(raw_scores) || !is.matrix(raw_scores) ||
      ncol(raw_scores) != length(c_values)) {
    return(list(
      sd = rep(NA_real_, length(c_values)),
      peak_fraction = rep(NA_real_, length(c_values)),
      peak_entropy = NA_real_
    ))
  }
  scores <- raw_scores
  if (!is.null(converged) && identical(dim(converged), dim(scores))) {
    scores[!converged] <- NA_real_
  }
  score_sd <- apply(scores, 2L, function(value) {
    value <- value[is.finite(value)]
    if (length(value) >= 2L) stats::sd(value) else NA_real_
  })
  peak_position <- apply(scores, 1L, function(value) {
    if (!any(is.finite(value))) return(NA_integer_)
    which.max(replace(value, !is.finite(value), -Inf))
  })
  peak_fraction <- vapply(seq_along(c_values), function(position) {
    mean(peak_position == position, na.rm = TRUE)
  }, numeric(1L))
  probabilities <- peak_fraction[peak_fraction > 0]
  peak_entropy <- if (length(probabilities) && length(c_values) > 1L) {
    -sum(probabilities * log(probabilities)) / log(length(c_values))
  } else 0
  list(sd = score_sd, peak_fraction = peak_fraction,
       peak_entropy = peak_entropy)
}

advanced_candidate_curve_features <- function(
    candidates, c_values, y_values, raw_scores = NULL, converged = NULL,
    near_max_tolerance = 0.05, ambiguity_temperature = 0.10) {
  candidates <- as.integer(candidates)
  c_values <- as.integer(c_values)
  y_values <- as.numeric(y_values)
  if (!identical(c_values, sort(unique(c_values))) ||
      length(c_values) != length(y_values) || any(!is.finite(y_values))) {
    stop("advanced curve inputs are invalid.", call. = FALSE)
  }
  amplitude <- diff(range(y_values))
  scale <- max(amplitude, .Machine$double.eps)
  near_max <- advanced_near_max_topology(
    y_values, amplitude, near_max_tolerance
  )
  secondary <- advanced_secondary_peak(c_values, y_values, amplitude)
  soft_temperature <- max(ambiguity_temperature * amplitude,
                          .Machine$double.eps)
  soft_weights <- exp((y_values - max(y_values)) / soft_temperature)
  soft_weights <- soft_weights / sum(soft_weights)
  ambiguity_entropy <- -sum(soft_weights * log(soft_weights)) /
    log(length(soft_weights))
  effective_candidates <- exp(-sum(soft_weights * log(soft_weights)))
  smoothed <- stats::filter(y_values, rep(1 / 3, 3L), sides = 2L)
  smoothed <- as.numeric(smoothed)
  smoothed[!is.finite(smoothed)] <- y_values[!is.finite(smoothed)]
  smoothed_peak <- c_values[[which.max(smoothed)]]
  restart <- advanced_restart_features(
    c_values, raw_scores, converged
  )

  rows <- lapply(candidates, function(candidate) {
    position <- match(candidate, c_values)
    available <- !is.na(position)
    if (!available) {
      return(data.frame(
        adv__numeric_available = 0L,
        adv__unimodality_loss = NA_real_,
        adv__post_rebound_over_amplitude = NA_real_,
        adv__post_sign_reversals = NA_real_,
        adv__first_drop_fraction_of_eventual = NA_real_,
        adv__late_slope_over_amplitude = NA_real_,
        adv__right_evidence_fraction = NA_real_,
        adv__terminal_margin_over_amplitude = NA_real_,
        adv__near_max_member = 0L,
        adv__topographic_prominence_over_amplitude = NA_real_,
        adv__left_segment_slope_over_amplitude = NA_real_,
        adv__right_segment_slope_over_amplitude = NA_real_,
        adv__segmented_r_squared = NA_real_,
        adv__segmented_rmse_over_amplitude = NA_real_,
        adv__break_strength_over_amplitude = NA_real_,
        adv__smoothed_peak_member = 0L,
        adv__left_step_signal_to_noise = NA_real_,
        adv__right_drop_signal_to_noise = NA_real_,
        adv__restart_peak_fraction = NA_real_,
        stringsAsFactors = FALSE
      ))
    }

    left_indices <- seq_len(position)
    right_indices <- position:length(y_values)
    left_isotonic <- stats::isoreg(
      c_values[left_indices], y_values[left_indices]
    )$yf
    right_isotonic <- rev(stats::isoreg(
      rev(-c_values[right_indices]), rev(y_values[right_indices])
    )$yf)
    isotonic_residual <- sum(
      (y_values[left_indices] - left_isotonic)^2
    ) + sum((y_values[right_indices] - right_isotonic)^2)
    unimodality_loss <- isotonic_residual /
      (scale^2 * (length(left_indices) + length(right_indices)))

    after <- if (position < length(y_values)) {
      y_values[(position + 1L):length(y_values)]
    } else numeric()
    post_increments <- if (length(after) >= 2L) diff(after) else numeric()
    running_minimum <- if (length(after)) cummin(after) else numeric()
    rebound <- if (length(after) >= 2L) {
      max(c(0, after[-1L] - running_minimum[-length(running_minimum)]))
    } else 0
    sign_values <- sign(post_increments[post_increments != 0])
    sign_reversals <- if (length(sign_values) >= 2L) {
      sum(diff(sign_values) != 0)
    } else 0
    immediate_drop <- if (length(after)) y_values[[position]] - after[[1L]] else 0
    eventual_drop <- if (length(after)) {
      y_values[[position]] - min(after)
    } else 0
    first_drop_fraction <- if (eventual_drop > .Machine$double.eps) {
      immediate_drop / eventual_drop
    } else 0
    late_fit <- if (length(after) >= 2L) {
      advanced_linear_fit(c_values[(position + 1L):length(y_values)], after)
    } else c(slope = NA_real_, fitted_ss = NA_real_, residual_ss = NA_real_)

    left_fit <- advanced_linear_fit(
      c_values[left_indices], y_values[left_indices]
    )
    right_fit <- advanced_linear_fit(
      c_values[right_indices], y_values[right_indices]
    )
    total_ss <- sum((y_values - mean(y_values))^2)
    segmented_residual <- left_fit[["residual_ss"]] +
      right_fit[["residual_ss"]]
    segmented_r_squared <- if (is.finite(segmented_residual) &&
                               total_ss > .Machine$double.eps) {
      1 - segmented_residual / total_ss
    } else NA_real_

    left_step <- if (position > 1L) {
      y_values[[position]] - y_values[[position - 1L]]
    } else NA_real_
    right_drop <- if (position < length(y_values)) {
      y_values[[position]] - y_values[[position + 1L]]
    } else NA_real_
    left_noise <- if (position > 1L) {
      sqrt(restart$sd[[position]]^2 + restart$sd[[position - 1L]]^2)
    } else NA_real_
    right_noise <- if (position < length(y_values)) {
      sqrt(restart$sd[[position]]^2 + restart$sd[[position + 1L]]^2)
    } else NA_real_

    data.frame(
      adv__numeric_available = 1L,
      adv__unimodality_loss = unimodality_loss,
      adv__post_rebound_over_amplitude = rebound / scale,
      adv__post_sign_reversals = sign_reversals,
      adv__first_drop_fraction_of_eventual = first_drop_fraction,
      adv__late_slope_over_amplitude = late_fit[["slope"]] / scale,
      adv__right_evidence_fraction = if (length(after)) {
        mean(after < y_values[[position]])
      } else NA_real_,
      adv__terminal_margin_over_amplitude = if (length(after)) {
        (y_values[[position]] - utils::tail(y_values, 1L)) / scale
      } else NA_real_,
      adv__near_max_member = as.integer(
        y_values[[position]] >= max(y_values) - near_max_tolerance * amplitude
      ),
      adv__topographic_prominence_over_amplitude =
        advanced_topographic_prominence(y_values, position) / scale,
      adv__left_segment_slope_over_amplitude = left_fit[["slope"]] / scale,
      adv__right_segment_slope_over_amplitude = right_fit[["slope"]] / scale,
      adv__segmented_r_squared = segmented_r_squared,
      adv__segmented_rmse_over_amplitude = if (is.finite(segmented_residual)) {
        sqrt(segmented_residual / length(y_values)) / scale
      } else NA_real_,
      adv__break_strength_over_amplitude =
        (left_fit[["slope"]] - right_fit[["slope"]]) / scale,
      adv__smoothed_peak_member = as.integer(candidate == smoothed_peak),
      adv__left_step_signal_to_noise = advanced_curve_safe_ratio(
        left_step, left_noise, NA_real_
      ),
      adv__right_drop_signal_to_noise = advanced_curve_safe_ratio(
        right_drop, right_noise, NA_real_
      ),
      adv__restart_peak_fraction = restart$peak_fraction[[position]],
      stringsAsFactors = FALSE
    )
  })
  local <- do.call(rbind, rows)
  global <- data.frame(
    adv__near_max_count = near_max[["count"]],
    adv__near_max_fraction = near_max[["fraction"]],
    adv__near_max_components = near_max[["components"]],
    adv__near_max_span = near_max[["span"]],
    adv__ambiguity_entropy = ambiguity_entropy,
    adv__effective_candidate_count = effective_candidates,
    adv__secondary_peak_height_ratio = secondary[["height_ratio"]],
    adv__secondary_peak_distance = secondary[["distance"]],
    adv__secondary_valley_depth_over_amplitude =
      secondary[["valley_depth_over_amplitude"]],
    adv__restart_peak_entropy = restart$peak_entropy,
    stringsAsFactors = FALSE
  )
  result <- cbind(
    local, global[rep(1L, length(candidates)), , drop = FALSE],
    stringsAsFactors = FALSE
  )
  rownames(result) <- NULL
  result
}
