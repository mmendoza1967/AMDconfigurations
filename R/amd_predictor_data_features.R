summarize_vector <- function(values, prefix) {
  values <- values[is.finite(values)]
  if (!length(values)) {
    result <- rep(NA_real_, 7L)
  } else {
    result <- c(
      min = min(values), q25 = unname(stats::quantile(values, 0.25)),
      median = stats::median(values), mean = mean(values),
      q75 = unname(stats::quantile(values, 0.75)), max = max(values),
      sd = if (length(values) > 1L) stats::sd(values) else 0
    )
  }
  stats::setNames(as.list(result), paste0(prefix, "__", names(result)))
}

standardized_moment <- function(values, order) {
  centered <- values - mean(values)
  scale <- sqrt(mean(centered^2))
  if (!is.finite(scale) || scale <= .Machine$double.eps) return(NA_real_)
  mean((centered / scale)^order)
}

describe_unlabelled_data <- function(data, distance_limit = 2000L,
                                     seed = 1L) {
  if (!is.matrix(data) || !is.numeric(data) || any(!is.finite(data))) {
    stop("data must be a finite numeric matrix.", call. = FALSE)
  }
  n <- nrow(data)
  dimensions <- ncol(data)
  centered <- sweep(data, 2L, colMeans(data), FUN = "-")
  covariance <- crossprod(centered) / max(1, n - 1L)
  eigenvalues <- pmax(eigen(covariance, symmetric = TRUE,
                            only.values = TRUE)$values, 0)
  total_variance <- sum(eigenvalues)
  proportions <- if (total_variance > 0) eigenvalues / total_variance else
    rep(NA_real_, length(eigenvalues))
  positive <- eigenvalues[eigenvalues > max(eigenvalues, 0) * 1e-12]
  condition <- if (length(positive) > 1L) max(positive) / min(positive) else 1
  spectral_entropy <- if (all(is.finite(proportions)) && total_variance > 0) {
    nonzero <- proportions[proportions > 0]
    -sum(nonzero * log(nonzero))
  } else {
    NA_real_
  }
  effective_dimension <- if (is.finite(spectral_entropy)) {
    exp(spectral_entropy)
  } else {
    NA_real_
  }

  standard_deviations <- apply(data, 2L, stats::sd)
  skewness <- apply(data, 2L, standardized_moment, order = 3L)
  excess_kurtosis <- apply(data, 2L, standardized_moment, order = 4L) - 3
  variable_ranges <- apply(data, 2L, function(x) diff(range(x)))

  usable <- standard_deviations > .Machine$double.eps
  off_diagonal <- numeric()
  if (sum(usable) >= 2L) {
    correlation <- stats::cor(data[, usable, drop = FALSE])
    off_diagonal <- correlation[upper.tri(correlation)]
  }

  sampled <- data
  sampled_n <- n
  if (n > distance_limit) {
    sampled_n <- as.integer(distance_limit)
    sampled <- .amd_with_seed(seed, {
      data[sample.int(n, sampled_n), , drop = FALSE]
    })
  }
  distance_object <- stats::dist(sampled)
  distances <- as.numeric(distance_object)
  distance_matrix <- as.matrix(distance_object)
  diag(distance_matrix) <- Inf
  nearest <- apply(distance_matrix, 1L, min)
  second_nearest <- apply(distance_matrix, 1L, function(row) {
    sort(row, partial = 2L)[[2L]]
  })

  base <- list(
    data__N_over_D = n / dimensions,
    data__D_over_N = dimensions / n,
    data__total_variance = total_variance,
    data__mean_variance = total_variance / dimensions,
    data__covariance_condition = condition,
    data__log_covariance_condition = log(condition),
    data__pc1_fraction = if (length(proportions)) proportions[[1L]] else NA_real_,
    data__pc2_cumulative_fraction = if (length(proportions)) {
      sum(utils::head(proportions, min(2L, length(proportions))))
    } else {
      NA_real_
    },
    data__spectral_entropy = spectral_entropy,
    data__effective_dimension = effective_dimension,
    data__effective_dimension_fraction = effective_dimension / dimensions,
    data__zero_variance_fraction = mean(!usable),
    data__mean_absolute_correlation = if (length(off_diagonal)) {
      mean(abs(off_diagonal))
    } else {
      0
    },
    data__max_absolute_correlation = if (length(off_diagonal)) {
      max(abs(off_diagonal))
    } else {
      0
    },
    data__distance_sample_n = sampled_n,
    data__pairwise_distance_cv = if (mean(distances) > 0) {
      stats::sd(distances) / mean(distances)
    } else {
      NA_real_
    },
    data__distance_contrast_q95_q05 = {
      quantiles <- stats::quantile(distances, c(0.05, 0.95))
      if (quantiles[[1L]] > 0) quantiles[[2L]] / quantiles[[1L]] else NA_real_
    },
    data__nearest_over_median_pairwise = if (stats::median(distances) > 0) {
      stats::median(nearest) / stats::median(distances)
    } else {
      NA_real_
    }
  )
  summaries <- c(
    summarize_vector(standard_deviations, "data__sd"),
    summarize_vector(variable_ranges, "data__range"),
    summarize_vector(abs(skewness), "data__absolute_skewness"),
    summarize_vector(excess_kurtosis, "data__excess_kurtosis"),
    summarize_vector(abs(off_diagonal), "data__absolute_correlation"),
    summarize_vector(distances, "data__pairwise_distance"),
    summarize_vector(nearest, "data__nearest_distance"),
    summarize_vector(second_nearest, "data__second_nearest_distance")
  )
  as.data.frame(c(base, summaries), check.names = FALSE)
}
