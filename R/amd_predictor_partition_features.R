partition_safe_ratio <- function(numerator, denominator,
                                 fallback = 0) {
  valid <- is.finite(denominator) &
    abs(denominator) > .Machine$double.eps
  result <- rep_len(fallback, max(length(numerator), length(denominator)))
  numerator <- rep_len(numerator, length(result))
  denominator <- rep_len(denominator, length(result))
  valid <- rep_len(valid, length(result))
  result[valid] <- numerator[valid] / denominator[valid]
  result
}

partition_with_seed <- function(seed, expression) {
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

partition_quantile <- function(values, probability, fallback = NA_real_) {
  values <- values[is.finite(values)]
  if (!length(values)) return(fallback)
  as.numeric(stats::quantile(
    values, probability, names = FALSE, type = 8, na.rm = TRUE
  ))
}

partition_cv <- function(values, fallback = 0) {
  values <- values[is.finite(values)]
  if (length(values) < 2L || abs(mean(values)) <= .Machine$double.eps) {
    return(fallback)
  }
  stats::sd(values) / mean(values)
}

partition_adjusted_rand <- function(left, right) {
  contingency <- table(left, right)
  choose_two <- function(value) value * (value - 1) / 2
  observed <- sum(choose_two(contingency))
  row_pairs <- sum(choose_two(rowSums(contingency)))
  column_pairs <- sum(choose_two(colSums(contingency)))
  total_pairs <- choose_two(sum(contingency))
  if (total_pairs <= 0) return(1)
  expected <- row_pairs * column_pairs / total_pairs
  maximum <- (row_pairs + column_pairs) / 2
  if (abs(maximum - expected) <= .Machine$double.eps) {
    return(if (abs(observed - expected) <= .Machine$double.eps) 1 else 0)
  }
  (observed - expected) / (maximum - expected)
}

partition_normalized_mutual_information <- function(left, right) {
  contingency <- table(left, right)
  probabilities <- contingency / sum(contingency)
  left_probability <- rowSums(probabilities)
  right_probability <- colSums(probabilities)
  occupied <- which(probabilities > 0, arr.ind = TRUE)
  mutual_information <- sum(vapply(seq_len(nrow(occupied)), function(index) {
    row <- occupied[index, 1L]
    column <- occupied[index, 2L]
    probability <- probabilities[row, column]
    probability * log(
      probability / (left_probability[[row]] * right_probability[[column]])
    )
  }, numeric(1L)))
  entropy_left <- -sum(left_probability[left_probability > 0] *
    log(left_probability[left_probability > 0]))
  entropy_right <- -sum(right_probability[right_probability > 0] *
    log(right_probability[right_probability > 0]))
  denominator <- sqrt(entropy_left * entropy_right)
  if (denominator <= .Machine$double.eps) {
    return(as.numeric(identical(as.integer(left), as.integer(right))))
  }
  mutual_information / denominator
}

partition_silhouette_values <- function(data, cluster) {
  cluster <- as.integer(factor(cluster))
  group_count <- length(unique(cluster))
  if (group_count < 2L || nrow(data) < 3L) {
    return(rep(NA_real_, nrow(data)))
  }
  distances <- as.matrix(stats::dist(data))
  result <- rep(NA_real_, nrow(data))
  for (row in seq_len(nrow(data))) {
    own <- which(cluster == cluster[[row]])
    own <- setdiff(own, row)
    if (!length(own)) {
      result[[row]] <- 0
      next
    }
    within <- mean(distances[row, own])
    alternatives <- vapply(
      setdiff(sort(unique(cluster)), cluster[[row]]),
      function(group) mean(distances[row, cluster == group]),
      numeric(1L)
    )
    nearest <- min(alternatives)
    denominator <- max(within, nearest)
    result[[row]] <- if (denominator > 0) {
      (nearest - within) / denominator
    } else 0
  }
  result
}

partition_weighted_covariance <- function(data, weights, center) {
  weight_sum <- sum(weights)
  if (!is.finite(weight_sum) || weight_sum <= .Machine$double.eps) {
    return(matrix(NA_real_, ncol(data), ncol(data)))
  }
  centered <- sweep(data, 2L, center, FUN = "-")
  crossprod(centered * sqrt(weights)) / weight_sum
}

partition_covariance_shape <- function(covariance, tolerance = 1e-8) {
  if (any(!is.finite(covariance))) {
    return(c(log_condition = NA_real_, effective_dimension_fraction = NA_real_,
             rank_fraction = NA_real_))
  }
  eigenvalues <- pmax(
    eigen(covariance, symmetric = TRUE, only.values = TRUE)$values,
    0
  )
  maximum <- max(eigenvalues)
  if (maximum <= .Machine$double.eps) {
    return(c(log_condition = 0, effective_dimension_fraction = 0,
             rank_fraction = 0))
  }
  floor_value <- maximum * tolerance
  regularized <- pmax(eigenvalues, floor_value)
  effective_dimension <- sum(eigenvalues)^2 / sum(eigenvalues^2)
  c(
    log_condition = log(maximum / min(regularized)),
    effective_dimension_fraction = effective_dimension / length(eigenvalues),
    rank_fraction = mean(eigenvalues > floor_value)
  )
}

analytical_single_partition <- function(data) {
  membership <- matrix(1, nrow = nrow(data), ncol = 1L)
  centers <- matrix(colMeans(data), nrow = 1L)
  list(
    c = 1L,
    cluster = rep.int(1L, nrow(data)),
    membership = membership,
    centers = centers,
    objective = sum(rowSums(sweep(data, 2L, centers[1L, ], FUN = "-")^2)),
    selected_start = NA_integer_,
    selected_seed = NA_integer_,
    starts = data.frame(),
    source = "analytical_c1"
  )
}

describe_candidate_partition <- function(data, assignment, m = 2,
                                         ambiguity_gap = 0.1) {
  data <- as.matrix(data)
  storage.mode(data) <- "double"
  membership <- as.matrix(assignment$membership)
  centers <- as.matrix(assignment$centers)
  cluster <- as.integer(assignment$cluster)
  candidate <- ncol(membership)
  if (nrow(data) != nrow(membership) || nrow(data) != length(cluster) ||
      ncol(data) != ncol(centers) || nrow(centers) != candidate ||
      any(!is.finite(data)) || any(!is.finite(membership)) ||
      any(!is.finite(centers))) {
    stop("partition inputs are inconsistent or non-finite.", call. = FALSE)
  }
  row_totals <- rowSums(membership)
  if (any(abs(row_totals - 1) > 1e-6) || any(membership < -1e-12)) {
    stop("memberships must be non-negative and sum to one by row.",
         call. = FALSE)
  }

  observations <- nrow(data)
  dimensions <- ncol(data)
  soft_mass <- colSums(membership)
  soft_fraction <- soft_mass / observations
  size_entropy <- -sum(soft_fraction[soft_fraction > 0] *
    log(soft_fraction[soft_fraction > 0]))
  size_entropy <- if (candidate > 1L) size_entropy / log(candidate) else 0

  ordered_membership <- t(apply(membership, 1L, sort, decreasing = TRUE))
  winning_membership <- ordered_membership[, 1L]
  membership_gap <- if (candidate > 1L) {
    ordered_membership[, 1L] - ordered_membership[, 2L]
  } else rep.int(1, observations)
  observation_entropy <- -rowSums(ifelse(
    membership > 0, membership * log(membership), 0
  ))
  if (candidate > 1L) observation_entropy <- observation_entropy / log(candidate)
  else observation_entropy[] <- 0

  global_center <- colMeans(data)
  global_squared_distance <- rowSums(
    sweep(data, 2L, global_center, FUN = "-")^2
  )
  total_ss <- sum(global_squared_distance)
  global_rms <- sqrt(mean(global_squared_distance))
  fuzzy_weights <- membership^m
  group_rms <- rep(NA_real_, candidate)
  covariance_shape <- matrix(
    NA_real_, nrow = candidate, ncol = 3L,
    dimnames = list(NULL, c(
      "log_condition", "effective_dimension_fraction", "rank_fraction"
    ))
  )
  within_ss <- 0
  observation_selected_distance <- rep(NA_real_, observations)
  for (group in seq_len(candidate)) {
    centered <- sweep(data, 2L, centers[group, ], FUN = "-")
    squared_distance <- rowSums(centered^2)
    weight <- fuzzy_weights[, group]
    denominator <- sum(weight)
    within_ss <- within_ss + sum(weight * squared_distance)
    group_rms[[group]] <- if (denominator > .Machine$double.eps) {
      sqrt(sum(weight * squared_distance) / denominator)
    } else NA_real_
    covariance_shape[group, ] <- partition_covariance_shape(
      partition_weighted_covariance(data, weight, centers[group, ])
    )
    selected <- cluster == group
    observation_selected_distance[selected] <- sqrt(squared_distance[selected])
  }

  center_distance <- as.matrix(stats::dist(centers))
  center_pairs_available <- candidate > 1L
  if (center_pairs_available) {
    center_distance[lower.tri(center_distance, diag = TRUE)] <- NA_real_
    pair_index <- which(is.finite(center_distance), arr.ind = TRUE)
    raw_pair_distance <- center_distance[pair_index]
    pooled_pair_rms <- vapply(seq_len(nrow(pair_index)), function(index) {
      left <- pair_index[index, 1L]
      right <- pair_index[index, 2L]
      sqrt((group_rms[[left]]^2 + group_rms[[right]]^2) / 2)
    }, numeric(1L))
    standardized_pair_distance <- partition_safe_ratio(
      raw_pair_distance, pooled_pair_rms, NA_real_
    )
    full_center_distance <- as.matrix(stats::dist(centers))
    diag(full_center_distance) <- Inf
    nearest_center_distance <- apply(full_center_distance, 1L, min)
  } else {
    raw_pair_distance <- standardized_pair_distance <- numeric()
    nearest_center_distance <- numeric()
  }

  between_ss <- sum(soft_mass * rowSums(
    sweep(centers, 2L, global_center, FUN = "-")^2
  ))
  silhouette <- partition_silhouette_values(data, cluster)
  finite_silhouette <- silhouette[is.finite(silhouette)]

  convergence_fraction <- objective_cv <- amd_sd <- objective_gap <- NA_real_
  if (!is.null(assignment$starts) && nrow(assignment$starts)) {
    starts <- assignment$starts
    convergence_fraction <- mean(starts$converged)
    eligible <- which(starts$converged & is.finite(starts$objective))
    if (length(eligible)) {
      objectives <- starts$objective[eligible]
      objective_cv <- partition_cv(objectives, NA_real_)
      ordered <- sort(objectives)
      objective_gap <- if (length(ordered) >= 2L) {
        partition_safe_ratio(
          ordered[[2L]] - ordered[[1L]], abs(ordered[[1L]]), NA_real_
        )
      } else NA_real_
      amd_values <- starts$amd[eligible]
      amd_sd <- if (length(amd_values) >= 2L) stats::sd(amd_values) else 0
    }
  }

  data.frame(
    part__separation_available = as.integer(center_pairs_available),
    part__silhouette_available = as.integer(length(finite_silhouette) > 0L),
    part__minimum_soft_fraction = min(soft_fraction),
    part__maximum_soft_fraction = max(soft_fraction),
    part__soft_size_entropy = size_entropy,
    part__soft_size_cv = partition_cv(soft_fraction),
    part__mean_winning_membership = mean(winning_membership),
    part__q10_winning_membership = partition_quantile(
      winning_membership, 0.1
    ),
    part__mean_membership_entropy = mean(observation_entropy),
    part__q90_membership_entropy = partition_quantile(
      observation_entropy, 0.9
    ),
    part__mean_top_two_gap = mean(membership_gap),
    part__q10_top_two_gap = partition_quantile(membership_gap, 0.1),
    part__ambiguous_fraction = mean(membership_gap < ambiguity_gap),
    part__within_to_total = partition_safe_ratio(within_ss, total_ss),
    part__weighted_mean_rms_over_global = partition_safe_ratio(
      stats::weighted.mean(group_rms, soft_mass, na.rm = TRUE), global_rms
    ),
    part__maximum_rms_over_global = partition_safe_ratio(
      max(group_rms, na.rm = TRUE), global_rms
    ),
    part__group_rms_cv = partition_cv(group_rms),
    part__q90_assigned_distance_over_global = partition_safe_ratio(
      partition_quantile(observation_selected_distance, 0.9), global_rms
    ),
    part__minimum_center_distance_over_global = if (center_pairs_available) {
      partition_safe_ratio(min(raw_pair_distance), global_rms, NA_real_)
    } else NA_real_,
    part__mean_nearest_center_distance_over_global = if (
      center_pairs_available
    ) {
      partition_safe_ratio(mean(nearest_center_distance), global_rms, NA_real_)
    } else NA_real_,
    part__minimum_center_to_pooled_rms = if (center_pairs_available) {
      min(standardized_pair_distance, na.rm = TRUE)
    } else NA_real_,
    part__between_to_within = partition_safe_ratio(
      between_ss, within_ss, NA_real_
    ),
    part__mean_log_anisotropy = mean(
      covariance_shape[, "log_condition"], na.rm = TRUE
    ),
    part__maximum_log_anisotropy = max(
      covariance_shape[, "log_condition"], na.rm = TRUE
    ),
    part__anisotropy_cv = partition_cv(
      covariance_shape[, "log_condition"]
    ),
    part__mean_effective_dimension_fraction = mean(
      covariance_shape[, "effective_dimension_fraction"], na.rm = TRUE
    ),
    part__minimum_rank_fraction = min(
      covariance_shape[, "rank_fraction"], na.rm = TRUE
    ),
    part__mean_silhouette = if (length(finite_silhouette)) {
      mean(finite_silhouette)
    } else NA_real_,
    part__q10_silhouette = partition_quantile(
      finite_silhouette, 0.1, NA_real_
    ),
    part__negative_silhouette_fraction = if (length(finite_silhouette)) {
      mean(finite_silhouette < 0)
    } else NA_real_,
    part__converged_start_fraction = convergence_fraction,
    part__objective_cv_across_starts = objective_cv,
    part__relative_objective_gap = objective_gap,
    part__amd_sd_across_starts = amd_sd,
    stringsAsFactors = FALSE
  )
}

describe_partition_transition <- function(left_cluster, right_cluster,
                                          prefix) {
  left_cluster <- as.integer(factor(left_cluster))
  right_cluster <- as.integer(factor(right_cluster))
  if (length(left_cluster) != length(right_cluster)) {
    stop("transition partitions must have equal lengths.", call. = FALSE)
  }
  contingency <- table(left_cluster, right_cluster)
  child_consistency <- sum(apply(contingency, 2L, max)) / sum(contingency)
  fragmentation <- vapply(seq_len(nrow(contingency)), function(row) {
    counts <- contingency[row, ]
    if (!sum(counts)) return(0)
    1 - max(counts) / sum(counts)
  }, numeric(1L))
  fragmentation_mass <- as.numeric(rowSums(contingency)) * fragmentation
  concentration <- if (sum(fragmentation_mass) > 0) {
    max(fragmentation_mass) / sum(fragmentation_mass)
  } else 1
  result <- data.frame(
    adjusted_rand = partition_adjusted_rand(left_cluster, right_cluster),
    normalized_mutual_information =
      partition_normalized_mutual_information(left_cluster, right_cluster),
    reorganization_fraction = 1 - child_consistency,
    split_concentration = concentration,
    fragmented_parent_fraction = mean(fragmentation > 0.05),
    stringsAsFactors = FALSE
  )
  names(result) <- paste0(prefix, names(result))
  result
}

add_candidate_transition_features <- function(assignments, candidates) {
  if (!identical(as.integer(names(assignments)), as.integer(candidates))) {
    stop("assignments must be named and ordered by candidate.", call. = FALSE)
  }
  rows <- vector("list", length(candidates))
  for (index in seq_along(candidates)) {
    left_available <- index > 1L
    right_available <- index < length(candidates)
    left <- if (left_available) {
      describe_partition_transition(
        assignments[[index - 1L]]$cluster,
        assignments[[index]]$cluster,
        "trans__from_left_"
      )
    } else data.frame()
    right <- if (right_available) {
      describe_partition_transition(
        assignments[[index]]$cluster,
        assignments[[index + 1L]]$cluster,
        "trans__to_right_"
      )
    } else data.frame()
    missing_block <- function(prefix) {
      values <- as.data.frame(as.list(rep(NA_real_, 5L)))
      names(values) <- paste0(prefix, c(
        "adjusted_rand", "normalized_mutual_information",
        "reorganization_fraction", "split_concentration",
        "fragmented_parent_fraction"
      ))
      values
    }
    if (!left_available) left <- missing_block("trans__from_left_")
    if (!right_available) right <- missing_block("trans__to_right_")
    rows[[index]] <- cbind(
      data.frame(
        trans__left_available = as.integer(left_available),
        trans__right_available = as.integer(right_available)
      ),
      left, right, stringsAsFactors = FALSE
    )
  }
  result <- do.call(rbind, rows)
  rownames(result) <- NULL
  result
}

describe_partition_refit_stability <- function(data, reference_cluster,
                                               candidate, seeds,
                                               iter_max = 2000L, m = 2) {
  if (candidate == 1L) {
    return(data.frame(
      stab__available = 0L,
      stab__successful_refits = 0L,
      stab__mean_ari_to_selected = NA_real_,
      stab__minimum_ari_to_selected = NA_real_,
      stab__mean_pairwise_ari = NA_real_,
      stringsAsFactors = FALSE
    ))
  }
  fits <- lapply(as.integer(seeds), function(seed) {
    fit <- tryCatch(
      partition_with_seed(seed, e1071::cmeans(
        data, centers = candidate, iter.max = iter_max,
        verbose = FALSE, m = m
      )),
      error = function(error) NULL
    )
    if (is.null(fit) || fit$iter >= iter_max || !is.finite(fit$withinerror)) {
      return(NULL)
    }
    as.integer(fit$cluster)
  })
  fits <- Filter(Negate(is.null), fits)
  if (!length(fits)) {
    return(data.frame(
      stab__available = 0L,
      stab__successful_refits = 0L,
      stab__mean_ari_to_selected = NA_real_,
      stab__minimum_ari_to_selected = NA_real_,
      stab__mean_pairwise_ari = NA_real_,
      stringsAsFactors = FALSE
    ))
  }
  ari_reference <- vapply(
    fits, partition_adjusted_rand, numeric(1L), right = reference_cluster
  )
  pairwise <- if (length(fits) >= 2L) {
    pairs <- utils::combn(seq_along(fits), 2L)
    vapply(seq_len(ncol(pairs)), function(index) {
      partition_adjusted_rand(
        fits[[pairs[1L, index]]], fits[[pairs[2L, index]]]
      )
    }, numeric(1L))
  } else ari_reference
  data.frame(
    stab__available = 1L,
    stab__successful_refits = length(fits),
    stab__mean_ari_to_selected = mean(ari_reference),
    stab__minimum_ari_to_selected = min(ari_reference),
    stab__mean_pairwise_ari = mean(pairwise),
    stringsAsFactors = FALSE
  )
}

