#' Describe an AMD curve and its stored partitions
#'
#' Summarise the primary curve, memberships and hard assignments without
#' fitting new models, selecting a number of groups or changing the input.
#' The selected fits must be those saved by [compute_amd_curve()].
#'
#' @param object An `amd_curve` with a primary curve and a selected fit for
#'   every evaluated candidate. Legacy 0.1.0 objects are not sufficient.
#' @param ambiguity_gap A number in `[0, 1]`. An observation is counted as
#'   ambiguous when its largest membership minus its second-largest membership
#'   is strictly less than this value. The default, 0.1, is a descriptive
#'   convention, not a validated universal decision threshold.
#'
#' @return An object of class `amd_diagnostics` with:
#' \describe{
#'   \item{candidates}{One row per evaluated `c`, describing curve neighbours,
#'     winning memberships, normalised membership entropy, top-two gaps and
#'     hard and soft group-size balance. Boundary local maxima are `NA`, since
#'     one neighbour is missing. A local maximum here means strictly greater
#'     than both evaluated neighbours; plateaus are not classified as peaks.}
#'   \item{groups}{One row per membership column at each `c`. Hard sizes use
#'     the stored winning assignment. Soft masses are sums of memberships,
#'     not of memberships raised to the fuzziness parameter. Summaries within
#'     a hard group are `NA` when that group is empty.}
#'   \item{transitions}{ARI, geometric-mean normalised mutual information (NMI)
#'     and directional fragmentation between successive evaluated candidates.
#'     Fragmentation is the fraction outside each group's most frequent group
#'     in the other partition. `from_fragmentation` describes splitting of
#'     source groups; `to_fragmentation` describes mixing of source groups in
#'     destination groups. These are descriptive overlaps, not evidence of
#'     physical splitting, merging or biological stability.}
#'   \item{curve_summary}{Numerical maximum, exact ties, boundary status and
#'     the evaluated range. These do not constitute a scientific choice of K.}
#'   \item{dictionary}{Definitions and scales for every table column.}
#'   \item{unavailable}{Diagnostics not computed by this operation and why.}
#'   \item{complete_rows, omitted_rows}{The original observation row map.}
#'   \item{settings}{Diagnostic conventions and provenance.}
#' }
#'
#' @details
#' Normalised observation entropy is `-sum(u * log(u)) / log(c)`, with
#' `0 * log(0)` interpreted as zero. Size entropy applies the same formula to
#' group fractions. Quantiles use [stats::quantile()] with `type = 8`.
#' Exact top-membership ties are reported separately and the stored assignment
#' is retained; no new tie-breaking or random-number generation takes place.
#'
#' Transitions compare the same observations, not the numerical group labels.
#' ARI and NMI are invariant to relabelling. Identical single-group partitions
#' have ARI and NMI one even if their occupied membership columns differ. NMI
#' is zero when only one of the partitions has a single occupied group.
#'
#' This function uses memberships and assignments only. It does not calculate
#' sample-to-sample distances, silhouette, geometric compactness, density,
#' connectivity or resampling stability, and does not require the original
#' data. It does not impute missing descriptive values or call a supervised
#' reader. Its working arrays grow linearly with the number of observations
#' per candidate, plus contingency tables between occupied groups; it does not
#' construct an observation-by-observation distance matrix.
#'
#' @seealso [compute_amd_curve()], [assign_configurations()], [amd_score()]
#' @examples
#' set.seed(42)
#' X <- rbind(matrix(rnorm(40, -2), ncol = 2),
#'            matrix(rnorm(40, 2), ncol = 2))
#' curve <- compute_amd_curve(X, its = 2, nin = 2, nsp = 3,
#'                            seeds = 1:4, iter_max = 1000, verbose = FALSE)
#' diagnostics <- diagnose_amd(curve)
#' diagnostics$candidates
#' diagnostics$transitions
#' # Inspect the curve and diagnostics before making an explicit choice.
#' assigned <- assign_configurations(curve, c = 2)
#' @export
diagnose_amd <- function(object, ambiguity_gap = 0.1) {
  if (.amd_curve_schema(object) != "current" ||
      is.null(object$selected_fits)) {
    stop("diagnose_amd requires a current amd_curve with selected_fits; ",
         "recompute legacy or incomplete curves.", call. = FALSE)
  }
  if (!is.numeric(ambiguity_gap) || length(ambiguity_gap) != 1L ||
      !is.finite(ambiguity_gap) || ambiguity_gap < 0 || ambiguity_gap > 1) {
    stop("ambiguity_gap must be one finite number in [0, 1].", call. = FALSE)
  }
  values <- object$curve
  ks <- suppressWarnings(as.numeric(names(values)))
  if (!is.numeric(values) || !length(values) || any(!is.finite(values)) ||
      length(ks) != length(values) || any(!is.finite(ks)) ||
      any(ks < 2 | ks != floor(ks) | ks > .Machine$integer.max) ||
      is.unsorted(ks, strictly = TRUE) ||
      !is.list(object$selected_fits) ||
      anyDuplicated(names(object$selected_fits)) ||
      !all(names(values) %in% names(object$selected_fits))) {
    stop("the primary curve must have unique, increasing integer c names ",
         "and matching selected_fits.", call. = FALSE)
  }
  ks <- as.integer(ks)
  n <- nrow(object$selected_fits[[names(values)[[1L]]]]$membership)
  .amd_diagnostic_row_map(object, n)

  candidate_rows <- group_rows <- vector("list", length(ks))
  transitions <- vector("list", max(0L, length(ks) - 1L))
  previous_cluster <- previous_row_names <- NULL
  for (index in seq_along(ks)) {
    candidate <- ks[[index]]
    fit <- object$selected_fits[[names(values)[[index]]]]
    U <- fit$membership
    cluster <- fit$cluster
    .amd_diagnostic_validate_fit(U, cluster, candidate, n)
    if (index > 1L && !identical(rownames(U), previous_row_names)) {
      stop("membership row names must agree across selected fits.",
           call. = FALSE)
    }

    winning <- second <- rep(0, n)
    entropy <- rep(0, n)
    for (group in seq_len(candidate)) {
      u <- U[, group]
      second <- pmax(second, pmin(winning, u))
      winning <- pmax(winning, u)
      positive <- u > 0
      entropy[positive] <- entropy[positive] - u[positive] * log(u[positive])
    }
    entropy <- entropy / log(candidate)
    gap <- winning - second
    assigned_membership <- U[cbind(seq_len(n), cluster)]
    if (any(winning - assigned_membership > sqrt(.Machine$double.eps))) {
      stop("stored hard assignments must select a winning membership.",
           call. = FALSE)
    }
    if (abs(mean(winning) - 1 / candidate - values[[index]]) >
        sqrt(.Machine$double.eps)) {
      stop("a primary AMD value does not match its stored memberships.",
           call. = FALSE)
    }
    hard_n <- tabulate(cluster, nbins = candidate)
    hard_fraction <- hard_n / n
    soft_mass <- colSums(U)
    soft_fraction <- soft_mass / sum(soft_mass)
    candidate_rows[[index]] <- data.frame(
      c = candidate, amd = unname(values[[index]]),
      mean_winning_membership = mean(winning),
      q10_winning_membership = .amd_diagnostic_quantile(winning, 0.1),
      median_winning_membership = .amd_diagnostic_quantile(winning, 0.5),
      mean_membership_entropy = mean(entropy),
      q90_membership_entropy = .amd_diagnostic_quantile(entropy, 0.9),
      mean_top_two_gap = mean(gap),
      q10_top_two_gap = .amd_diagnostic_quantile(gap, 0.1),
      ambiguous_fraction = mean(gap < ambiguity_gap),
      tied_winner_fraction = mean(gap == 0),
      minimum_hard_fraction = min(hard_fraction),
      maximum_hard_fraction = max(hard_fraction),
      hard_size_entropy = .amd_diagnostic_entropy(hard_fraction),
      empty_hard_groups = sum(hard_n == 0),
      minimum_soft_fraction = min(soft_fraction),
      maximum_soft_fraction = max(soft_fraction),
      soft_size_entropy = .amd_diagnostic_entropy(soft_fraction)
    )
    group_rows[[index]] <- data.frame(
      c = candidate, group = seq_len(candidate),
      hard_n = hard_n, hard_fraction = hard_fraction,
      soft_mass = unname(soft_mass), soft_fraction = unname(soft_fraction),
      mean_winning_membership = vapply(seq_len(candidate), function(group) {
        if (hard_n[[group]] == 0) return(NA_real_)
        mean(winning[cluster == group])
      }, numeric(1)),
      mean_membership_entropy = vapply(seq_len(candidate), function(group) {
        if (hard_n[[group]] == 0) return(NA_real_)
        mean(entropy[cluster == group])
      }, numeric(1))
    )
    if (index > 1L) {
      overlaps <- table(previous_cluster, cluster)
      transitions[[index - 1L]] <- data.frame(
        from_c = ks[[index - 1L]], to_c = candidate,
        c_step = candidate - ks[[index - 1L]],
        amd_change = unname(values[[index]] - values[[index - 1L]]),
        ari = .amd_diagnostic_ari(overlaps),
        nmi = .amd_diagnostic_nmi(overlaps),
        from_fragmentation = 1 - sum(apply(overlaps, 1L, max)) / n,
        to_fragmentation = 1 - sum(apply(overlaps, 2L, max)) / n
      )
    }
    previous_cluster <- cluster
    previous_row_names <- rownames(U)
  }
  candidates <- do.call(rbind, candidate_rows)
  delta <- diff(unname(values))
  candidates$delta_previous <- c(NA_real_, delta)
  candidates$drop_next <- c(-delta, NA_real_)
  candidates$at_boundary <- seq_along(ks) %in% c(1L, length(ks))
  candidates$local_maximum <- ifelse(candidates$at_boundary, NA,
    candidates$delta_previous > 0 & candidates$drop_next > 0)
  candidates$at_numerical_maximum <- unname(values) == max(values)
  transition_table <- if (length(transitions)) do.call(rbind, transitions) else {
    data.frame(from_c = integer(), to_c = integer(), c_step = integer(),
      amd_change = numeric(), ari = numeric(), nmi = numeric(),
      from_fragmentation = numeric(), to_fragmentation = numeric())
  }
  maxima <- ks[candidates$at_numerical_maximum]
  out <- list(
    candidates = candidates,
    groups = do.call(rbind, group_rows),
    transitions = transition_table,
    curve_summary = list(candidate_c = ks, numerical_peak_c = min(maxima),
      exact_maximum_candidates = maxima, maximum_amd = max(values),
      amd_range = range(unname(values)),
      maximum_at_boundary = any(candidates$at_boundary &
                                 candidates$at_numerical_maximum)),
    dictionary = .amd_diagnostic_dictionary(),
    unavailable = data.frame(
      diagnostic = c("geometric_compactness", "density_or_connectivity",
                     "resampling_stability", "supervised_K_and_certainty"),
      reason = c("Not computed; requires original data and a separate definition.",
        "Not measured by memberships or overlaps alone.",
        "No resampling performed; random starts are not sample replicates.",
        "No supervised reader is called by this descriptive operation.")
    ),
    complete_rows = object$complete_rows,
    omitted_rows = object$omitted_rows,
    settings = list(schema_version = 1L, ambiguity_gap = ambiguity_gap,
      quantile_type = 8L, n_observations = n,
      numerical_tolerance = sqrt(.Machine$double.eps),
      source_manifest_id = object$manifest_id,
      backend = object$backend, selection_rule = object$selection_rule,
      preprocessing = object$settings, new_fits = 0L,
      sample_distance_matrix = FALSE)
  )
  class(out) <- "amd_diagnostics"
  out
}

.amd_diagnostic_row_map <- function(object, n) {
  complete <- object$complete_rows
  omitted <- object$omitted_rows
  valid_indices <- function(rows) is.numeric(rows) && all(is.finite(rows)) &&
    all(rows >= 1 & rows == floor(rows)) && !anyDuplicated(rows)
  if (is.null(n) || length(n) != 1L || n < 1L ||
      !valid_indices(complete) || !valid_indices(omitted) ||
      length(complete) != n || length(intersect(complete, omitted)) ||
      !setequal(c(complete, omitted), seq_len(n + length(omitted)))) {
    stop("the complete_rows and omitted_rows maps are missing or inconsistent.",
         call. = FALSE)
  }
}

.amd_diagnostic_validate_fit <- function(U, cluster, candidate, n) {
  if (!is.matrix(U) || !is.numeric(U) || !identical(dim(U), c(n, candidate)) ||
      any(!is.finite(U)) || any(U < 0 | U > 1) ||
      any(abs(rowSums(U) - 1) > sqrt(.Machine$double.eps))) {
    stop("selected memberships must be finite probabilities with matching ",
         "dimensions and row sums of one.", call. = FALSE)
  }
  if (!is.numeric(cluster) || length(cluster) != n ||
      any(!is.finite(cluster)) ||
      any(cluster < 1 | cluster > candidate | cluster != floor(cluster))) {
    stop("stored hard assignments must index the membership columns.",
         call. = FALSE)
  }
}

.amd_diagnostic_quantile <- function(values, probability) {
  unname(stats::quantile(values, probability, type = 8))
}

.amd_diagnostic_entropy <- function(fractions) {
  positive <- fractions[fractions > 0]
  -sum(positive * log(positive)) / log(length(fractions))
}

.amd_diagnostic_ari <- function(overlaps) {
  pairs <- function(x) {
    x <- as.double(x)
    x * (x - 1) / 2
  }
  total <- pairs(sum(overlaps))
  if (total == 0) return(1)
  observed <- sum(pairs(overlaps))
  expected <- sum(pairs(rowSums(overlaps))) *
    sum(pairs(colSums(overlaps))) / total
  upper <- (sum(pairs(rowSums(overlaps))) +
              sum(pairs(colSums(overlaps)))) / 2
  if (upper == expected) return(1)
  (observed - expected) / (upper - expected)
}

.amd_diagnostic_nmi <- function(overlaps) {
  p <- overlaps / sum(overlaps)
  left <- rowSums(p)
  right <- colSums(p)
  entropy <- function(x) -sum(x[x > 0] * log(x[x > 0]))
  h_left <- entropy(left)
  h_right <- entropy(right)
  if (h_left == 0 && h_right == 0) return(1)
  if (h_left == 0 || h_right == 0) return(0)
  independent <- outer(left, right)
  positive <- p > 0
  sum(p[positive] * log(p[positive] / independent[positive])) /
    sqrt(h_left * h_right)
}

.amd_diagnostic_dictionary <- function() {
  candidate <- c(
    c = "Evaluated candidate count.",
    amd = "Mean winning membership minus 1/c; published, unnormalised AMD.",
    mean_winning_membership = "Mean largest membership across observations.",
    q10_winning_membership = "Tenth percentile of largest memberships (type 8).",
    median_winning_membership = "Median largest membership (type 8).",
    mean_membership_entropy = "Mean -sum(u log(u))/log(c) across observations.",
    q90_membership_entropy = "Ninetieth percentile of normalised observation entropy (type 8).",
    mean_top_two_gap = "Mean difference between largest and second-largest memberships.",
    q10_top_two_gap = "Tenth percentile of the top-two membership gap (type 8).",
    ambiguous_fraction = "Fraction with top-two gap strictly below settings$ambiguity_gap.",
    tied_winner_fraction = "Fraction with an exactly zero top-two gap.",
    minimum_hard_fraction = "Smallest group fraction under the stored winning assignment; includes empty groups.",
    maximum_hard_fraction = "Largest group fraction under the stored winning assignment.",
    hard_size_entropy = "Normalised entropy of hard group fractions: 0 concentrated, 1 balanced.",
    empty_hard_groups = "Number of membership columns with no hard-assigned observations.",
    minimum_soft_fraction = "Smallest membership-column sum divided by total membership mass.",
    maximum_soft_fraction = "Largest membership-column sum divided by total membership mass.",
    soft_size_entropy = "Normalised entropy of soft group fractions: 0 concentrated, 1 balanced.",
    delta_previous = "AMD(c) minus AMD at the previous evaluated candidate; NA at the first.",
    drop_next = "AMD(c) minus AMD at the next evaluated candidate; NA at the last.",
    at_boundary = "TRUE at the first or last evaluated candidate.",
    local_maximum = "Strictly above both evaluated neighbours; NA at boundaries, FALSE on plateaus.",
    at_numerical_maximum = "AMD equals the highest evaluated value; includes all exact ties."
  )
  group <- c(
    c = "Evaluated candidate count.",
    group = "Column index in the stored membership matrix; labels are arbitrary.",
    hard_n = "Number of observations assigned to this membership column.",
    hard_fraction = "Hard count divided by the fitted observation count.",
    soft_mass = "Sum of memberships in this column; no fuzziness power applied.",
    soft_fraction = "Soft mass divided by total membership mass.",
    mean_winning_membership = "Mean winning membership among hard-assigned observations; NA if empty.",
    mean_membership_entropy = "Mean normalised observation entropy within the hard group; NA if empty."
  )
  transition <- c(
    from_c = "Previous evaluated candidate count.",
    to_c = "Next evaluated candidate count.",
    c_step = "to_c minus from_c; gaps are not silently treated as unit steps.",
    amd_change = "AMD(to_c) minus AMD(from_c).",
    ari = "Adjusted Rand index of hard assignments; label invariant, can be negative.",
    nmi = "Mutual information divided by sqrt(H(from)*H(to)); label invariant.",
    from_fragmentation = "1 - sum(row maxima of overlap table)/N; fragmentation of source groups.",
    to_fragmentation = "1 - sum(column maxima of overlap table)/N; source mixing in destination groups."
  )
  make_rows <- function(definitions, table_name) {
    fields <- names(definitions)
    scale <- rep("[0, 1]", length(fields))
    scale[fields %in% c("c", "group", "hard_n", "empty_hard_groups",
                        "from_c", "to_c", "c_step")] <- "count"
    scale[fields == "soft_mass"] <- "membership mass"
    scale[fields == "amd"] <- "[0, 1 - 1/c]"
    scale[fields %in% c("delta_previous", "drop_next", "amd_change")] <- "AMD difference"
    scale[fields == "ari"] <- "[-1, 1]"
    scale[fields %in% c("at_boundary", "local_maximum",
                        "at_numerical_maximum")] <- "logical"
    data.frame(table = table_name, variable = fields,
               definition = unname(definitions), scale = scale)
  }
  rbind(make_rows(candidate, "candidates"), make_rows(group, "groups"),
        make_rows(transition, "transitions"))
}

#' Print descriptive AMD diagnostics
#' @param x An object returned by [diagnose_amd()].
#' @param ... Additional arguments passed to [print.data.frame()].
#' @return Invisibly returns `x`.
#' @export
print.amd_diagnostics <- function(x, ...) {
  cat("AMD curve and stored-partition diagnostics\n")
  cat("Observations:", x$settings$n_observations, "\n")
  cat("Numerical maximum at c =",
      paste(x$curve_summary$exact_maximum_candidates, collapse = ", "), "\n")
  cat("Descriptive only: no automatic K or calibrated certainty.\n")
  cat("No new fits, resampling or sample distance matrix.\n\n")
  print(x$candidates[, c("c", "amd", "mean_winning_membership",
    "mean_membership_entropy", "ambiguous_fraction", "local_maximum")],
    row.names = FALSE, ...)
  invisible(x)
}
