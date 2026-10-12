diagnostic_curve_fixture <- function(memberships, clusters = NULL) {
  ks <- vapply(memberships, ncol, integer(1))
  names(memberships) <- as.character(ks)
  if (is.null(clusters)) {
    clusters <- lapply(memberships, max.col, ties.method = "first")
  }
  fits <- Map(function(U, labels) list(membership = U, cluster = labels),
              memberships, clusters)
  values <- vapply(memberships, amd_score, numeric(1))
  peak <- ks[[which.max(values)]]
  structure(list(
    schema_version = 2L, curve = values, mean = values, max = values,
    c_peak = peak, peak_at_boundary = peak %in% range(ks),
    selected_fits = fits,
    selected_objectives = setNames(rep(1, length(ks)), names(values)),
    selected_starts = setNames(rep(1L, length(ks)), names(values)),
    converged = matrix(TRUE, nrow = 1, ncol = length(ks)),
    complete_rows = seq_len(nrow(memberships[[1L]])),
    omitted_rows = integer(), manifest_id = "test_manifest",
    backend = "test_memberships", selection_rule = "stored_test_fits",
    settings = list(scaled = FALSE)
  ), class = "amd_curve")
}

diagnostic_split_fixture <- function() {
  two <- rbind(c(.9, .1), c(.9, .1), c(.1, .9), c(.1, .9))
  three <- rbind(c(.9, .05, .05), c(.9, .05, .05),
                 c(.05, .9, .05), c(.05, .05, .9))
  diagnostic_curve_fixture(list(two, three))
}

test_that("membership summaries agree with hand calculations", {
  x <- diagnostic_split_fixture()
  d <- diagnose_amd(x)
  expect_s3_class(d, "amd_diagnostics")
  expect_equal(d$candidates$amd, c(.4, .9 - 1/3))
  expect_equal(d$candidates$mean_winning_membership, c(.9, .9))
  expect_equal(d$candidates$q10_winning_membership, c(.9, .9))
  expect_equal(d$candidates$median_winning_membership, c(.9, .9))
  expect_equal(d$candidates$mean_top_two_gap, c(.8, .85))
  expect_equal(d$candidates$q10_top_two_gap, c(.8, .85))
  entropy_two <- -sum(c(.9, .1) * log(c(.9, .1))) / log(2)
  entropy_three <- -sum(c(.9, .05, .05) * log(c(.9, .05, .05))) / log(3)
  expect_equal(d$candidates$mean_membership_entropy,
               c(entropy_two, entropy_three))
  expect_equal(d$candidates$q90_membership_entropy,
               c(entropy_two, entropy_three))
  expect_equal(d$candidates$ambiguous_fraction, c(0, 0))
  expect_equal(d$candidates$tied_winner_fraction, c(0, 0))
  expect_equal(d$candidates$hard_size_entropy[[1L]], 1)
  expect_equal(d$candidates$minimum_hard_fraction, c(.5, .25))
  expect_equal(d$candidates$maximum_hard_fraction, c(.5, .5))
  expect_equal(d$groups$hard_n, c(2L, 2L, 2L, 1L, 1L))
  expect_equal(d$groups$soft_mass, c(2, 2, 1.9, 1.05, 1.05))
  expect_equal(d$groups$soft_fraction, d$groups$soft_mass / 4)
  expect_equal(d$transitions$ari, 4/7)
  expect_equal(d$transitions$nmi, sqrt(2/3))
  expect_equal(d$transitions$from_fragmentation, .25)
  expect_equal(d$transitions$to_fragmentation, 0)
  expect_equal(d$transitions$amd_change, 1/6)
})

test_that("all output columns have definitions and scales", {
  d <- diagnose_amd(diagnostic_split_fixture())
  for (table_name in c("candidates", "groups", "transitions")) {
    rows <- d$dictionary[d$dictionary$table == table_name, ]
    expect_setequal(rows$variable, names(d[[table_name]]))
    expect_false(anyDuplicated(rows$variable) > 0)
    expect_true(all(nzchar(rows$definition)))
    expect_true(all(nzchar(rows$scale)))
  }
  expect_identical(d$settings$quantile_type, 8L)
  expect_identical(d$settings$new_fits, 0L)
  expect_false(d$settings$sample_distance_matrix)
  expect_match(d$unavailable$reason[[3L]], "not sample replicates")
})

test_that("transitions are invariant to independent group relabelling", {
  original <- diagnostic_split_fixture()
  relabelled <- original
  for (key in names(relabelled$selected_fits)) {
    fit <- relabelled$selected_fits[[key]]
    permutation <- rev(seq_len(ncol(fit$membership)))
    fit$membership <- fit$membership[, permutation, drop = FALSE]
    fit$cluster <- match(fit$cluster, permutation)
    relabelled$selected_fits[[key]] <- fit
  }
  d1 <- diagnose_amd(original)
  d2 <- diagnose_amd(relabelled)
  expect_equal(d1$transitions, d2$transitions)
  expect_equal(d1$candidates, d2$candidates)
})

test_that("uniform memberships, exact ties and empty hard groups are explicit", {
  U <- matrix(.5, nrow = 5, ncol = 2)
  x <- diagnostic_curve_fixture(list(U))
  x$selected_fits[[1L]]$cluster <- rep(2L, 5)
  d <- diagnose_amd(x)
  expect_equal(d$candidates$amd, 0)
  expect_equal(d$candidates$mean_membership_entropy, 1)
  expect_equal(d$candidates$mean_top_two_gap, 0)
  expect_equal(d$candidates$tied_winner_fraction, 1)
  expect_equal(d$candidates$ambiguous_fraction, 1)
  expect_equal(d$candidates$empty_hard_groups, 1)
  expect_equal(d$candidates$hard_size_entropy, 0)
  expect_equal(d$candidates$soft_size_entropy, 1)
  expect_true(is.na(d$groups$mean_winning_membership[[1L]]))
  expect_true(is.na(d$groups$mean_membership_entropy[[1L]]))
  expect_equal(d$groups$hard_n, c(0, 5))
  expect_equal(nrow(d$transitions), 0)
  expect_true(is.na(d$candidates$local_maximum))
  expect_equal(diagnose_amd(x, ambiguity_gap = 0)$candidates$ambiguous_fraction, 0)
})

test_that("crisp memberships attain the published bound without normalisation", {
  U <- diag(3)[c(1, 1, 2, 2, 3, 3), ]
  d <- diagnose_amd(diagnostic_curve_fixture(list(U)))
  expect_equal(d$candidates$amd, 2/3)
  expect_equal(d$candidates$mean_winning_membership, 1)
  expect_equal(d$candidates$mean_membership_entropy, 0)
  expect_equal(d$candidates$mean_top_two_gap, 1)
  expect_equal(d$candidates$hard_size_entropy, 1)
  expect_equal(d$candidates$soft_size_entropy, 1)
  expect_equal(d$candidates$ambiguous_fraction, 0)
})

test_that("degenerate similarities are label invariant", {
  two <- matrix(c(.9, .1), nrow = 6, ncol = 2, byrow = TRUE)
  three <- matrix(c(.05, .05, .9), nrow = 6, ncol = 3, byrow = TRUE)
  d <- diagnose_amd(diagnostic_curve_fixture(list(two, three)))
  expect_equal(d$transitions$ari, 1)
  expect_equal(d$transitions$nmi, 1)
  expect_equal(d$transitions$from_fragmentation, 0)
  expect_equal(d$transitions$to_fragmentation, 0)
  three[4:6, ] <- three[4:6, c(3, 2, 1)]
  split <- diagnose_amd(diagnostic_curve_fixture(list(two, three)))
  expect_equal(split$transitions$ari, 0)
  expect_equal(split$transitions$nmi, 0)
  expect_equal(split$transitions$from_fragmentation, .5)
  expect_equal(split$transitions$to_fragmentation, 0)
})

test_that("the whole curve is described without selecting K or adding c=1", {
  profile <- function(c, score) {
    top <- score + 1/c
    matrix(c(top, rep((1 - top)/(c - 1), c - 1)),
           nrow = 6, ncol = c, byrow = TRUE)
  }
  x <- diagnostic_curve_fixture(list(
    profile(2, .1), profile(3, .25), profile(4, .25), profile(5, .2)
  ))
  # Construct exact ties, rather than floating-point near ties.
  x$curve[] <- c(.1, .25, .25, .2)
  d <- diagnose_amd(x)
  expect_identical(d$candidates$c, 2:5)
  expect_equal(d$candidates$delta_previous, c(NA, .15, 0, -.05))
  expect_equal(d$candidates$drop_next, c(-.15, 0, .05, NA))
  expect_identical(d$candidates$local_maximum, c(NA, FALSE, FALSE, NA))
  expect_equal(d$curve_summary$exact_maximum_candidates, c(3L, 4L))
  expect_identical(d$curve_summary$numerical_peak_c, 3L)
  expect_false(d$curve_summary$maximum_at_boundary)
  expect_false("K" %in% names(d))
  single_peak <- x
  single_peak$selected_fits[["4"]]$membership <- profile(4, .22)
  single_peak$curve[["4"]] <- .22
  expect_identical(diagnose_amd(single_peak)$candidates$local_maximum,
                   c(NA, TRUE, FALSE, NA))
})

test_that("user candidate ranges are not restricted to the RF training range", {
  U15 <- matrix(1/15, nrow = 20, ncol = 15)
  U17 <- matrix(1/17, nrow = 20, ncol = 17)
  d <- diagnose_amd(diagnostic_curve_fixture(list(U15, U17)))
  expect_identical(d$candidates$c, c(15L, 17L))
  expect_identical(d$transitions$c_step, 2L)
  expect_equal(d$transitions$ari, 1)
  expect_equal(d$transitions$nmi, 1)
})

test_that("stored curve fits, row maps, preprocessing and RNG are preserved", {
  set.seed(20261012)
  X <- matrix(rnorm(180), ncol = 3)
  colnames(X) <- c("a", "b", "c")
  rownames(X) <- paste0("sample_", seq_len(nrow(X)))
  X[7, 1] <- NA_real_
  expect_warning(x <- compute_amd_curve(
    X, its = 3, nin = 2, nsp = 4, seeds = 101:109,
    scale_data = TRUE, preselect_top_sd = 2, iter_max = 2000, verbose = FALSE
  ), "row\\(s\\) with missing values")
  # Version 2 expands lazy ALTREP names. Reading them can change their internal
  # representation, but must never change any stored value or attribute.
  before <- serialize(x, NULL, version = 2)
  rng_before <- .Random.seed
  d <- diagnose_amd(x)
  expect_identical(x, unserialize(before))
  expect_identical(serialize(x, NULL, version = 2), before)
  expect_identical(.Random.seed, rng_before)
  expect_identical(d$complete_rows, x$complete_rows)
  expect_identical(d$omitted_rows, 7L)
  expect_identical(d$settings$preprocessing, x$settings)
  expect_identical(d$settings$source_manifest_id, x$manifest_id)
  expect_equal(d$candidates$amd, unname(x$curve))
  for (key in names(x$selected_fits)) {
    assigned <- assign_configurations(x, c = as.integer(key))
    expect_identical(assigned$membership, x$selected_fits[[key]]$membership)
    expect_identical(assigned$cluster, x$selected_fits[[key]]$cluster)
  }
  expect_output(returned <- print(d), "no automatic K")
  expect_identical(returned, d)
})

test_that("no RNG state is created when the caller has none", {
  x <- diagnostic_split_fixture()
  existed <- exists(".Random.seed", .GlobalEnv, inherits = FALSE)
  if (existed) saved <- get(".Random.seed", .GlobalEnv)
  on.exit({
    if (existed) assign(".Random.seed", saved, .GlobalEnv)
  })
  if (existed) rm(".Random.seed", envir = .GlobalEnv)
  diagnose_amd(x)
  expect_false(exists(".Random.seed", .GlobalEnv, inherits = FALSE))
})

test_that("invalid or incompatible saved objects fail rather than being imputed", {
  x <- diagnostic_split_fixture()
  expect_error(diagnose_amd(matrix(1, 2, 2)), "current amd_curve")
  expect_error(diagnose_amd(legacy_amd_curve_fixture()), "recompute legacy")
  for (gap in list(-.1, 1.1, NA_real_, Inf, c(.1, .2), "0.1", TRUE)) {
    expect_error(diagnose_amd(x, ambiguity_gap = gap), "ambiguity_gap")
  }
  bad <- x
  bad$selected_fits[["3"]] <- NULL
  expect_error(diagnose_amd(bad), "matching selected_fits")
  bad <- x
  names(bad$curve) <- c("3", "2")
  expect_error(diagnose_amd(bad), "increasing integer")
  bad <- x
  bad$curve[[1L]] <- NA_real_
  expect_error(diagnose_amd(bad), "primary curve")
  bad <- x
  bad$curve[[1L]] <- .2
  expect_error(diagnose_amd(bad), "does not match")
  bad <- x
  bad$complete_rows <- c(1L, 1L, 3L, 4L)
  expect_error(diagnose_amd(bad), "maps are missing or inconsistent")
  bad <- x
  bad$omitted_rows <- 1L
  expect_error(diagnose_amd(bad), "maps are missing or inconsistent")
  for (value in c(NA_real_, Inf, -.1, 1.1)) {
    bad <- x
    bad$selected_fits[["2"]]$membership[1, 1] <- value
    expect_error(diagnose_amd(bad), "finite probabilities")
  }
  bad <- x
  bad$selected_fits[["2"]]$membership[1, 1] <- .8
  expect_error(diagnose_amd(bad), "row sums of one")
  bad <- x
  bad$selected_fits[["3"]]$membership <- matrix(1/3, 5, 3)
  expect_error(diagnose_amd(bad), "matching dimensions")
  for (label in c(NA_real_, 0, 1.5, 3)) {
    bad <- x
    bad$selected_fits[["2"]]$cluster[[1L]] <- label
    expect_error(diagnose_amd(bad), "index the membership columns")
  }
  bad <- x
  bad$selected_fits[["2"]]$cluster[[1L]] <- 2L
  expect_error(diagnose_amd(bad), "winning membership")
  bad <- x
  rownames(bad$selected_fits[["3"]]$membership) <- letters[1:4]
  expect_error(diagnose_amd(bad), "row names must agree")
})

test_that("large N membership-only diagnostics do not use sample distances", {
  n <- 100000L
  two <- matrix(c(.9, .1), nrow = n, ncol = 2, byrow = TRUE)
  three <- matrix(c(.05, .05, .9), nrow = n, ncol = 3, byrow = TRUE)
  x <- diagnostic_curve_fixture(list(two, three))
  # A dense N by N double matrix would require 80 GB for this fixture.
  # Making the distance function fail catches an unintended exact silhouette.
  local_mocked_bindings(dist = function(...) stop("sample distances called"),
                        .package = "stats")
  d <- diagnose_amd(x)
  expect_equal(d$settings$n_observations, n)
  expect_equal(nrow(d$groups), 5L)
  expect_equal(d$transitions$nmi, 1)
  expect_lt(as.numeric(object.size(d)), 2000000)
})
