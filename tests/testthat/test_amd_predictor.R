test_that("predictor seed plans are reproducible and preserve caller RNG", {
  set.seed(123)
  before <- .Random.seed
  first <- AMDconfigurations:::.amd_predictor_seed_plan(1L, 2:14, 20L, 5L)
  expect_identical(.Random.seed, before)
  second <- AMDconfigurations:::.amd_predictor_seed_plan(1L, 2:14, 20L, 5L)
  expect_identical(first, second)
  expect_identical(dim(first$curve), c(20L, 13L))
  expect_identical(dim(first$stability), c(5L, 13L))
})

test_that("candidate-score descriptors keep structure and exact votes separate", {
  scores <- data.frame(
    candidate_c = 1:4,
    mean_score = c(0.1, 0.2, 0.8, 0.3)
  )
  votes <- data.frame(
    rf_seed = 1:5,
    prediction = c(3L, 3L, 3L, 4L, 4L)
  )
  result <- AMDconfigurations:::.amd_prediction_descriptors(scores, votes)
  expect_identical(result$prediction, 3L)
  expect_equal(result$exact_vote_fraction, 0.6)
  expect_equal(result$structure_vote_fraction, 1)
  expect_true(result$structure_margin_norm > 0)
})

test_that("applicability separates validation, interpolation and extrapolation", {
  contract <- list(
    n_validation_range = c(150L, 600L),
    d_validation_range = c(2L, 15L),
    n_validation_grid = c(150L, 600L),
    d_validation_grid = c(2L, 6L, 15L)
  )
  inside <- AMDconfigurations:::.amd_predictor_applicability(
    matrix(0, 150L, 2L), contract
  )
  interpolation <- AMDconfigurations:::.amd_predictor_applicability(
    matrix(0, 300L, 3L), contract
  )
  outside <- AMDconfigurations:::.amd_predictor_applicability(
    matrix(0, 100L, 20L), contract
  )
  expect_true(inside$in_validated_numeric_domain)
  expect_identical(inside$numerical_status, "validated_grid_point")
  expect_false(interpolation$in_validated_numeric_domain)
  expect_true(interpolation$within_validation_envelope)
  expect_identical(
    interpolation$numerical_status, "unvalidated_interpolation"
  )
  expect_false(outside$in_validated_numeric_domain)
  expect_false(outside$within_validation_envelope)
  expect_identical(outside$numerical_status, "unvalidated_extrapolation")
  expect_length(outside$reasons, 2L)
  expect_match(outside$geometry_note, "geometry")
})

test_that("preprocessing reports imputations absent from training", {
  bundle <- list(
    raw_predictors = c("ordinary", "known_missing"),
    processed_predictors = c(
      "ordinary", "missing__known_missing", "known_missing"
    ),
    preprocessing = data.frame(
      variable = c("ordinary", "known_missing"),
      replacement = c(1, 2),
      training_missing = c(FALSE, TRUE),
      value_retained = c(TRUE, TRUE),
      stringsAsFactors = FALSE
    )
  )
  data <- data.frame(
    ordinary = c(NA_real_, 3), known_missing = c(4, NA_real_)
  )
  processed <- AMDconfigurations:::.amd_apply_predictor_preprocessing(
    data, bundle
  )
  diagnostics <- attr(processed, "amd_imputation_diagnostics")
  expect_equal(processed$ordinary, c(1, 3))
  expect_equal(processed$missing__known_missing, c(0, 1))
  expect_identical(diagnostics$unexpected_variables, "ordinary")
  expect_identical(diagnostics$unexpected_nonfinite_values, 1L)
})

test_that("curve-data validation rejects a mismatched data set", {
  set.seed(4)
  X <- matrix(rnorm(80), ncol = 2)
  curve <- compute_amd_curve(
    X, its = 1L, nin = 2L, nsp = 3L,
    seeds = matrix(
      c(1L, 2L), nrow = 1L,
      dimnames = list("start_1", c("2", "3"))
    ),
    verbose = FALSE, iter_max = 2000L
  )
  contract <- list(
    numeric_candidates = 2:3,
    curve_starts = 1L,
    iter_max = 2000L,
    m = 2,
    selected_fit_rule = "minimum_objective_among_converged_starts"
  )
  prepared <- AMDconfigurations:::.amd_predictor_data(X)
  expect_invisible(
    AMDconfigurations:::.amd_validate_predictor_curve(
      curve, prepared, contract
    )
  )
  altered <- X
  altered[1, 1] <- altered[1, 1] + 1
  expect_error(
    AMDconfigurations:::.amd_validate_predictor_curve(
      curve, AMDconfigurations:::.amd_predictor_data(altered), contract
    ),
    "do not reproduce"
  )
  altered_curve <- curve
  altered_curve$raw[altered_curve$selected_starts[["2"]], "2"] <-
    altered_curve$raw[altered_curve$selected_starts[["2"]], "2"] + 0.01
  expect_error(
    AMDconfigurations:::.amd_validate_predictor_curve(
      altered_curve, prepared, contract
    ),
    "inconsistent"
  )
})
