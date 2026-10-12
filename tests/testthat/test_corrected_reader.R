test_that("corrected local features make nonexistent left-pair margins zero", {
  c_values <- 2:5
  y_values <- c(.2, .4, .3, .1)
  first <- candidate_local_features(1L, c_values, y_values)
  edge <- candidate_local_features(2L, c_values, y_values)
  interior <- candidate_local_features(3L, c_values, y_values)
  expect_equal(first$cand__left_pair_available, 0)
  expect_equal(first$cand__left_pair_post_all_margin, 0)
  expect_equal(edge$cand__left_pair_available, 0)
  expect_equal(edge$cand__left_pair_post_all_margin, 0)
  expect_equal(interior$cand__left_pair_available, 1)
  expect_equal(interior$cand__left_pair_post_all_margin, -.1)
})

test_that("model loading requires integrity and rejects historical contracts", {
  skip_if_not_installed("digest")
  file <- tempfile(fileext = ".rds")
  on.exit(unlink(file))
  legacy <- setNames(vector("list", length(.amd_predictor_required_fields)),
                     .amd_predictor_required_fields)
  legacy$schema <- "amd_predictor_bundle_v1"
  legacy$feature_contract <- "amd_modal_features_v1"
  saveRDS(legacy, file)
  hash <- digest::digest(file = file, algo = "sha256")
  expect_error(load_amd_predictor(file), "expected 64-character")
  expect_error(load_amd_predictor(file, "bad"), "expected 64-character")
  expect_error(load_amd_predictor(file, paste(rep("0", 64), collapse = "")),
               "SHA-256 mismatch")
  expect_error(load_amd_predictor(file, hash), "historical v1 bundles remain separate")
  expect_error(load_amd_predictor(NA_character_, hash), "existing predictor bundle")
  expect_error(load_amd_predictor(c(file, file), hash), "existing predictor bundle")
})

test_that("the exact-descriptor memory guard precedes expensive work", {
  X <- matrix(0, 600, 15)
  check <- .amd_predictor_cost(X, 512 * 1024^2)
  expect_true(check$exact_sample_distances)
  expect_false(check$estimate_is_memory_guarantee)
  expect_lt(check$estimated_working_bytes, check$limit_bytes)
  expect_error(.amd_predictor_cost(matrix(0, 100000, 2), 512 * 1024^2),
               "No AMD fits or sample distance matrices")
  expect_error(.amd_predictor_cost(matrix(0, 15, 10000), 512 * 1024^2),
               "global-distance descriptor cap")
  for (limit in list(0, -1, Inf, NA_real_, "512", c(1, 2))) {
    expect_error(.amd_predictor_cost(X, limit), "finite positive")
  }
})

test_that("global distance sampling is disclosed at the actual frozen cap", {
  set.seed(20261012L)
  X <- matrix(rnorm(2001L * 2L), ncol = 2L)
  state <- .Random.seed
  features <- describe_unlabelled_data(X, seed = 214L)
  resources <- .amd_predictor_cost(X, 512 * 1024^2)
  expect_identical(.Random.seed, state)
  expect_equal(features$data__distance_sample_n, 2000L)
  expect_equal(resources$global_distance_sample_n, features$data__distance_sample_n)
  expect_false(resources$exact_sample_distances)
  expect_true(resources$exact_partition_distances)
  expect_true(resources$global_distance_subsampled)
  expect_lt(resources$estimated_working_bytes, resources$limit_bytes)
})

test_that("score ties and consensus are deterministic and not probabilities", {
  expect_identical(.amd_rank_candidate_scores(1:4, c(.7, .7, .2, .1)), 1L)
  expect_identical(.amd_mode_smallest(c(2L, 2L, 3L, 3L, 4L)), 2L)
  scores <- data.frame(candidate_c = 1:4, mean_score = c(.6, .6, .1, .1))
  votes <- data.frame(rf_seed = 1:5, prediction = c(1L, 1L, 2L, 2L, 3L))
  descriptors <- .amd_prediction_descriptors(scores, votes)
  expect_identical(descriptors$prediction, 1L)
  expect_equal(descriptors$exact_vote_fraction, .4)
  expect_equal(descriptors$structure_vote_fraction, .4)
  expect_equal(descriptors$consensus_margin_norm, 0)
})

test_that("unexpected missingness is documented rather than silently discarded", {
  bundle <- list(raw_predictors = c("a", "b"), processed_predictors = c("a", "b"),
    preprocessing = data.frame(variable = c("a", "b"), replacement = c(1, 2),
      training_missing = c(FALSE, FALSE), value_retained = c(TRUE, TRUE)))
  processed <- .amd_apply_predictor_preprocessing(data.frame(a = NA_real_, b = 3), bundle)
  expect_equal(processed$a, 1)
  diagnostic <- attr(processed, "amd_imputation_diagnostics")
  expect_identical(diagnostic$unexpected_nonfinite_values, 1L)
  expect_identical(diagnostic$unexpected_variables, "a")
})

test_that("departures from omitted training-constant values are flagged", {
  bundle <- list(raw_predictors = c("zero", "rank", "variable"),
    processed_predictors = "variable", preprocessing = data.frame(
      variable = c("zero", "rank", "variable"), replacement = c(0, 1, 2),
      training_missing = c(FALSE, FALSE, FALSE), value_retained = c(FALSE, FALSE, TRUE)))
  processed <- .amd_apply_predictor_preprocessing(
    data.frame(zero = c(0, .2), rank = c(1, .5), variable = c(3, 4)), bundle)
  diagnostic <- attr(processed, "amd_imputation_diagnostics")
  expect_identical(names(processed), "variable")
  expect_identical(processed$variable, c(3, 4))
  expect_equal(diagnostic$unassessed_constant_values, 2L)
  expect_identical(diagnostic$unassessed_constant_predictors, c("zero", "rank"))
})

test_that("reader plots allow usual labels without changing the curve", {
  result <- structure(list(curve = list(curve = c("2" = .3, "3" = .4, "4" = .2)),
    numerical_peak = 3L, proposed_k = 3L), class = "amd_prediction")
  file <- tempfile(fileext = ".pdf")
  grDevices::pdf(file)
  on.exit({grDevices::dev.off(); unlink(file)})
  expect_identical(plot(result, xlab = "Candidates", ylab = "Raw AMD"), result)
  expect_identical(plot(result, show_c1 = TRUE, main = "Analytic anchor"), result)
  expect_error(plot(result, show_c1 = NA), "TRUE or FALSE")
})
