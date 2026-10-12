test_that("public predictor completes an end-to-end analysis", {
  bundle_path <- Sys.getenv("AMD_PREDICTOR_BUNDLE", unset = "")
  skip_if(!nzchar(bundle_path),
          "external predictor bundle is not available during base checks")
  skip_if_not_installed("ranger")

  set.seed(27092026L)
  dimensions <- 6L
  per_group <- 50L
  centers <- rbind(
    c(-5, 0, 0, 0, 0, 0),
    c(0, 5, 0, 0, 0, 0),
    c(5, 0, 0, 0, 0, 0)
  )
  X <- do.call(rbind, lapply(seq_len(nrow(centers)), function(group) {
    values <- matrix(rnorm(per_group * dimensions), ncol = dimensions)
    sweep(values, 2L, centers[group, ], FUN = "+")
  }))
  X <- X[sample.int(nrow(X)), , drop = FALSE]

  predictor <- load_amd_predictor(bundle_path,
    sha256 = Sys.getenv("AMD_PREDICTOR_SHA256", unset = ""))
  random_state <- .Random.seed
  result <- predict_amd(X, predictor, seed = 27092027L, verbose = FALSE)
  expect_identical(.Random.seed, random_state)

  expect_s3_class(result, "amd_prediction")
  expect_s3_class(result$curve, "amd_curve")
  expect_identical(as.integer(names(result$curve$curve)), 2:14)
  expect_equal(nrow(result$candidate_scores), 14L)
  expect_equal(nrow(result$per_forest), 5L)
  expect_true(result$applicability$on_validated_grid)
  expect_true(all(is.finite(unlist(
    result$reliability[c("structure", "exact_k")]
  ))))
  expect_identical(result$diagnostics$curve_source, "computed")
  expect_identical(
    result$diagnostics$imputation$unexpected_nonfinite_values, 0L
  )
  expect_identical(predictor$feature_contract, "amd_modal_features_left_pair_v2")
  expect_identical(result$predictor$confidence_assessment,
                   "retrospective_final_bank_20261012")
  expect_true(result$diagnostics$refits_are_computational_not_resampling)
  expect_true(result$diagnostics$resources$exact_sample_distances)
  expect_identical(summary(result)$target, "numerically_resolved_density_mode_count")
  expect_length(result$diagnostics$imputation$unassessed_constant_predictors, 0L)
  expect_output(print(result), "Proposed modal count")
  expect_true(all(result$diagnostics$candidate_features$cand__left_pair_post_all_margin[
    result$diagnostics$candidate_features$cand__left_pair_available == 0L] == 0))

  wrong <- predictor
  wrong$feature_contract <- "amd_modal_features_v1"
  expect_error(predict_amd(X, wrong, verbose = FALSE), "historical v1 bundles")
  wrong <- predictor
  wrong$created_utc <- NULL
  expect_error(predict_amd(X, wrong, verbose = FALSE), "lacks required fields: created_utc")
  wrong <- predictor
  wrong$contract$resolved_target <- "generating_component_count"
  expect_error(predict_amd(X, wrong, verbose = FALSE), "resolved density-mode target")
  wrong <- predictor
  wrong$calibrators$structure$terms <- stats::terms(~ N)
  expect_error(predict_amd(X, wrong, verbose = FALSE), "calibrator formulas")
  expect_error(predict_amd(matrix(0, 100000, 2), predictor,
    curve = stop("supplied curve evaluated too early"), verbose = FALSE),
    "No AMD fits or sample distance matrices")

  original_applicability <- .amd_predictor_applicability
  local_mocked_bindings(
    compute_amd_curve = function(...) stop("principal fits must not be repeated"),
    .amd_predictor_applicability = function(X, contract) {
      value <- original_applicability(X, contract)
      value$on_validated_grid <- FALSE
      value$in_validated_numeric_domain <- FALSE
      value$numerical_status <- "unvalidated_interpolation"
      value
    }
  )
  reused <- predict_amd(X, predictor, curve = result$curve,
                        seed = 27092027L, verbose = FALSE)
  expect_identical(reused$curve, result$curve)
  expect_identical(reused$proposed_k, result$proposed_k)
  expect_equal(reused$candidate_scores, result$candidate_scores)
  expect_equal(reused$per_forest, result$per_forest)
  expect_identical(reused$diagnostics$curve_source, "supplied")
  expect_identical(.Random.seed, random_state)
  expect_true(is.na(reused$reliability$structure))
  expect_true(is.na(reused$reliability$exact_k))
  expect_identical(reused$reliability$status, "suppressed_unvalidated_interpolation")
  expect_output(print(reused), "proposal itself is exploratory")

  local({
    saved_random_state <- .Random.seed
    on.exit(assign(".Random.seed", saved_random_state, envir = .GlobalEnv))
    rm(".Random.seed", envir = .GlobalEnv)
    explicit <- predict_amd(X, predictor, curve = result$curve,
      seed = 27092027L, verbose = FALSE, allow_extrapolation = TRUE)
    expect_false(exists(".Random.seed", envir = .GlobalEnv, inherits = FALSE))
    expect_equal(explicit$candidate_scores, result$candidate_scores)
    expect_equal(explicit$reliability$structure, result$reliability$structure)
    expect_equal(explicit$reliability$exact_k, result$reliability$exact_k)
    expect_identical(explicit$reliability$status, "unvalidated_interpolation")
  })

  original_preprocessing <- .amd_apply_predictor_preprocessing
  local_mocked_bindings(.amd_apply_predictor_preprocessing = function(data, bundle) {
    value <- original_preprocessing(data, bundle)
    diagnostics <- attr(value, "amd_imputation_diagnostics")
    diagnostics$unassessed_constant_predictors <- "data__zero_variance_fraction"
    diagnostics$unassessed_constant_values <- 1L
    attr(value, "amd_imputation_diagnostics") <- diagnostics
    value
  })
  expect_warning(degenerate <- predict_amd(X, predictor, curve = result$curve,
    seed = 27092027L, verbose = FALSE, allow_extrapolation = TRUE),
    "departs from training-constant predictors")
  expect_true(is.na(degenerate$reliability$structure))
  expect_true(is.na(degenerate$reliability$exact_k))
  expect_identical(degenerate$reliability$status, "suppressed_unassessed_constant_predictors")
  expect_equal(degenerate$candidate_scores, result$candidate_scores)
  expect_identical(degenerate$proposed_k, result$proposed_k)
})
