test_data <- function() {
  set.seed(8102)
  rbind(
    matrix(rnorm(120, mean = 0, sd = 0.7), ncol = 3),
    matrix(rnorm(120, mean = 4, sd = 0.7), ncol = 3),
    matrix(rnorm(120, mean = 8, sd = 0.7), ncol = 3)
  )
}

test_that("compute_amd_curve returns the corrected and legacy components", {
  X <- test_data()
  result <- compute_amd_curve(
    X,
    its = 4,
    nin = 2,
    nsp = 5,
    seeds = seq_len(16),
    iter_max = 1000,
    verbose = FALSE
  )

  expect_s3_class(result, "amd_curve")
  expect_identical(result$schema_version, 2L)
  expect_identical(result$c_opt, result$c_peak)
  expect_length(result$curve, 4)
  expect_length(result$mean, 4)
  expect_length(result$max, 4)
  expect_equal(dim(result$raw), c(4, 4))
  expect_equal(dim(result$objectives), c(4, 4))
  expect_equal(dim(result$converged), c(4, 4))
  expect_true(all(result$converged))
  expect_identical(
    result$seeds,
    matrix(
      seq_len(16),
      nrow = 4,
      ncol = 4,
      byrow = TRUE,
      dimnames = list(paste0("start_", 1:4), as.character(2:5))
    )
  )
  expect_identical(result$settings$seed_input_format, "vector")
  expect_identical(result$settings$input_column_count, ncol(X))
  expect_null(result$settings$input_column_names)
  expect_identical(result$settings$retained_columns, seq_len(ncol(X)))
  expect_null(result$settings$retained_column_names)
  expect_null(result$settings$scale_center)
  expect_null(result$settings$scale_scale)
  expect_identical(
    result$manifest_id,
    amd_reproducibility_manifest()$manifest_id
  )
  expect_identical(result$complete_rows, seq_len(nrow(X)))
  expect_identical(result$omitted_rows, integer())
  expect_identical(
    result$selection_rule,
    "minimum_objective_among_converged_starts"
  )
})

test_that("the public 0.1.0 call signature and object fields are retained", {
  expect_identical(
    names(formals(compute_amd_curve)),
    c(
      "data", "its", "nin", "nsp", "seeds", "verbose", "plot_curve",
      "open_device", "scale_data", "iter_max", "m", "preselect_top_sd"
    )
  )
  expect_false("definition" %in% names(formals(compute_amd_curve)))

  X <- test_data()
  result <- compute_amd_curve(
    X, its = 2, nin = 2, nsp = 3, seeds = 1:4,
    iter_max = 1000, verbose = FALSE
  )
  expect_true(all(
    c("c_opt", "max", "mean", "raw", "coordinates") %in% names(result)
  ))
})

test_that("preselection and scaling transformations are fully recorded", {
  X <- test_data()
  X <- sweep(X, 2L, c(1, 10, 3), `*`)
  colnames(X) <- c("low_variance", "high_variance", "medium_variance")
  expected_keep <- order(apply(X, 2L, stats::sd), decreasing = TRUE)[1:2]
  selected_X <- X[, expected_keep, drop = FALSE]
  expected_scaled <- scale(selected_X)
  seed_matrix <- matrix(
    4321L,
    nrow = 1L,
    dimnames = list("start_1", "2")
  )

  result <- compute_amd_curve(
    X,
    its = 1,
    nin = 2,
    nsp = 2,
    seeds = seed_matrix,
    scale_data = TRUE,
    preselect_top_sd = 2,
    iter_max = 1000,
    verbose = FALSE
  )

  expect_identical(result$settings$input_column_count, ncol(X))
  expect_identical(result$settings$input_column_names, colnames(X))
  expect_identical(result$settings$retained_columns, expected_keep)
  expect_identical(
    result$settings$retained_column_names,
    colnames(X)[expected_keep]
  )
  expect_identical(
    result$settings$scale_center,
    attr(expected_scaled, "scaled:center")
  )
  expect_identical(
    result$settings$scale_scale,
    attr(expected_scaled, "scaled:scale")
  )

  set.seed(seed_matrix[1, "2"])
  reconstructed_fit <- e1071::cmeans(
    scale(
      selected_X,
      center = result$settings$scale_center,
      scale = result$settings$scale_scale
    ),
    centers = 2,
    iter.max = 1000,
    verbose = FALSE,
    m = 2
  )

  expect_identical(
    result$selected_fits[["2"]]$membership,
    reconstructed_fit$membership
  )
  expect_identical(
    result$selected_fits[["2"]]$centers,
    reconstructed_fit$centers
  )
  expect_identical(
    result$selected_fits[["2"]]$cluster,
    reconstructed_fit$cluster
  )
  expect_identical(
    result$selected_fits[["2"]]$objective,
    as.numeric(reconstructed_fit$withinerror)
  )
})

test_that("named seed matrices preserve common fits across nested ranges", {
  X <- test_data()
  its <- 4L
  master_seeds <- matrix(
    seq.int(1001, 1001 + its * 7L - 1L),
    nrow = its,
    ncol = 7L,
    dimnames = list(NULL, as.character(2:8))
  )

  narrow <- compute_amd_curve(
    X, its, 2, 6, seeds = master_seeds,
    iter_max = 1000, verbose = FALSE
  )
  extended <- compute_amd_curve(
    X, its, 2, 8, seeds = master_seeds,
    iter_max = 1000, verbose = FALSE
  )
  common_c <- as.character(2:6)

  expect_identical(narrow$settings$seed_input_format, "named_matrix")
  expect_identical(narrow$seeds, extended$seeds[, common_c, drop = FALSE])
  expect_identical(narrow$raw, extended$raw[, common_c, drop = FALSE])
  expect_identical(
    narrow$objectives,
    extended$objectives[, common_c, drop = FALSE]
  )
  expect_identical(
    narrow$iterations,
    extended$iterations[, common_c, drop = FALSE]
  )
  expect_identical(
    narrow$converged,
    extended$converged[, common_c, drop = FALSE]
  )
  expect_identical(narrow$curve, extended$curve[common_c])
  expect_identical(
    narrow$selected_fits,
    extended$selected_fits[common_c]
  )
})

test_that("seed matrices fail early when their mapping is ambiguous", {
  X <- test_data()

  unnamed <- matrix(1:6, nrow = 2)
  expect_error(
    compute_amd_curve(X, 2, 2, 4, seeds = unnamed, verbose = FALSE),
    "unique, non-empty column names"
  )

  wrong_rows <- matrix(1:9, nrow = 3,
                       dimnames = list(NULL, as.character(2:4)))
  expect_error(
    compute_amd_curve(X, 2, 2, 4, seeds = wrong_rows, verbose = FALSE),
    "one row per random start"
  )

  missing_c <- matrix(1:4, nrow = 2,
                      dimnames = list(NULL, c("2", "4")))
  expect_error(
    compute_amd_curve(X, 2, 2, 4, seeds = missing_c, verbose = FALSE),
    "no column for c = 3"
  )
})

test_that("incomplete rows are reported and retained in the row map", {
  X <- test_data()
  omitted <- c(5L, 77L)
  X[omitted, 1] <- NA_real_

  expect_warning(
    result <- compute_amd_curve(
      X, 2, 2, 4, seeds = seq_len(6), iter_max = 1000, verbose = FALSE
    ),
    "2 row\\(s\\) with missing values were omitted"
  )

  expect_identical(result$complete_rows, setdiff(seq_len(nrow(X)), omitted))
  expect_identical(result$omitted_rows, omitted)
  expect_equal(
    nrow(result$selected_fits[["2"]]$membership),
    length(result$complete_rows)
  )
})

test_that("the supplied seeds reproduce every fitted result", {
  X <- test_data()
  seeds <- seq.int(101, 116)

  set.seed(1)
  first <- compute_amd_curve(
    X, 4, 2, 5, seeds = seeds, iter_max = 1000, verbose = FALSE
  )
  set.seed(999)
  second <- compute_amd_curve(
    X, 4, 2, 5, seeds = seeds, iter_max = 1000, verbose = FALSE
  )

  expect_identical(first$raw, second$raw)
  expect_identical(first$objectives, second$objectives)
  expect_identical(first$curve, second$curve)
  expect_identical(first$coordinates, second$coordinates)
})

test_that("supplied seeds do not change the caller random-number state", {
  X <- test_data()
  set.seed(923)
  state_before <- .Random.seed

  invisible(compute_amd_curve(
    X, 3, 2, 4, seeds = seq_len(9), iter_max = 1000, verbose = FALSE
  ))

  expect_identical(.Random.seed, state_before)
})

test_that("NULL curve seeds use and reproducibly advance caller RNG", {
  X <- test_data()

  set.seed(925)
  state_before <- .Random.seed
  first <- compute_amd_curve(
    X, 3, 2, 4, seeds = NULL, iter_max = 1000, verbose = FALSE
  )
  state_after <- .Random.seed

  set.seed(925)
  second <- compute_amd_curve(
    X, 3, 2, 4, seeds = NULL, iter_max = 1000, verbose = FALSE
  )

  expect_false(identical(state_before, state_after))
  expect_identical(first$seeds, second$seeds)
  expect_identical(first$curve, second$curve)
  expect_identical(first$settings$seed_input_format, "generated_vector")
})

test_that("duplicate seeds within a candidate are reported", {
  X <- test_data()
  duplicate_seeds <- matrix(
    c(11, 11, 12, 21, 22, 23),
    nrow = 3,
    dimnames = list(NULL, c("2", "3"))
  )
  expect_warning(
    compute_amd_curve(
      X, 3, 2, 3, seeds = duplicate_seeds,
      iter_max = 1000, verbose = FALSE
    ),
    "duplicate seed values within candidate c = 2"
  )
})

test_that("each primary curve point uses the converged minimum-objective fit", {
  X <- test_data()
  result <- compute_amd_curve(
    X,
    its = 5,
    nin = 2,
    nsp = 5,
    seeds = seq_len(20),
    iter_max = 1000,
    verbose = FALSE
  )

  for (j in seq_along(result$curve)) {
    converged_objectives <- result$objectives[result$converged[, j], j]
    expect_equal(result$selected_objectives[[j]], min(converged_objectives))
    expect_equal(
      result$curve[[j]],
      amd_score(result$selected_fits[[j]]$membership)
    )
  }

  peak_index <- match(as.character(result$c_peak), names(result$curve))
  expect_identical(
    result$coordinates,
    result$selected_fits[[peak_index]]$membership
  )
  expect_equal(
    amd_score(result$coordinates),
    result$curve[[peak_index]]
  )
})

test_that("a lower-objective nonconverged start cannot supply a curve point", {
  X <- test_data()
  testthat::local_mocked_bindings(
    cmeans = mixed_convergence_cmeans(),
    .package = "e1071"
  )

  expect_warning(
    result <- compute_amd_curve(
      X, its = 2, nin = 2, nsp = 2, seeds = 1:2,
      iter_max = 10, verbose = FALSE
    ),
    "1 fit\\(s\\) reached iter_max and were excluded"
  )

  expect_equal(unname(result$objectives[, "2"]), c(1, 5))
  expect_identical(unname(result$converged[, "2"]), c(FALSE, TRUE))
  expect_identical(unname(result$selected_starts[["2"]]), 2L)
  expect_equal(unname(result$selected_objectives[["2"]]), 5)
  expect_equal(
    result$curve[["2"]],
    amd_score(result$selected_fits[["2"]]$membership)
  )
})

test_that("curve construction rejects c = 1 and invalid ranges", {
  X <- test_data()

  expect_error(
    compute_amd_curve(X, 2, 1, 3, seeds = 1:6, verbose = FALSE),
    "must begin at c >= 2"
  )
  expect_error(
    compute_amd_curve(X, 2, 4, 3, seeds = 1:4, verbose = FALSE),
    "nin cannot be greater"
  )
  expect_error(
    compute_amd_curve(X, 2, 2, 4, seeds = 1:2, verbose = FALSE),
    "enough positive integer"
  )

  tiny <- matrix(rnorm(12), nrow = 4)
  expect_error(
    compute_amd_curve(tiny, 1, 2, 4, seeds = 1, verbose = FALSE),
    "enough complete rows"
  )
})

test_that("compute_amd_curve can plot on an already active device", {
  X <- test_data()
  plot_file <- tempfile(fileext = ".pdf")
  grDevices::pdf(plot_file)
  on.exit(grDevices::dev.off(), add = TRUE)
  active_device <- grDevices::dev.cur()

  expect_silent(compute_amd_curve(
    X, 2, 2, 3, seeds = 1:4, iter_max = 1000,
    verbose = FALSE, plot_curve = TRUE, open_device = FALSE
  ))
  expect_identical(grDevices::dev.cur(), active_device)
})

test_that("logical curve arguments fail early with informative errors", {
  X <- test_data()

  expect_error(
    compute_amd_curve(X, 1, 2, 2, seeds = 1, verbose = 1),
    "verbose must be TRUE or FALSE"
  )
  expect_error(
    compute_amd_curve(
      X, 1, 2, 2, seeds = 1, verbose = FALSE, plot_curve = NA
    ),
    "plot_curve must be TRUE or FALSE"
  )
  expect_error(
    compute_amd_curve(
      X, 1, 2, 2, seeds = 1, verbose = FALSE, open_device = c(TRUE, FALSE)
    ),
    "open_device must be TRUE or FALSE"
  )
  expect_error(
    compute_amd_curve(
      X, 1, 2, 2, seeds = 1, verbose = FALSE, scale_data = 0
    ),
    "scale_data must be TRUE or FALSE"
  )
})

test_that("a numerical peak at the lower boundary is explicitly flagged", {
  X <- test_data()
  testthat::local_mocked_bindings(
    cmeans = curve_profile_cmeans(c("2" = 0.40, "3" = 0.20, "4" = 0.10)),
    .package = "e1071"
  )
  result <- compute_amd_curve(
    X, 1, 2, 4, seeds = 1:3, iter_max = 10, verbose = FALSE
  )

  expect_true(result$peak_at_boundary)
  expect_identical(result$c_peak, 2L)
})

test_that("a numerical peak at the upper boundary is explicitly flagged", {
  X <- test_data()
  testthat::local_mocked_bindings(
    cmeans = curve_profile_cmeans(c("2" = 0.10, "3" = 0.20, "4" = 0.60)),
    .package = "e1071"
  )
  result <- compute_amd_curve(
    X, 1, 2, 4, seeds = 1:3, iter_max = 10, verbose = FALSE
  )

  expect_true(result$peak_at_boundary)
  expect_identical(result$c_peak, 4L)
})

test_that("an interior numerical peak is not flagged as a boundary peak", {
  X <- test_data()
  testthat::local_mocked_bindings(
    cmeans = curve_profile_cmeans(c("2" = 0.10, "3" = 0.55, "4" = 0.30)),
    .package = "e1071"
  )
  result <- compute_amd_curve(
    X, 1, 2, 4, seeds = 1:3, iter_max = 10, verbose = FALSE
  )

  expect_false(result$peak_at_boundary)
  expect_identical(result$c_peak, 3L)
})

test_that("an exact maximum tie is reported at the smaller c", {
  X <- test_data()
  testthat::local_mocked_bindings(
    cmeans = curve_profile_cmeans(c("2" = 0.25, "3" = 0.10, "4" = 0.25)),
    .package = "e1071"
  )
  result <- compute_amd_curve(
    X, 1, 2, 4, seeds = 1:3, iter_max = 10, verbose = FALSE
  )

  expect_identical(result$curve[["2"]], result$curve[["4"]])
  expect_true(result$peak_at_boundary)
  expect_identical(result$c_peak, 2L)
})

test_that("the documented default iteration limit works with the real backend", {
  set.seed(941)
  X <- rbind(
    matrix(rnorm(60, mean = -2, sd = 0.5), ncol = 2),
    matrix(rnorm(60, mean = 2, sd = 0.5), ncol = 2)
  )

  result <- compute_amd_curve(
    X, its = 2, nin = 2, nsp = 3, seeds = 1:4, verbose = FALSE
  )

  expect_true(all(result$converged))
  expect_true(all(result$iterations < 100L))
})
