assignment_test_data <- function() {
  set.seed(2468)
  rbind(
    matrix(rnorm(90, mean = 0, sd = 0.6), ncol = 3),
    matrix(rnorm(90, mean = 4, sd = 0.6), ncol = 3),
    matrix(rnorm(90, mean = 8, sd = 0.6), ncol = 3)
  )
}

test_that("an assignment extracted from a curve is the exact selected fit", {
  X <- assignment_test_data()
  curve <- compute_amd_curve(
    X, its = 4, nin = 2, nsp = 4, seeds = 101:112,
    iter_max = 1000, verbose = FALSE
  )

  assigned <- assign_configurations(curve, c = 3)
  fit <- curve$selected_fits[["3"]]

  expect_s3_class(assigned, "amd_assignment")
  expect_identical(assigned$schema_version, 2L)
  expect_identical(assigned$source, "amd_curve")
  expect_identical(assigned$c, 3L)
  expect_identical(assigned$c_opt, assigned$c)
  expect_identical(assigned$membership, fit$membership)
  expect_identical(assigned$cluster, fit$cluster)
  expect_identical(assigned$centers, fit$centers)
  expect_equal(assigned$objective, fit$objective)
  expect_equal(assigned$amd, curve$curve[["3"]])
  expect_identical(assigned$selected_start,
                   unname(curve$selected_starts[["3"]]))
  expect_identical(assigned$selected_seed,
                   unname(curve$selected_seeds[["3"]]))
  expect_equal(assigned$starts$objective,
               unname(curve$objectives[, "3"]))
  expect_equal(assigned$starts$amd, unname(curve$raw[, "3"]))
  expect_identical(assigned$complete_rows, curve$complete_rows)
  expect_identical(assigned$omitted_rows, curve$omitted_rows)
  expect_identical(assigned$manifest_id, curve$manifest_id)
  expect_identical(assigned$settings, curve$settings)
})

test_that("the direct 0.1.0 assignment interface remains available", {
  expect_identical(
    names(formals(assign_configurations)),
    c("data", "c", "its", "iter_max", "m", "scale_data", "verbose", "seeds")
  )

  X <- assignment_test_data()
  assigned <- assign_configurations(
    X, c = 3, its = 3, iter_max = 1000,
    seeds = 801:803, verbose = FALSE
  )
  expect_identical(assigned$schema_version, 2L)
  expect_true(all(
    c("c_opt", "cluster", "membership", "centers", "objective") %in%
      names(assigned)
  ))
  expect_identical(assigned$manifest_id,
                   amd_reproducibility_manifest()$manifest_id)
  expect_identical(assigned$settings$input_column_count, ncol(X))
  expect_identical(assigned$settings$retained_columns, seq_len(ncol(X)))
  expect_false(assigned$settings$scaled)
})

test_that("curve extraction propagates the original row map", {
  X <- assignment_test_data()
  omitted <- c(4L, 63L)
  X[omitted, 2] <- NA_real_

  expect_warning(
    curve <- compute_amd_curve(
      X, its = 3, nin = 2, nsp = 4, seeds = seq_len(9),
      iter_max = 1000, verbose = FALSE
    ),
    "2 row\\(s\\) with missing values were omitted"
  )
  assigned <- assign_configurations(curve, c = 3)

  expect_identical(assigned$complete_rows,
                   setdiff(seq_len(nrow(X)), omitted))
  expect_identical(assigned$omitted_rows, omitted)
  expect_equal(nrow(assigned$membership), length(assigned$complete_rows))
})

test_that("standalone fitting is reproducible from supplied seeds", {
  X <- assignment_test_data()
  seeds <- 301:305

  set.seed(1)
  first <- assign_configurations(
    X, c = 3, its = 5, iter_max = 1000, seeds = seeds, verbose = FALSE
  )
  set.seed(999)
  second <- assign_configurations(
    X, c = 3, its = 5, iter_max = 1000, seeds = seeds, verbose = FALSE
  )

  expect_identical(first$membership, second$membership)
  expect_identical(first$starts, second$starts)
  expect_identical(first$selected_seed, second$selected_seed)
})

test_that("supplied standalone seeds preserve the caller random-number state", {
  X <- assignment_test_data()
  set.seed(923)
  state_before <- .Random.seed

  invisible(assign_configurations(
    X, c = 3, its = 4, iter_max = 1000,
    seeds = 401:404, verbose = FALSE
  ))

  expect_identical(.Random.seed, state_before)
})

test_that("NULL standalone seeds use and reproducibly advance caller RNG", {
  X <- assignment_test_data()

  set.seed(924)
  state_before <- .Random.seed
  first <- assign_configurations(
    X, c = 3, its = 4, iter_max = 1000, seeds = NULL, verbose = FALSE
  )
  state_after <- .Random.seed

  set.seed(924)
  second <- assign_configurations(
    X, c = 3, its = 4, iter_max = 1000, seeds = NULL, verbose = FALSE
  )

  expect_false(identical(state_before, state_after))
  expect_identical(first$starts$seed, second$starts$seed)
  expect_identical(first$membership, second$membership)
  expect_identical(first$settings$seed_input_format, "generated_vector")
})

test_that("standalone preprocessing and centre coordinate space are recorded", {
  X <- assignment_test_data()
  colnames(X) <- c("a", "b", "c")
  expected <- scale(X)
  assigned <- assign_configurations(
    X, c = 3, its = 3, iter_max = 1000, seeds = 901:903,
    scale_data = TRUE, verbose = FALSE
  )

  expect_true(assigned$settings$scaled)
  expect_identical(assigned$settings$input_column_names, colnames(X))
  expect_identical(assigned$settings$retained_column_names, colnames(X))
  expect_identical(assigned$settings$scale_center,
                   attr(expected, "scaled:center"))
  expect_identical(assigned$settings$scale_scale,
                   attr(expected, "scaled:scale"))
})

test_that("duplicate standalone seeds are reported", {
  X <- assignment_test_data()
  expect_warning(
    assign_configurations(
      X, c = 3, its = 3, iter_max = 1000,
      seeds = c(11, 11, 12), verbose = FALSE
    ),
    "duplicate seed values within candidate c = 3"
  )
})

test_that("standalone fitting selects the converged minimum-objective start", {
  X <- assignment_test_data()
  assigned <- assign_configurations(
    X, c = 3, its = 6, iter_max = 1000,
    seeds = 501:506, verbose = FALSE
  )

  eligible <- assigned$starts$objective[assigned$starts$converged]
  expect_equal(assigned$objective, min(eligible))
  expect_true(assigned$starts$converged[[assigned$selected_start]])
  expect_identical(
    assigned$selected_seed,
    assigned$starts$seed[[assigned$selected_start]]
  )
})

test_that("a lower-objective nonconverged start cannot supply an assignment", {
  X <- assignment_test_data()
  testthat::local_mocked_bindings(
    cmeans = mixed_convergence_cmeans(),
    .package = "e1071"
  )

  expect_warning(
    assigned <- assign_configurations(
      X, c = 2, its = 2, iter_max = 10,
      seeds = 1:2, verbose = FALSE
    ),
    "1 fit\\(s\\) reached iter_max and were excluded"
  )

  expect_equal(assigned$starts$objective, c(1, 5))
  expect_identical(assigned$starts$converged, c(FALSE, TRUE))
  expect_identical(assigned$selected_start, 2L)
  expect_equal(assigned$objective, 5)
  expect_true(assigned$converged)
})

test_that("standalone fitting can reproduce the corresponding curve fit", {
  X <- assignment_test_data()
  curve <- compute_amd_curve(
    X, its = 5, nin = 2, nsp = 4, seeds = 601:615,
    iter_max = 1000, verbose = FALSE
  )
  seeds_for_c <- curve$seeds[, "3"]

  assigned <- assign_configurations(
    X, c = 3, its = 5, iter_max = 1000,
    seeds = seeds_for_c, verbose = FALSE
  )

  expect_identical(
    assigned$membership,
    curve$selected_fits[["3"]]$membership
  )
  expect_equal(assigned$objective, curve$selected_objectives[["3"]])
  expect_equal(assigned$starts$objective,
               unname(curve$objectives[, "3"]))
  expect_equal(assigned$starts$amd, unname(curve$raw[, "3"]))
})

test_that("assignment inputs fail early with informative errors", {
  X <- assignment_test_data()
  curve <- compute_amd_curve(
    X, its = 2, nin = 2, nsp = 3, seeds = 1:4,
    iter_max = 1000, verbose = FALSE
  )

  expect_error(assign_configurations(X, c = 1, verbose = FALSE),
               "at least 2")
  expect_error(assign_configurations(X, c = 3, its = 0, verbose = FALSE),
               "its must be one positive integer")
  expect_error(
    assign_configurations(X, c = 3, its = 3, seeds = 1:2,
                          verbose = FALSE),
    "enough positive integer"
  )
  expect_error(assign_configurations(curve, c = 4),
               "was not evaluated")

  legacy <- legacy_amd_curve_fixture()
  expect_error(
    assign_configurations(legacy, c = 2),
    "does not contain selected_fits; recompute"
  )

  truncated <- curve
  truncated$selected_objectives <- NULL
  expect_error(
    assign_configurations(truncated, c = 2),
    "incomplete or unrecognised schema"
  )

  constant_X <- cbind(X, constant = 1)
  expect_error(
    assign_configurations(constant_X, c = 3, scale_data = TRUE,
                          verbose = FALSE),
    "zero or non-finite variable variance"
  )
})

test_that("nonconverged starts cannot supply an assignment", {
  X <- assignment_test_data()

  expect_error(
    suppressWarnings(assign_configurations(
      X, c = 3, its = 3, iter_max = 1,
      seeds = 701:703, verbose = FALSE
    )),
    "no converged fit was found"
  )
})
