test_that("reference probabilities never bypass feature and support guards", {
  calibrated <- c(structure = 0.98, exact = 0.91)
  grid <- list(on_validated_grid = TRUE, numerical_status = "validated_grid_point")
  offgrid <- list(on_validated_grid = FALSE, numerical_status = "unvalidated_interpolation")
  inside <- .amd_reference_reliability(calibrated, grid, TRUE, TRUE, TRUE, FALSE)
  expect_equal(inside$one_several, 0.98)
  expect_equal(inside$exact_k, 0.91)
  expect_identical(inside$status, "conditional_simulation_reference")
  outside <- .amd_reference_reliability(calibrated, offgrid, TRUE, TRUE, TRUE, FALSE)
  expect_true(is.na(outside$exact_k))
  explicit <- .amd_reference_reliability(calibrated, offgrid, TRUE, TRUE, TRUE, TRUE)
  expect_equal(explicit$exact_k, 0.91)
  expect_identical(explicit$status, "reference_only_unvalidated_interpolation")
  for (on_grid in list(grid, offgrid)) {
    for (override in c(FALSE, TRUE)) {
      for (guard in seq_len(3L)) {
        flags <- rep(TRUE, 3L)
        flags[guard] <- FALSE
        value <- .amd_reference_reliability(
          calibrated, on_grid, flags[1], flags[2], flags[3], override
        )
        expect_true(is.na(value$exact_k))
        expect_true(is.na(value$one_several))
        expect_match(value$status, "^suppressed_")
      }
    }
  }
})
