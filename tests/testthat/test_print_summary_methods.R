test_that("print and summary methods run without error", {
  set.seed(1)
  X <- matrix(rnorm(200), ncol = 5)

  res <- compute_amd_curve(
    data = X,
    its = 3,
    nin = 2,
    nsp = 4,
    seeds = 1:9,
    iter_max = 1000,
    verbose = FALSE
  )

  expect_output(print(res), "Numerical peak c")
  expect_output(summary(res), "selected_objective")
})

test_that("print and summary describe legacy objects without inventing a curve", {
  legacy <- legacy_amd_curve_fixture()

  expect_identical(names(legacy$mean), c("2", "3", "4"))
  expect_identical(names(legacy$max), c("2", "3", "4"))
  expect_identical(
    AMDconfigurations:::.amd_curve_candidates(legacy$mean),
    c(2, 3, 4)
  )

  expect_warning(invisible(print(legacy)), "legacy AMDconfigurations 0.1.0")
  printed <- capture.output(suppressWarnings(print(legacy)))
  expect_true(any(grepl("Primary minimum-objective curve: unavailable", printed)))
  expect_true(any(grepl(
    paste0("Stored c_opt: ", legacy$c_opt),
    printed,
    fixed = TRUE
  )))

  expect_warning(invisible(summary(legacy)), "legacy AMDconfigurations 0.1.0")
  summarised <- capture.output(suppressWarnings(summary(legacy)))
  expect_true(any(grepl("legacy schema 0.1.0", summarised)))
  expect_true(any(grepl("Primary minimum-objective curve: unavailable", summarised)))
})

test_that("truncated schema-2 objects fail cleanly", {
  set.seed(2)
  X <- matrix(rnorm(120), ncol = 3)
  curve <- compute_amd_curve(
    X, its = 2, nin = 2, nsp = 3, seeds = 1:4,
    iter_max = 1000, verbose = FALSE
  )
  curve$converged <- NULL

  expect_error(print(curve), "recognised AMD curve schema")
  expect_error(summary(curve), "recognised AMD curve schema")
})
