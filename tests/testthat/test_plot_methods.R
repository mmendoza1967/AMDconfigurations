test_that("plot methods run without error", {
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

  plot_file <- tempfile(fileext = ".pdf")
  grDevices::pdf(plot_file)
  on.exit(grDevices::dev.off(), add = TRUE)

  expect_silent(plot(res, open_device = FALSE))
  expect_silent(plot_AMD(res, open_device = FALSE))
  expect_silent(plot_AMD(res, type = "mean", open_device = FALSE))
  expect_silent(plot_AMD(res, type = "max", open_device = FALSE))
  expect_error(
    plot_AMD(res, open_device = NA),
    "open_device must be TRUE or FALSE"
  )
})

test_that("legacy 0.1.0 curve objects remain plottable without fabrication", {
  legacy <- legacy_amd_curve_fixture()
  plot_file <- tempfile(fileext = ".pdf")
  grDevices::pdf(plot_file)
  on.exit(grDevices::dev.off(), add = TRUE)

  expect_warning(
    plot(legacy),
    "legacy AMDconfigurations 0.1.0"
  )
  expect_warning(
    plot_AMD(legacy, type = "max"),
    "Plotting the stored max"
  )
  expect_error(
    plot_AMD(legacy, type = "curve"),
    "has no stored primary curve"
  )
})

test_that("plot methods use the active device by default", {
  set.seed(2)
  X <- matrix(rnorm(120), ncol = 3)
  res <- compute_amd_curve(
    X, its = 2, nin = 2, nsp = 3, seeds = 1:4,
    iter_max = 1000, verbose = FALSE
  )
  plot_file <- tempfile(fileext = ".pdf")
  grDevices::pdf(plot_file)
  on.exit(grDevices::dev.off(), add = TRUE)
  active_device <- grDevices::dev.cur()

  expect_silent(plot(res))
  expect_identical(grDevices::dev.cur(), active_device)
})
