test_that("bundled ecological data match the documented contract", {
  environment <- new.env(parent = emptyenv())
  utils::data("Fulldata", package = "AMDconfigurations", envir = environment)
  object <- environment$Fulldata

  expect_s3_class(object, "data.frame")
  expect_identical(dim(object), c(6619L, 34L))
  expect_true(all(c(
    "lat", "lon", "Key", "IFd", "GIn", "HIn", "IFr", "Crn", "Pln",
    "Frg", "Nct", "Grn", paste0("BIO", 1:19), "island", "Clst", "NAME"
  ) %in% names(object)))
})

test_that("bundled transcriptomic data match the documented contract", {
  environment <- new.env(parent = emptyenv())
  utils::data(
    "Transcdata_small", package = "AMDconfigurations", envir = environment
  )
  object <- environment$Transcdata_small

  expect_s3_class(object, "data.frame")
  expect_identical(dim(object), c(931L, 1000L))
  expect_true(all(vapply(object, is.numeric, logical(1))))
})
