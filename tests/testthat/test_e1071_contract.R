test_that("the supported e1071 fuzzy c-means contract is present", {
  expect_gte(
    utils::compareVersion(
      as.character(utils::packageVersion("e1071")),
      "1.7-17"
    ),
    0
  )

  set.seed(1901)
  X <- rbind(
    matrix(rnorm(80, mean = -2), ncol = 2),
    matrix(rnorm(80, mean = 2), ncol = 2)
  )
  fit <- e1071::cmeans(
    X, centers = 2, iter.max = 1000, verbose = FALSE, m = 2
  )

  expect_true(all(
    c("centers", "cluster", "membership", "iter", "withinerror") %in%
      names(fit)
  ))
  expect_true(is.matrix(fit$membership))
  expect_true(is.matrix(fit$centers))
  expect_length(fit$cluster, nrow(X))
  expect_equal(rowSums(fit$membership), rep(1, nrow(X)), tolerance = 1e-12)
  expect_identical(fit$cluster, apply(fit$membership, 1L, which.max))
  expect_true(is.finite(fit$withinerror))
  expect_true(is.numeric(fit$iter) && length(fit$iter) == 1L)
  expect_lt(fit$iter, 1000)

  squared_distances <- vapply(
    seq_len(nrow(fit$centers)),
    function(j) {
      centered <- sweep(X, 2L, fit$centers[j, ], FUN = "-")
      rowSums(centered^2)
    },
    numeric(nrow(X))
  )
  reconstructed_objective <- sum(
    fit$membership^2 * squared_distances
  ) / nrow(X)
  expect_equal(fit$withinerror, reconstructed_objective, tolerance = 1e-10)
})

test_that("the iteration cap used by the convergence gate is pinned", {
  set.seed(123)
  X <- matrix(rnorm(400), ncol = 4)
  set.seed(777)
  capped <- e1071::cmeans(
    X,
    centers = 3,
    iter.max = 4,
    m = 2,
    control = list(reltol = 1e-30)
  )

  expect_identical(as.integer(capped$iter), 4L)
})

test_that("retired interfaces are absent and the corrected reader is optional", {
  namespace <- asNamespace("AMDconfigurations")
  expect_false(exists("estimate_sigma_equivalent", envir = namespace,
                      inherits = FALSE))
  expect_false(exists("estimate_sigma_equivalent_mc", envir = namespace,
                      inherits = FALSE))
  expect_false("definition" %in% names(formals(amd_score)))
  expect_false("definition" %in% names(formals(compute_amd_curve)))
  expect_true(all(c("load_amd_predictor", "predict_amd") %in%
                    getNamespaceExports("AMDconfigurations")))
  expect_false("ranger" %in% names(getNamespaceImports(namespace)))
  expect_false(exists("amd_predictor_bundle_v2", envir = namespace,
                      inherits = FALSE))
})
