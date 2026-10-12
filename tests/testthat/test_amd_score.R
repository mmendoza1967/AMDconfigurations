test_that("amd_score matches the mathematical definition", {
  U <- rbind(
    c(0.8, 0.2),
    c(0.6, 0.4),
    c(0.5, 0.5)
  )

  expected <- mean(c(0.8, 0.6, 0.5)) - 1 / 2
  expect_equal(amd_score(U), expected)
})

test_that("uniform and crisp memberships attain the analytical bounds", {
  uniform <- matrix(1 / 4, nrow = 10, ncol = 4)
  crisp <- diag(4)[rep(1:4, length.out = 12), , drop = FALSE]

  expect_equal(amd_score(uniform), 0)
  expect_equal(amd_score(crisp), 1 - 1 / 4)
})

test_that("a one-configuration membership matrix has AMD zero", {
  expect_equal(amd_score(matrix(1, nrow = 12, ncol = 1)), 0)
})

test_that("AMD is invariant to order and complete duplication", {
  U <- rbind(
    c(0.7, 0.2, 0.1),
    c(0.1, 0.3, 0.6),
    c(0.2, 0.7, 0.1)
  )

  expect_equal(amd_score(U), amd_score(U[c(3, 1, 2), c(2, 3, 1)]))
  expect_equal(amd_score(U), amd_score(U[rep(seq_len(nrow(U)), 2), ]))
})

test_that("invalid membership matrices are rejected", {
  expect_error(amd_score(matrix(c(0.2, 0.2), nrow = 1)), "sum to one")
  expect_error(amd_score(matrix(c(1.1, -0.1), nrow = 1)), "lie in")
  expect_error(amd_score(matrix(c(NA_real_, 1), nrow = 1)), "non-finite")
  expect_error(amd_score("not a matrix"), "matrix or data frame")
})
