test_that("the reproducibility manifest records the required environment", {
  manifest <- amd_reproducibility_manifest()

  expect_s3_class(manifest, "amd_reproducibility_manifest")
  expect_identical(manifest$schema_version, 2L)
  expect_true(manifest$manifest_generation >= 1L)
  expect_match(
    manifest$manifest_id,
    "^amd-[0-9]+T[0-9]+Z-p[0-9]+-g[0-9]+$"
  )
  expect_match(
    manifest$created_at_utc,
    "^[0-9]{4}-[0-9]{2}-[0-9]{2}T[0-9:.]+Z$"
  )
  expect_identical(manifest$process_id, Sys.getpid())
  expect_identical(manifest$r_version, R.version.string)
  expect_identical(
    manifest$amdconfigurations_version,
    as.character(utils::packageVersion("AMDconfigurations"))
  )
  expect_identical(
    manifest$e1071_version,
    as.character(utils::packageVersion("e1071"))
  )
  expect_identical(manifest$rng_kind, RNGkind())
  expect_s3_class(manifest$session_info, "sessionInfo")
})

test_that("creating and reusing a manifest preserves random-number state", {
  runtime_state <- get(
    ".amd_runtime_state",
    envir = asNamespace("AMDconfigurations")
  )
  previous_names <- ls(runtime_state, all.names = TRUE)
  previous_values <- mget(
    previous_names,
    envir = runtime_state,
    inherits = FALSE
  )
  if (length(previous_names) > 0L) {
    rm(list = previous_names, envir = runtime_state)
  }
  on.exit({
    current_names <- ls(runtime_state, all.names = TRUE)
    if (length(current_names) > 0L) {
      rm(list = current_names, envir = runtime_state)
    }
    list2env(previous_values, envir = runtime_state)
  }, add = TRUE)

  set.seed(6021)
  state_before <- .Random.seed

  first <- amd_reproducibility_manifest()
  expect_identical(.Random.seed, state_before)
  second <- amd_reproducibility_manifest()

  expect_identical(first, second)
  expect_identical(.Random.seed, state_before)
})

test_that("changing RNGkind creates and links a new manifest", {
  original_kind <- RNGkind()
  on.exit(do.call(RNGkind, as.list(original_kind)), add = TRUE)

  first <- amd_reproducibility_manifest()
  RNGkind("Wichmann-Hill")
  second <- amd_reproducibility_manifest()

  expect_false(identical(first$manifest_id, second$manifest_id))
  expect_identical(second$process_id, first$process_id)
  expect_identical(second$rng_kind, RNGkind())

  X <- matrix(seq_len(60), ncol = 3)
  curve <- compute_amd_curve(
    X, its = 1, nin = 2, nsp = 2, seeds = 19L,
    iter_max = 1000, verbose = FALSE
  )
  expect_identical(curve$manifest_id, second$manifest_id)
})
