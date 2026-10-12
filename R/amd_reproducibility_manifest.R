#' Record the reproducibility environment for an AMD process
#'
#' Creates a compact record of the software and random-number environment used
#' by AMD. The manifest is created once and then reused within the current R
#' process while its random-number generator remains unchanged. A different
#' process, including each parallel worker, or a change to `RNGkind()` receives
#' a different manifest identifier.
#'
#' [compute_amd_curve()] records only the manifest identifier, rather than
#' duplicating the complete session information in every curve. Retrieve the
#' manifest immediately after each curve and save one copy of every distinct
#' `manifest_id`. Curves sharing an identifier can share the saved copy.
#'
#' @return An object of class `"amd_reproducibility_manifest"` containing a
#'   schema version, a unique environment identifier, its generation and UTC
#'   creation time, the process identifier, R and package versions,
#'   `RNGkind()`, and `sessionInfo()`.
#'
#' @examples
#' manifest <- amd_reproducibility_manifest()
#' manifest$manifest_id
#' @export
amd_reproducibility_manifest <- function() {
  process_id <- Sys.getpid()
  current_rng_kind <- RNGkind()
  has_current_manifest <-
    exists("manifest", envir = .amd_runtime_state, inherits = FALSE) &&
    identical(.amd_runtime_state$manifest$process_id, process_id) &&
    identical(.amd_runtime_state$manifest$rng_kind, current_rng_kind)

  if (!has_current_manifest) {
    generation <- if (
      exists("generation", envir = .amd_runtime_state, inherits = FALSE)
    ) {
      .amd_runtime_state$generation + 1L
    } else {
      1L
    }
    created_at <- Sys.time()
    timestamp <- format(
      created_at,
      format = "%Y%m%dT%H%M%OS6",
      tz = "UTC"
    )
    timestamp <- gsub("[^0-9T]", "", timestamp)

    manifest <- list(
      schema_version = 2L,
      manifest_id = paste0(
        "amd-", timestamp, "Z-p", process_id, "-g", generation
      ),
      manifest_generation = generation,
      created_at_utc = format(
        created_at,
        format = "%Y-%m-%dT%H:%M:%OS6Z",
        tz = "UTC"
      ),
      process_id = process_id,
      r_version = R.version.string,
      amdconfigurations_version = as.character(
        utils::packageVersion("AMDconfigurations")
      ),
      e1071_version = as.character(utils::packageVersion("e1071")),
      rng_kind = current_rng_kind,
      session_info = utils::sessionInfo()
    )
    class(manifest) <- c("amd_reproducibility_manifest", "list")
    .amd_runtime_state$generation <- generation
    .amd_runtime_state$manifest <- manifest
  }

  .amd_runtime_state$manifest
}
