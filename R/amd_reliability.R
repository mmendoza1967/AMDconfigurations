# Calibrator outputs describe a reference simulation regime. Numerical-grid
# membership alone cannot establish transport to the distribution being analysed.
.amd_reference_reliability <- function(calibrated, applicability, supported_k,
                                       expected_missingness, expected_constants,
                                       allow_extrapolation) {
  status <- if (!supported_k) {
    "suppressed_outside_observed_output_support"
  } else if (!expected_missingness) {
    "suppressed_unassessed_missingness_pattern"
  } else if (!expected_constants) {
    "suppressed_unassessed_constant_predictors"
  } else if (applicability$on_validated_grid) {
    "conditional_simulation_reference"
  } else if (allow_extrapolation) {
    paste0("reference_only_", applicability$numerical_status)
  } else {
    paste0("suppressed_", applicability$numerical_status)
  }
  eligible <- supported_k && expected_missingness && expected_constants &&
    (applicability$on_validated_grid || allow_extrapolation)
  values <- if (eligible) calibrated else c(structure = NA_real_, exact = NA_real_)
  list(
    one_several = unname(values[["structure"]]),
    exact_k = unname(values[["exact"]]),
    status = status,
    interpretation = paste(
      "Continuous estimated correctness conditional on the reference simulations;",
      "transport to the analysed distribution has not been established."
    )
  )
}
