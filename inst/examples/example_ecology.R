## Ecological example: AMD curve and explicit extraction

library(AMDconfigurations)

data(Fulldata)

trophic_columns <- c(
  "IFd", "GIn", "HIn", "IFr", "Crn", "Pln", "Frg", "Nct", "Grn"
)
X <- as.matrix(Fulldata[, trophic_columns])

candidate_c <- 2:10
starts <- 10L
seed_matrix <- outer(
  seq_len(starts),
  candidate_c,
  function(start, c) 200000L + 1000L * c + start
)
colnames(seed_matrix) <- as.character(candidate_c)

curve <- compute_amd_curve(
  X,
  its = starts,
  nin = min(candidate_c),
  nsp = max(candidate_c),
  seeds = seed_matrix,
  iter_max = 1000,
  verbose = TRUE
)

plot(curve, open_device = FALSE)
print(data.frame(c = candidate_c, AMD = unname(curve$curve)))

## This value demonstrates the extraction API only. It is not an automatic
## inference from c_peak. A scientific analysis must replace it with a value
## justified from the complete curve and its diagnostics.
chosen_c <- 3L
assignment <- assign_configurations(curve, c = chosen_c)

## Restore alignment with the original input if rows were omitted.
cluster_labels <- rep(NA_integer_, nrow(X))
cluster_labels[assignment$complete_rows] <- assignment$cluster

result <- data.frame(Fulldata, amd_configuration = cluster_labels)

## Retrieve the manifest immediately after each curve and save one file for
## every distinct identifier in a production analysis:
## manifest <- amd_reproducibility_manifest()
## saveRDS(manifest, paste0(manifest$manifest_id, ".rds"))
