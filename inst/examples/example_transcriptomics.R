## Transcriptomic example: tracked preselection and explicit extraction

library(AMDconfigurations)

data(Transcdata_small)
X <- as.matrix(Transcdata_small)

candidate_c <- 2:10
starts <- 10L
seed_matrix <- outer(
  seq_len(starts),
  candidate_c,
  function(start, c) 300000L + 1000L * c + start
)
colnames(seed_matrix) <- as.character(candidate_c)

curve <- compute_amd_curve(
  X,
  its = starts,
  nin = min(candidate_c),
  nsp = max(candidate_c),
  seeds = seed_matrix,
  preselect_top_sd = 200,
  scale_data = TRUE,
  iter_max = 1000,
  verbose = TRUE
)

plot(curve, open_device = FALSE)
print(data.frame(c = candidate_c, AMD = unname(curve$curve)))

## The curve records exactly which transcripts were retained and the centering
## and scaling vectors used for the fit.
curve$settings$retained_column_names

## This value demonstrates the extraction API only. It is not an automatic
## inference from c_peak. A scientific analysis must replace it with a value
## justified from the complete curve and its diagnostics.
chosen_c <- 3L
assignment <- assign_configurations(curve, c = chosen_c)

cluster_labels <- rep(NA_integer_, nrow(X))
cluster_labels[assignment$complete_rows] <- assignment$cluster

## Retrieve the manifest immediately after each curve and save one file for
## every distinct identifier in a production analysis:
## manifest <- amd_reproducibility_manifest()
## saveRDS(manifest, paste0(manifest$manifest_id, ".rds"))
