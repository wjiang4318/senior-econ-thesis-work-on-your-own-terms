# ==============================================================================
# run_all.R -- reproduce every figure and table from the raw extract
#
# Usage, from the project root:
#   Rscript R/run_all.R
# or, in RStudio with the .Rproj open:
#   source("R/run_all.R")
# ==============================================================================

stopifnot(requireNamespace("here", quietly = TRUE))

scripts <- c("01_prepare_data.R", "02_figures.R", "03_tables.R")

for (s in scripts) {
  message("\n=== ", s, " ===")
  source(here::here("R", s), echo = FALSE)
}

message("\nDone. See output/figures/ and output/tables/.")
