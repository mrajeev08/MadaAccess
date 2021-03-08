# ------------------------------------------------------------------------------
#' Compile paper for submission
# ------------------------------------------------------------------------------

library(here)
library(knitr)
source("R/split_render.R")

# For submitting final proofs (supplementary materials only!)
split_render(output_dir = "analysis/paper/final_proof", write_man = TRUE)

# Internal steps
# Convert to pdf (using system & this applescript: https://superuser.com/a/1559043
# superuser answer)
system("rm -r analysis/paper/final_proof/*.pdf") # clean first
system("bash analysis/paper/final_proof/word2pdf.sh")

