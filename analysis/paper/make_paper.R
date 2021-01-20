# ------------------------------------------------------------------------------
#' Compile paper for submission
# ------------------------------------------------------------------------------

start <- Sys.time()
library(here)
library(knitr)
source("R/split_render.R")

# For revisions
rmarkdown::render(here("analysis/paper/minor_revision_1/response_reviewers.Rmd"),
                  output_dir = here("analysis/paper/minor_revision_1"))

# upload to google drive for feedback
googledrive::drive_update('MadaAccess', here('analysis/paper/minor_revision_1/manuscript_supplement_tracked.docx'))
googledrive::drive_put(here('analysis/paper/minor_revision_1/response_reviewers.docx'), 'MadaAccess_response')

# For submitting
split_render(manuscript = "analysis/paper/manuscript.Rmd",
             output_dir ="analysis/paper/minor_revision_1")

# Internal steps
# Convert to pdf (using system & this applescript: https://superuser.com/a/1559043
# superuser answer)
system("rm -r analysis/paper/minor_revision_1/*.pdf") # clean first
system("bash analysis/paper/minor_revision_1/word2pdf.sh")

