# ------------------------------------------------------------------------------
#' Compile paper for submission
# ------------------------------------------------------------------------------

start <- Sys.time()
library(here)
library(knitr)
source("R/split_render.R")

# Get the submission files
split_render(manuscript = "analysis/paper/manuscript.Rmd",
             output_dir ="analysis/paper/subfiles")

# Internal steps
# Convert to pdf (using system & this applescript: https://superuser.com/a/1559043
# superuser answer)
system("rm -r analysis/paper/subfiles/*.pdf") # clean first
system("bash analysis/paper/subfiles/word2pdf.sh")

# Push up to google drive (single document)
rmarkdown::render("analysis/paper/manuscript.Rmd",
                  output_format = "bookdown::word_document2")
googledrive::drive_update('MadaAccess', 'analysis/paper/manuscript.docx')

# cover letter
rmarkdown::render(here("analysis/paper/cover_letter.Rmd"),
                  output_dir = here("analysis/paper/subfiles"))
