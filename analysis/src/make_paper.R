# ------------------------------------------------------------------------------
#' Compile paper for submission
# ------------------------------------------------------------------------------

library(here)
source("R/split_render.R")

# Get the submission files
split_render(manuscript = "analysis/paper/manuscript.Rmd",
             output_dir ="analysis/paper/subfiles")

# Internal steps
# Convert to pdf (using system & this:
# stack exchange answer)
system("rm -r analysis/paper/subfiles/*.pdf") # clean first
system("bash analysis/paper/subfiles/word2pdf.sh")

# Push up to google drive (single document)
rmarkdown::render("analysis/paper/manuscript.Rmd",
                  output_format = "bookdown::word_document2")
googledrive::drive_update('MadaAccess', 'analysis/paper/manuscript.docx')
