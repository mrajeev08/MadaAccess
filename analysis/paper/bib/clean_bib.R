# Clean up bib files
library(citr)
tidy_bib_file("docs/manuscript.rmd", "docs/bib/refs_all.bib", file = "docs/bib/refs_all.bib",
              encoding = getOption("citr.encoding"))
