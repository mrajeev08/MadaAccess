## Clean up bib files
library(citr)
tidy_bib_file("docs/main_temp.rmd", "docs/bib/refs_all.bib", file = "docs/bib/refs_main.bib",
              encoding = getOption("citr.encoding"))
tidy_bib_file("docs/supp_temp.rmd", "docs/bib/refs_all.bib", file = "docs/bib/refs_supp.bib",
              encoding = getOption("citr.encoding"))
