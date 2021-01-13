
split_render <- function(manuscript = "analysis/paper/manuscript.Rmd",
                         output_dir ="analysis/paper/subfiles") {
  single <- readLines(manuscript)
  split <- grep("<!-- split -->", single)
  man <- single[1:(split-1)]
  supplement <- single[(split-1):length(single)]

  # take out yaml from first one
  inds <- grep("---", man)
  man <- man[-c(inds[1]:inds[2])]
  man <- c(readLines(here::here("analysis/paper/format/main.yaml")),
           man)

  writeLines(man, here::here("analysis/paper/man.Rmd"))

  # and level 1 header from supplement
  inds <- grep("# Supplementary Appendix", supplement)
  supplement <- supplement[-c(inds[1])]

  # Also add the first set-up code chunk!
  inds <- grep("```", single)
  supplement <- c(single[inds[1]:inds[2]], supplement)
  supplement <- c(readLines(here::here("analysis/paper/format/supplement.yaml")),
                  supplement)

  writeLines(supplement, here::here("analysis/paper/supplement.Rmd"))

  # and render both
  if(!dir.exists(here::here(output_dir))) {
    dir.create(here::here(output_dir))
  }

  rmarkdown::render(here::here("analysis/paper/supplement.Rmd"),
                    output_dir = here::here(output_dir))
  rmarkdown::render(here::here("analysis/paper/man.Rmd"),
                    output_dir = here::here(output_dir))

  # clean up files
  unlink(here::here("analysis/paper/supplement.Rmd"))
  unlink(here::here("analysis/paper/man.Rmd"))
}
