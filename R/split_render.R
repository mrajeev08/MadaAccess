
split_render <- function(manuscript = "analysis/paper/manuscript.Rmd",
                         output_dir ="analysis/paper/subfiles",
                         write_man = TRUE) {

  # and render both
  if(!dir.exists(here::here(output_dir))) {
    dir.create(here::here(output_dir))
  }

  single <- readLines(manuscript)
  split <- grep("<!-- split -->", single)

  stitles <- single[grep("## S[0-9]", single)]
  stitles <- gsub("#", "", stitles)

  for(i in seq_len(length(split))) {

    start <- split[i]
    end <- ifelse(i == length(split), length(single), split[i + 1])
    supplement <- single[start:end]
    supplement <- supplement[-grep("## S[0-9]", supplement)]

    # Also add the first set-up code chunk!
    inds <- grep("```", single)
    supplement <- c(single[inds[1]:inds[2]], supplement)
    supplement <- c(readLines(here::here("analysis/paper/format/supplement.yaml")),
                    supplement)

    s_name <- paste0("S", i, "_", "text.Rmd")

    writeLines(supplement, here::here("analysis/paper", s_name))

    rmarkdown::render(here::here("analysis/paper", s_name),
                      output_dir = here::here(output_dir),
                      params = list(nofig = FALSE, snum = i,
                                    stitle = stitles[i]))

    unlink(here::here("analysis/paper", s_name))

  }

  if(write_man) {

    # Handle the manuscript
    man <- single[1:(split[1]-1)]

    # handle the supplement but with (no fig = TRUE & toc = false)
    suppl <- single[split[1]:length(single)]
    suppl <- suppl[-grep("newpage|:::|## S7 Text|### Software references|### Additional supplementary",
                         suppl)]
    suppl <- c("## List of supplementary figures and tables", suppl)

    # take out yaml from first one
    inds <- grep("---", man)
    man <- man[-c(inds[1]:inds[2])]
    man <- c(readLines(here::here("analysis/paper/format/main.yaml")),
             man)
    man <- c(man, suppl)

    writeLines(man, here::here("analysis/paper/man.Rmd"))

    rmarkdown::render(here::here("analysis/paper/man.Rmd"),
                      output_dir = here::here(output_dir),
                      params = list(nofig = TRUE))

    unlink(here::here("analysis/paper/man.Rmd"))

  }


}
