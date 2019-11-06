## Function to summarize results in output directory (or in other directories accordingly)
## Subset outside of function if you want to run on only bits of it
get.samps <- function(parent_dir = "output/samps/", 
                              files = list.files("output/samps", recursive = TRUE)) {
  ## testing
  # parent_dir <- "output/scales/"
  # files <- list.files("output", recursive = TRUE)
  files <- files[!grepl("dic", files, fixed = TRUE)]
  
  ## Response var structure
  pop_predict <- case_when(grepl("flatPop", files, fixed = TRUE) ~ "flatPop",
                        grepl("addPop", files, fixed = TRUE) ~ "addPop",
                        grepl("onlyPop", files, fixed = TRUE) ~ "onlyPop")
  
  ## Location 
  intercept <- case_when(grepl("fixed", files, fixed = TRUE) ~ "fixed",
                        grepl("random", files, fixed = TRUE) ~ "random")
  
  ## Scale 
  scale <- case_when(grepl("Commune", files, fixed = TRUE) ~ "Commune",
                     grepl("District", files, fixed = TRUE) ~ "District")
  ## Structure 
  data_source <- case_when(grepl("Moramanga", files, fixed = TRUE) ~ "Moramanga",
                         grepl("National", files, fixed = TRUE) ~ "National")
 
  ## file data table
  file_dt <- as.data.frame(list(filenames = files, pop_predict = pop_predict, scale = scale,
                                intercept = intercept, data_source = data_source))
  
  ## Pull in samples
  all_samps <- foreach(i = 1:nrow(file_dt)) %do% {
    samples <- readRDS(paste0(parent_dir, file_dt$filenames[i]))
    samples <- as.data.frame(do.call(rbind, samples[[1]]))
    samples$scale <- file_dt$scale[i]
    samples$data_source <- file_dt$data_source[i]
    samples$intercept <- file_dt$intercept[i]
    samples$pop_predict <- file_dt$pop_predict[i]
    samples
  }
  
  all_samps <- bind_rows(all_samps)
  
  return(all_samps)
}
