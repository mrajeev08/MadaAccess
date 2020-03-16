# Function to summarize results in output directory (or in other directories accordingly)
# Subset outside of function if you want to run on only bits of it
get.samps <- function(parent_dir = "output/mods/samps/") {
  
  # list files
  files <- list.files(parent_dir, recursive = TRUE)

  # Response var structure
  pop_predict <- case_when(grepl("flatPop", files, fixed = TRUE) ~ "flatPop",
                        grepl("addPop", files, fixed = TRUE) ~ "addPop",
                        grepl("onlyPop", files, fixed = TRUE) ~ "onlyPop")
  
  # Location 
  intercept <- case_when(grepl("fixed", files, fixed = TRUE) ~ "fixed",
                        grepl("random", files, fixed = TRUE) ~ "random")
  
  # Scale 
  scale <- case_when(grepl("Commune", files, fixed = TRUE) ~ "Commune",
                     grepl("District", files, fixed = TRUE) ~ "District")
  # Data source 
  data_source <- case_when(grepl("Moramanga", parent_dir, fixed = TRUE) ~ "Moramanga",
                         grepl("National", parent_dir, fixed = TRUE) ~ "National")
 
  # file data table
  file_dt <- as.data.frame(list(filenames = files, pop_predict = pop_predict, scale = scale,
                                intercept = intercept, data_source = data_source))
  
  # Pull in samples
  all_samps <- foreach(j = iter(file_dt, by = "row"), .combine = bind_rows) %do% {
    filename <- paste0(parent_dir, j$filenames)
    samples <- readRDS(filename)
    samples <- ggmcmc::ggs(samples[[1]])
    samples$scale <- j$scale
    samples$data_source <- j$data_source
    samples$intercept <- j$intercept
    samples$pop_predict <- j$pop_predict
    samples$filename  <- filename
    samples
  }
 
  return(all_samps)
}

