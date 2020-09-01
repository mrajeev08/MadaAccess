#' Summarize mcmc chains into a data table
#' Loads all mcmc samples stored in a folder and combines them into one data.table with 
#' additional columns for model attributes.
#' @param parent_dir the directory to look for the mcmc files
#' @return A data.table with all samples melted and with iteration number, chain, and model attributes
#' @section Dependencies:
#'     data.table, coda
summarize.samps <- function(parent_dir = "output/mods/samps/") {
  
  require(coda)
  
  # list files
  files <- list.files(parent_dir, recursive = TRUE)

  # Response var structure
  file_dt <-data.table(filenames = files, 
                       pop_predict = case_when(grepl("flatPop", files, fixed = TRUE) ~ "flatPop",
                                               grepl("addPop", files, fixed = TRUE) ~ "addPop",
                                               grepl("onlyPop", files, fixed = TRUE) ~ "onlyPop"),
                       scale = case_when(grepl("Commune", files, fixed = TRUE) ~ "Commune",
                                         grepl("District", files, fixed = TRUE) ~ "District"), 
                       intercept = case_when(grepl("fixed", files, fixed = TRUE) ~ "fixed",
                                            grepl("random", files, fixed = TRUE) ~ "random"), 
                       data_source = case_when(grepl("Moramanga", parent_dir, 
                                                     fixed = TRUE) ~ "Moramanga",
                                               grepl("National", parent_dir, 
                                                     fixed = TRUE) ~ "National"),
                       OD = ifelse(grepl("_OD", files, fixed = TRUE), TRUE, FALSE))
  
  # Pull in samples
  all_samps <- foreach(j = iter(file_dt, by = "row"), .combine = rbind) %do% {
    filename <- paste0(parent_dir, j$filenames)
    samples <- readRDS(filename)[[1]]
    samples <- as.matrix(samples, iters = TRUE, chains = TRUE)
    samples <- melt(data.table(samples), id.vars = c("CHAIN", "ITER"))
    data.table(samples, j)
  }
  
  setnames(all_samps, c("CHAIN", "ITER", "variable"), c("Chain", "Iteration", "Parameter"))
  
  return(all_samps)
}

