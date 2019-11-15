# Utility functions for this project --------------------------------------
##' Edited Nov 2019
##' Malavika Rajeev

## Function for writing out session info at end of each script run
out.session <- function(path, filename = "sessionInfo.csv") {
  library(data.table) ## using data table to deal with fast binding
  out <- sessionInfo()
  versions <- lapply(out$otherPkgs, function(x) x["Version"])
  versions <- as.data.table(t(unlist(versions)))
  names(versions) <- gsub(".Version", "", names(versions))
  toappend <- data.table(ran = timestamp(), 
                         timestamp = format(Sys.time(), "%Y%m%d_%H%M%S"),
                         path, R.version = out$R.version$version.string,
                         platform = out$platform, running = out$running, versions)
  if(file.exists(filename)) {
    existing <- fread(filename)
    towrite <- rbind(existing, toappend, fill = TRUE)
    fwrite(towrite, filename)
  } else {
    fwrite(toappend, filename)
  }
}

## ggplot helpers fir reordering within facets
reorder_within <- function(x, by, within, fun = mean, sep = "___", ...) {
  new_x <- paste(x, within, sep = sep)
  stats::reorder(new_x, by, FUN = fun)
}

scale_x_reordered <- function(..., sep = "___") {
  reg <- paste0(sep, ".+$")
  ggplot2::scale_x_discrete(labels = function(x) gsub(reg, "", x), ...)
}

scale_y_reordered <- function(..., sep = "___") {
  reg <- paste0(sep, ".+$")
  ggplot2::scale_y_discrete(labels = function(x) gsub(reg, "", x), ...)
}

## Find nth most recent file
find.bydate <- function(path, patt, ind, rank){
  ## function to get latest file
  ## path = the folder to look in, pattern = the pattern to match to
  ## ind = where the date is stored in the name if you were to strsplit by '_'
  ## rank = which file do you want? most recent = 1, second most recent = 2, etc.
  ## will print warning if rank is greater than the # of files, but will give you the oldest one
  
  files <- list.files(path, pattern = patt)
  
  if (rank > length(files)){
    print("Warning: rank greater than # of files, returning oldest file")
    rank <- length(files)
  }
  
  time_stamp <- rep (NA, length(files))
  
  for (i in 1:length(files)){
    time_stamp[i] <- unlist(strsplit(files[i], "_"))[ind]
  }
  
  time_stamp <- substr(as.character(time_stamp), 1, 8)
  diff <- as.numeric(Sys.Date() - as.Date(time_stamp, "%Y%m%d"))
  files <- files[order(diff)]
  retrieved <- read.csv(paste0(path, files[rank]), fileEncoding = 'latin1')
  print(paste0(path, files[rank]))
  return(retrieved)
}