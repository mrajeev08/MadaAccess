# Utility functions for this project --------------------------------------
##' Edited Nov 2019
##' Malavika Rajeev

## Function for writing out session info at end of each script run
out.session <- function(path, filename = "log.csv", start) {
  
  library(data.table) ## using data table to deal with easy parsing / binding
  out <- sessionInfo()
  versions <- lapply(out$otherPkgs, function(x) x["Version"])
  versions <- as.data.table(t(unlist(versions)))
  names(versions) <- gsub(".Version", "", names(versions))
  
  if (!is.null(start)) {
    jobtime <- as.numeric(Sys.time() - start, units = "mins")
  } else {
    jobtime <- NA
  }
  
  toappend <- data.table(ran = timestamp(), 
                         timestamp = format(Sys.time(), "%Y%m%d_%H%M%S"),
                         jobtime = jobtime, 
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