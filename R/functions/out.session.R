# Utility functions for this project --------------------------------------
##' Edited Nov 2019
##' Malavika Rajeev

## Function for writing out session info at end of each script run
out.session <- function(path, filename = "log.csv", start = NULL) {
  
  
  library(data.table) ## using data table to deal with easy parsing / binding
  out <- sessionInfo()
  versions <- lapply(out$otherPkgs, function(x) x["Version"])
  versions <- as.data.table(t(unlist(versions)))
  names(versions) <- gsub(".Version", "", names(versions))
  
  out_gc <- out
  max_mem_mb <- sum(gc()[, ncol(gc())])
  
  if (!is.null(start)) {
    jobtime_mins <- as.numeric(Sys.time() - start, units = "mins")
  } else {
    jobtime_mins <- NULL
  }
  
  toappend <- data.table(ran = timestamp(), 
                         timestamp = format(Sys.time(), "%Y%m%d_%H%M%S"),
                         jobtime_mins, max_mem_mb,
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