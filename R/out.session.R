# Utility functions for this project --------------------------------------
##' Edited Nov 2019
##' Malavika Rajeev

## Function for writing out session info at end of each script run
out.session <- function(path, filename = "log.csv", start = NULL) {
  
  out <- sessionInfo()
  versions <- lapply(out$otherPkgs, function(x) x["Version"])
  versions <- as.data.table(t(unlist(versions)))
  names(versions) <- gsub(".Version", "", names(versions))
  
  out_gc <- out
  max_mem <- obj_size(sum(gc()[, ncol(gc())]))
  
  if (!is.null(start)) {
    jobtime <- timing(as.numeric(Sys.time() - start, units = "mins"))
  } else {
    jobtime <- NULL
  }
  
  toappend <- data.table(ran = timestamp(), 
                         timestamp = format(Sys.time(), "%Y%m%d_%H%M%S"),
                         jobtime, max_mem,
                         path, R.version = out$R.version$version.string,
                         platform = out$platform, running = out$running, versions)
  
  if(file.exists(filename)) {
    existing <- fread(filename)
    towrite <- rbind(existing, toappend, fill = TRUE)
    fwrite(towrite, filename)
  } else {
    write_create(toappend, filename, fwrite)
  }
}


obj_size <- function(mb) {
  ifelse(mb/1000 > 1, paste(round(mb/1000, 2), "Gb"), paste(round(mb, 2), "Mb"))
}

timing <- function(min) {
  if(min/60 > 1) tim <- paste(round(min/60, 2), "hr")
  if(min*60 < 60 & min*60 > 1) tim <- paste(round(min*60, 2), "sec")
  if(min*60 <= 1) tim <- paste(round(min*60*1000, 2), "ms")
  if(min/60 <= 1 & min*60 >=60 ) tim <- paste(round(min, 2), "min")
  
  tim
}
