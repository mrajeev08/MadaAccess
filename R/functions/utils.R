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

## ggplot helpers for reordering within facets
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
