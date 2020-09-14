# Utility functions for this project --------------------------------------
##' Edited Nov 2019
##' Malavika Rajeev

## Function for writing out session info at end of each script run
out.session <- function(logfile = "log.csv", start = NULL, ncores = 1) {

  # find the path (only works if script is sourced or run from cmd line)
  path <- this_file()

  if (is.null(path)) {
    print("Running interactively, either use source or Rscript on the command line
          to save session info.")
  } else {
    out <- devtools::session_info()
    versions <- data.table::as.data.table(t(out$packages$loadedversion))
    colnames(versions) <- out$packages$package

    max_mem <- obj_size(sum(gc()[, ncol(gc())]))

    if (!is.null(start)) {
      jobtime <- timing(as.numeric(Sys.time() - start, units = "mins"))
    } else {
      jobtime <- NULL
    }

    toappend <- data.table::data.table(
      ran = timestamp(),
      timestamp = format(Sys.time(), "%Y%m%d_%H%M%S"),
      jobtime, max_mem, ncores,
      path, r_version = out$platform$version,
      os = out$platform$os, system = out$platform$system,
      timezone = out$platform$tz, versions
    )

    if (file.exists(logfile)) {
      existing <- data.table::fread(logfile)
      towrite <- rbind(existing, toappend, fill = TRUE)
      data.table::fwrite(towrite, logfile)
    } else {
      write_create(toappend, logfile, data.table::fwrite)
    }
  }
}


obj_size <- function(mb) {
  ifelse(mb / 1000 > 1, paste(round(mb / 1000, 2), "Gb"), paste(round(mb, 2), "Mb"))
}

timing <- function(min) {
  if (min / 60 > 1) tim <- paste(round(min / 60, 2), "hr")
  if (min * 60 < 60 & min * 60 > 1) tim <- paste(round(min * 60, 2), "sec")
  if (min * 60 <= 1) tim <- paste(round(min * 60 * 1000, 2), "ms")
  if (min / 60 <= 1 & min * 60 >= 60) tim <- paste(round(min, 2), "min")

  tim
}

# From stack exchange:
# https://stackoverflow.com/questions/1815606/determine-path-of-the-executing-script
# I want the relative path!

this_file <- function() {
  cmdArgs <- commandArgs(trailingOnly = FALSE)
  match <- sub("--file=", "", cmdArgs[grep("--file=", cmdArgs)])
  sysf <- sys.frames()[[1]]$ofile

  # Rscript
  if (length(match) > 0) {
    return(match)
  }

  # 'source'd via R console
  if (!is.null(sysf)) {
    return(trimws(sub(here::here(), "", path.expand(sysf)), whitespace = "/"))
  }

  if (is.null(sysf) & length(match) == 0) {
    return(NULL)
  }
}

write_create <- function(obj, path, write_function, ...) {
  dir_name <- dirname(path)

  if (!dir.exists(dir_name)) {
    dir.create(dir_name, recursive = TRUE)
  }

  if (is.character(write_function)) {
    get(write_function)(obj, path, ...)
  }

  if (is.function(write_function)) {
    write_function(obj, path, ...)
  }

  if (!is.function(write_function) & !is.character(write_function)) {
    print("Error: arg write_function is not a function or is not loaded!")
  }
}

# a ggsave function with arguments switched
ggsave_it <- function(plot, filename, ...) {
  ggsave(filename, plot, ...)
}

# getting safe paths with here without using comma's
here_safe <- function(path, sep = "/") {
  path_list <- unlist(strsplit(path, sep))
  do.call(here::here, as.list(path_list))
}

# setting up cluster vs local (I'm not sure if this will work because of environments)

setup_cl <- function(slurm = Sys.getenv("SLURM_JOB_ID") != "",
                     args = commandArgs(trailingOnly = TRUE),
                     local_logfile = "test_local.csv",
                     cluster_logfile = "test_cluster.csv",
                     mpi = TRUE) {
  start <- Sys.time()

  if (!slurm) {
    logfile <- local_logfile

    if (!is.na(args[1])) {
      # set up local cluster
      ncores <- parallel::detectCores() - 1
      make_cl <<- function() {
        parallel::makeCluster(parallel::detectCores() - 1)
      }
      register_cl <<- doParallel::registerDoParallel
      close_cl <<- parallel::stopCluster
    }

    if (is.na(args[1])) {
      # Look for the name of the script and how long it will take in the log
      ncores <- 1
      make_cl <<- function() {} # empty func
      close_cl <<- register_cl <<- function(cl) {} # dummy functions
      print("Running serially (not in parallel!)")
    }
  }

  if (slurm) {
    ncores <- as.numeric(Sys.getenv("SLURM_NTASKS"))
    logfile <- cluster_logfile

    if (mpi == TRUE) {
      ncores <- ncores - 1
      make_cl <<- doMPI::startMPIcluster
      register_cl <<- doMPI::registerDoMPI
      close_cl <<- function(cl) {
        doMPI::closeCluster(cl)
        Rmpi::mpi.quit()
      }
    }

    if (mpi == FALSE) {
      make_cl <<- function() {
        parallel::makeCluster(parallel::detectCores() - 1)
      }
      register_cl <<- doParallel::registerDoParallel
      close_cl <<- parallel::stopCluster
    }
  }

  print("Setup complete, remember that you did some global function assignments
        (make_cl, register_cl, & close_cl)!")
  return(list(
    start = start, ncores = ncores, slurm = slurm,
    logfile = logfile
  ))
}
