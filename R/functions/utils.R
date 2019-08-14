# Utility functions for this project --------------------------------------
# MALAVIKA RAJEEV MAY 2016 ------------------------------------------------

# Getting consecutive dates
get.consec <- function (date, format_date, start = "01-01-2002", format_start = "%d-%m-%Y",
                      year1 = 2002, tstep = "bwn", period = FALSE, get.info = FALSE, check = FALSE) {
  
  if (get.info == TRUE) {
    cat ("get.consec
        *For getting consecutive time steps from a set start date* 

        Arguments:
        get.consec (date, format_date, start = '01-01-2002', format_start = '%d-%m-%Y',
                      year1 = 2002, tstep = c('bwn', 'day', 'week', 'month'), period = FALSE, get.info = TRUE, check = FALSE)
        - required: date (your date or vector of dates)
                    format_date (format of your date)
        - defaults: tstep = 'bwn', check = FALSE

        Notes:
        - if tstep = 'bwn', gets date in biweeks, other options are 'day', 'week', 'month'
        - if period = FALSE then uses calendar based functions (i.e. ISOweek, lubridate packages)
          if TRUE, gets consecutive dates by using the interval from the start date (i.e. months = 30.5, biweek = 14, bwn = 7)
        - start =  the start date set to Jan 1st 2002
          format_start = %d-%m-%Y' format of start date input
          year1 = 2002, corresponds to year of start date
        - if check = TRUE, plots distribution of days as a check to make sure function worked

        Dependencies:
        - vars: NA
        - packages: lubridate, ISOweek
        - scripts: none 

        Output:
        - returns: date or vector of dates
        - plots: if check == TRUE, distribution of dates in days as a check to make sure function worked appropriately
         
        ")
  }

  else {

    get.day <- function(date, format_date, start, format_start){
      st <- as.Date (start, format=format_start)
      d <- as.Date(date, format=format_date)
      day.check <- as.numeric(difftime(d, st, units="days"))
    }
    
    day.check <- get.day (date, format_date, start, format_start)
    par(mfrow = c(1, 1))
    par(mar=c(5,5,5,5))
    
    if (check == TRUE) {
      plot (day.check[order(day.check, decreasing=TRUE)], 1:length(day.check), xlab="date check")
    }
      
      if (tstep == "day"){
        return(get.day(date, format_date, start, format_start))
      }
    
      if (tstep == "month"){
      
        if (period == TRUE) {
          get.month.start <- function (date, format_date, start, format_start){
            day <- get.day (date, format_date, start, format_start)
            ceiling (day/30.5)
          }
        return (get.month.start(date, format_date, start, format_start))
      }
    
      else {
        get.month <- function (date, format_date, year1){
          d <- as.Date(date, format=format_date)
          yr <- year(d)-year1
          mon <- month(d) 
          mon + 12*yr
        }
        return (get.month(date, format_date, year1))  
      }
    }

    if (tstep == "bwn"){
      
      if (period == TRUE) {
          get.biweek.start <- function (date, format_date, start, format_start){
            day <- get.day (date, format_date, start, format_start)
            ceiling(day/14)
          }
        return (get.biweek.start(date, format_date, start, format_start))
      }
      
      else {
        get.bwn <- function (date, format_date, year1){
          d <- as.Date(date, format=format_date)
          yr <- year(d)-year1
          wk <- ISOweek (d)
          wk <- (as.numeric(substring (wk, 7, 8)))
          wk[wk==53] <- 52
          wk <- wk + 52*yr
          ceiling(wk/2)
        }
      return(get.bwn(date, format_date, year1))
      }
    }

    if (tstep == "week"){
      if (period == TRUE){
          get.week.start <- function (date, format_date, start, format_start){
            day <- get.day (date, format_date, start, format_start)
            ceiling(day/7)
          }
        return(get.week.start(date, format_date, start, format_start))
      }
      
      else {
        get.week <- function (date, format_date, year1){
          d <- as.Date(date, format=format_date)
          yr <- year(d)-year1
          wk <- ISOweek (d)
          wk <- (as.numeric(substring (wk, 7, 8)))
          wk[wk==53] <- 52
          wk + 52*yr
        }
        return(get.week(date, format_date, year1))
      }
    }
  }
}

# REP VILLNAMES WITH VILLAGE 2002 NAMES -----------------------------------
convert12_02 <- function(data){ # CONVERT VILLAGES FROM 2012 TO 2002! (data 2012)
  data$village <- as.character(data$village) # Convert to character - because otherwise a factor!
  
  # New villages since 2002 census:
  data$village[which(data$village == "Bokore")] <- "Kyambahi" # Bokore from Kyambahi (not Nyichoka)  
  data$village[which(data$village == "Hekwe")] <- "Kenyamonta" # Hekwe from Kenyamonta (not Magatini)
  data$village[which(data$village == "Kenokwe")] <- "Mosongo" # Kenokwe from Mosongo
  data$village[which(data$village == "Kerenero")] <- "Nyamoko" # Kerenero from Nyamoko (not Itununu)
  data$village[which(data$village == "Kitarungu")] <- "Nyansurura" # Kitarungu from Nyansurura
  data$village[which(data$village == "Manyata")] <- "Machochwe" # Manyata from Machochwe
  data$village[which(data$village == "Mbirikiri")] <- "Bonchugu" # Mbirikiri from Bonchugu
  data$village[which(data$village == "Nyamehuru")] <- "Busawe" # Nyamehuru from Busawe
  data$village[which(data$village == "Nyanungu")] <- "Kyambahi" # Nyanungu from Kyambahi (not Nyichoka)
  data$village[which(data$village == "Nyirongo")] <- "Nyamatare" # Nyirongo from Nyamatare
  data$village[which(data$village == "Sogoti")] <- "Kebanchebanche" # Sogoti from Kebanchebanche
  data$village[which(data$village == "Tamkeri")] <- "Mbalibali" # Tamkeri from Mbalibali (not Nyamburi)
  data$village[which(data$village == "Stendi Kuu")] <- "Mugumu" # Stendi Kuu from Mugumu
  # data$village[which(data$village == "Kerukerege")] <- "Maburi" # Kerukerege from Maburi
  # apparenly not, Kerukerege is a kitongoji in Maburi village
  data
}

## Bake without saving, adapted from POMP
bake.nosave <- function (file, expr, seed, kind = NULL, normal.kind = NULL) {
    rng.control <- !missing(seed)
    if (rng.control) {
      if (!exists(".Random.seed", envir = .GlobalEnv))
        set.seed(NULL)
      save.seed <- get(".Random.seed", envir = .GlobalEnv)
      set.seed(seed, kind = kind, normal.kind = normal.kind)
    }
    tmg <- system.time(val <- eval(expr))
    if (rng.control)
      assign(".Random.seed", save.seed, envir = .GlobalEnv)
    if (is.null(val)) {
      warning("in ", sQuote("bake"), ": expression evaluates to NULL",
              call. = FALSE)
      val <- paste0("NULL result returned by ", sQuote("bake"))
    }
    if (rng.control) {
      attr(val, "seed") <- seed
      attr(val, "kind") <- kind
      attr(val, "normal.kind") <- normal.kind
    }
    attr(val, "system.time") <- tmg
    saveRDS(val, file = file)
    val
}

## Match column names
match.colnames <- function (dataframe, lookup){
  colnames(dataframe) <- lookup$new.colnames[match(colnames(dataframe), 
                                                   lookup$og.colnames)]
  dataframe <- dataframe[ , !names(dataframe) %in% c("NULL")]
  return(dataframe)
}

## Adding alpha to colors
add.alpha <- function(col, alpha = 1){
  if(missing(col))
    stop("Please provide a vector of colours.")
  apply(sapply(col, col2rgb)/255, 2, 
        function(x) 
          rgb(x[1], x[2], x[3], alpha = alpha))  
}

## ggplot helpers
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