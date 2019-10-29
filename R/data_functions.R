############ DATA FUNCTIONS ########################################################################
## Malavika Rajeev
## March 2019

## Get days ------------------------------------------------------------------------------------
get.days <- function(dmat = dose_mat, doses_wide, threshold = 10) {
  date_mat <- matrix(NA, nrow(dmat), ncol(dmat))
  for (j in 1:ncol(date_mat)){
    rle(dmat[ , j]) %>%
      unclass() %>%
      as.data.frame() %>%
      mutate(end = cumsum(lengths),
             start = c(1, lag(end)[-1] + 1)) %>%
      filter(values == 0, lengths >= threshold) -> rles
    if(nrow(rles) > 0){
      for (i in 1:nrow(rles)){
        date_mat[rles$start[i]:rles$end[i], j] <- 0
      }
    }
  }
  
  date_mat <- replace(date_mat, is.na(date_mat), 1)
  date_mat %>% 
    as_tibble() %>%
    group_by(date = year(doses_wide$date_de_consultation)) %>%
    summarise_all(funs(sum(.)/n())) -> clinic_reporting
  colnames(clinic_reporting) <- colnames(doses_wide)[1:(ncol(doses_wide)-1)]
  
  clinic_reporting <- gather(clinic_reporting, id_ctar, prop, -date_de_consultation)
  clinic_reporting$ctar <- ctar_metadata$CTAR[match(clinic_reporting$id_ctar, 
                                                    ctar_metadata$id_ctar)]
  clinic_reporting$id_ctar <- as.numeric(clinic_reporting$id_ctar)
  clinic_reporting$threshold <- threshold
  return(list(clinic_reporting, date_mat))
}

## Get estimate of reporting annually  -----------------------------------------------------
get.reporting <- function(ctar_data, ctar_metadata, start_date = ymd("2014-01-01"), 
                          end_date = ymd("2017-12-31"), 
                          threshold = 10) {

  ctar_data %>%
    select(id_ctar, date_de_consultation) %>%
    drop_na(id_ctar) %>%
    mutate(date_de_consultation = ymd(date_de_consultation)) %>%
    gather(dose, date_de_consultation, -id_ctar) %>%
    group_by(id_ctar, date_de_consultation) %>%
    summarise(n = n()) -> patient_ts
  
  patient_ts <- patient_ts[!is.na(patient_ts$date_de_consultation), ]
  patient_ts$ctar <- ctar_metadata$CTAR[match(patient_ts$id_ctar, ctar_metadata$id_ctar)]
  patient_ts <- patient_ts[!is.na(patient_ts$ctar), ]
  
  ## getting reporting
  ts <- as_tibble(seq(start_date, end_date, by = "day"))
  
  patient_ts %>%
    right_join(ts, by = c("date_de_consultation" = "value")) %>%
    select(id_ctar, date_de_consultation, n) %>%
    spread(id_ctar, n) %>%
    replace(., is.na(.), 0) -> doses_wide
  
  dose_mat <- dmat <- as.matrix(doses_wide[, 2:(ncol(doses_wide) - 1)])
  
  date_mat <- matrix(NA, nrow(dose_mat), ncol(dose_mat))
  for (j in 1:ncol(date_mat)){
    rle(dose_mat[ , j]) %>%
      unclass() %>%
      as.data.frame() %>%
      mutate(end = cumsum(lengths),
             start = c(1, lag(end)[-1] + 1)) %>%
      filter(values == 0, lengths >= threshold) -> rles
    if(nrow(rles) > 0){
      for (i in 1:nrow(rles)){
        date_mat[rles$start[i]:rles$end[i], j] <- 0
      }
    }
  }
  
  date_mat <- check_mat <- replace(date_mat, is.na(date_mat), 1) 
  date_mat %>% 
    as_tibble() %>%
    group_by(year = year(doses_wide$date_de_consultation)) %>%
    summarise_all(funs(sum(.)/n())) -> clinic_reporting
  colnames(clinic_reporting) <- colnames(doses_wide)[1:(ncol(doses_wide)-1)]
  
  clinic_reporting <- gather(clinic_reporting, id_ctar, prop, -date_de_consultation)
  clinic_reporting <- rename(clinic_reporting, year = date_de_consultation)
  clinic_reporting$ctar <- ctar_metadata$CTAR[match(clinic_reporting$id_ctar, 
                                                    ctar_metadata$id_ctar)]
  clinic_reporting$id_ctar <- as.numeric(clinic_reporting$id_ctar)
  clinic_reporting$threshold <- threshold
  
  date_mat <- as.data.frame(date_mat)
  date_mat$date_de_consultation <- doses_wide$date_de_consultation
  date_mat <-melt(date_mat, id = "date_de_consultation")
  levels(date_mat$variable) <- names(doses_wide)[2:23]
  names(date_mat) <- c("date_de_consultation", "id_ctar", "exclude")
  
  return(list(clinic_reporting = clinic_reporting, patient_ts = patient_ts, 
              date_mat = date_mat, doses_wide = doses_wide, dose_mat = dmat, check_mat = check_mat))

}

## Get inclusion criteria for contact by date --------------------------------------------------
get.contacts <- function(ctar_data, times_sd) {
  
  ctar_data %>%
    select(id_ctar, date_de_consultation) %>%
    drop_na(id_ctar) %>%
    mutate(date_de_consultation = ymd(date_de_consultation)) %>%
    gather(dose, date_de_consultation, -id_ctar) %>%
    group_by(id_ctar, date_de_consultation) %>%
    summarise(n = n()) -> patient_ts
  
  patient_ts <- patient_ts[!is.na(patient_ts$date_de_consultation), ]
  patient_ts$ctar <- ctar_metadata$CTAR[match(patient_ts$id_ctar, ctar_metadata$id_ctar)]
  patient_ts <- patient_ts[!is.na(patient_ts$ctar), ]
  patient_ts %>%
    group_by(ctar) %>%
    mutate(exclude = ifelse(n > mean(n) + times_sd*sd(n), 1, 0)) -> patient_ts
  
  to_exclude <- c(grep("contact", unique(ctar_data$remarque), ignore.case = TRUE, value = TRUE),
                  grep("contam", unique(ctar_data$remarque), ignore.case = TRUE, value = TRUE),
                  grep("consom", unique(ctar_data$remarque), ignore.case = TRUE, value = TRUE),
                  grep("passage", unique(ctar_data$remarque), ignore.case = TRUE, value = TRUE))
  
  to_exclude <- to_exclude[-grep("date", to_exclude, ignore.case = TRUE)]
  
  ctar_data$exclude_bycomment <- 0
  ctar_data$exclude_bycomment[ctar_data$remarque %in% to_exclude] <- 1
  ctar_data <- mutate(ctar_data, date_de_consultation = ymd(date_de_consultation))
  patient_ts %>%
    ungroup() %>%
    select(id_ctar, date_de_consultation, exclude_bydate = exclude) %>%
    right_join(ctar_data) -> ctar_data
  return(list(ctar_data = ctar_data, patient_ts = patient_ts))
}

## Get bite data with covariates @ district level --------------------------------------------------
get.district.data <- function(ctar_data, ctar_metadata, covars, consec_days = 15, 
                              contact_cutoff = 3, reporting_thresh = 0.25, 
                              start = ymd("2014-01-01"), end = ymd("2017-12-31")) {
  
  ## Apply exclusion criteria
  ## 1. Get contacts by date and comments (applying SD cut-off)
  patient_data <- get.contacts(ctar_data, times_sd = contact_cutoff)[["ctar_data"]]
  
  ## 2. Get annual reporting by clinic
  clinic_reporting <- get.reporting(ctar_data, ctar_metadata, start_date = start, 
                                    end_date = end, 
                                    threshold = consec_days)[["clinic_reporting"]]
  ## 3. Exclude, apply reporting, join with covar data
  patient_data %>% 
    filter(exclude_bydate == 0, exclude_bycomment == 0) %>% # exclude contacts
    mutate_at(vars(starts_with("date")), funs(ymd(.))) %>% # format dates
    group_by(year = year(date_de_consultation), district, id_ctar) %>% # group by year and district
    summarize(n = n()) %>% # get count of exposures
    left_join(covars) %>% # add pop + ttimes + reporting
    left_join(clinic_reporting) %>%   # add reporting by clinic + year
    filter(exclude == 0, prop > reporting_thresh) %>% 
    ## filter out excluded catchments and ones with < 25% reporting
    group_by(district, year) %>%
    mutate(bites = n/prop) %>%
    summarise(bites = sum(bites, na.rm = TRUE)) %>%
    right_join(covars) %>%
    filter(exclude == 0) %>%
    replace_na(list(bites = 0)) -> exps
 return(exps)
}
  
# Get bite data with covariates for Moramanga  ----------------------------------------------------
#### Moramanga data! ###
get.morabites <- function(mada_communes) {

  ## ctar for # bites and also checking days to rule out contacts
  col_lookup <- read.csv("data/new_colnames.csv", header = TRUE)
  ctar <- read.csv("data/moramanga/ctar_up_todate.csv", header = TRUE)
  ctar <- match.colnames(ctar, subset(col_lookup, form == "ctar"))
  
  ## Set-up time series
  tfunc <- function (date, format_date = "%Y-%m-%d", start = "01-09-2016", format_start = "%d-%m-%Y",
                     year1 = 2016, tstep = "month", period = FALSE) {
    get.consec (date, format_date, start, format_start,
                year1, tstep, period) - 8 ## starts in Sep
  }
  
  ## all dates
  ctar %>%
    mutate_at(vars(starts_with("date")), funs(dmy(.))) -> ctar
  ctar$month <- tfunc(ctar$date.reported)
  ## subset to study period (Sep 2016 - to date)
  ## checks for cases where month is NA!
  ctar <- subset(ctar, month > 0 & month < 28)
  
  # ctar district and commune
  ctar$commune <- sapply(strsplit(as.character(ctar$patient.location), "\\("), "[", 1)
  ctar$commune <- trimws(ctar$commune, which = "right")
  ctar$district <- sapply(strsplit(as.character(ctar$patient.location), "\\, "), "[", 2)
  ctar$district <- gsub(" \\(District\\)", "", ctar$district)
  
  ## change all true/false to 0/1
  ctar[ctar == FALSE] <- 0
  
  ## Bite data in Moramanga
  n_months <- max(ctar$month, na.rm = TRUE)
  
  ctar %>%
    left_join(select(mada_communes@data, commune, mdg_cm_, mtch_ds), 
              by = c("commune" = "commune", "district" = "mtch_ds")) %>%
    filter(consult.type == "Animal bite/exposure") %>%
    group_by(mdg_cm_) %>%
    summarize(bites = round(n()/n_months*12)) %>%
    right_join(filter(comm_data, CTAR == "Moramanga")) %>% ## only districts within catchment (@ comm level)
    filter(!is.na(ttimes_weighted), 
           CTAR == "Moramanga") %>%
    replace_na(list(bites = 0)) -> mora_bites
  
  ### Moramanga model ###
  mora_coords <- cbind(ctar_metadata$LONGITUDE[ctar_metadata$CTAR == "Moramanga"], 
                       ctar_metadata$LATITUDE[ctar_metadata$CTAR == "Moramanga"])
  mada_communes@data %>%
    mutate(long = coordinates(mada_communes)[, 1], lat = coordinates(mada_communes)[, 2]) %>%
    select(commune, mdg_cm_, mtch_ds, long, lat) %>%
    right_join(mora_bites) -> mora_bites
  mora_bites %>% 
    mutate(distance = distm(select(mora_bites, long, lat), mora_coords)[, 1]/1000) -> mora_bites
  return(list(mora_bites = mora_bites, ctar = ctar))
}
