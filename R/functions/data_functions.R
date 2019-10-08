####################################################################################################
##' Functions for data wrangling/processing 
##' Details: for getting days to include, reporting, etc. 
##' Author: Malavika Rajeev 
####################################################################################################

##' Function for getting days to include via RLEs 
##' ------------------------------------------------------------------------------------------------
rle.days <- function(vec, threshold = 10) {
  
  rle(vec) %>%
    unclass() %>%
    as.data.frame() %>%
    mutate(end = cumsum(lengths),
    start = c(1, lag(end)[-1] + 1),
    include = ifelse(values == 0 &
    lengths >= threshold, 0, 1)) -> rles
  
  include <- rep(NA, length(vec))
  
  for(i in 1:nrow(rles)) {
    include[rles$start[i]:rles$end[i]] <- rles$include[i]
  }
  
  return(include)
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
    group_by(date = year(doses_wide$date_de_consultation)) %>%
    summarise_all(funs(sum(.)/n())) -> clinic_reporting
  colnames(clinic_reporting) <- colnames(doses_wide)[1:(ncol(doses_wide)-1)]
  
  clinic_reporting <- gather(clinic_reporting, id_ctar, prop, -date_de_consultation)
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
