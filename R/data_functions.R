
# Function for getting days to include via RLEs
rle.days <- function(vec, threshold = 10) {
  rle(vec) %>%
    unclass() %>%
    as.data.frame() %>%
    mutate(
      end = cumsum(lengths),
      start = c(1, lag(end)[-1] + 1),
      include = ifelse(values == 0 &
        lengths >= threshold, 0, 1)
    ) -> rles

  include <- rep(NA, length(vec))

  for (i in 1:nrow(rles)) {
    include[rles$start[i]:rles$end[i]] <- rles$include[i]
  }

  return(include)
}

ctar_to_exclude <- function(national, ctar_metadata, min_forms = 10) {
  national %>%
    filter(
      year(date_reported) > 2013, year(date_reported) < 2018, !is.na(distcode),
      !is.na(id_ctar)
    ) %>%
    mutate(date_reported = ymd(date_reported)) %>%
    group_by(id_ctar) %>%
    summarize(total_forms = n()) %>%
    complete(id_ctar = ctar_metadata$id_ctar, fill = list(total_forms = 0)) %>%
    right_join(ctar_metadata) %>%
    mutate(
      total_forms = ifelse(is.na(total_forms), 0, total_forms),
      exclude_dist = ifelse(total_forms > 10, 0, 1)
    ) -> ctar_metadata

  return(ctar_metadata)
}

# Function for getting bite estimates?
clean_natl <- function(national, mada_districts, mada_communes, ctar_metadata,
                       reporting_thresh = 0.25,
                       tput_thresh = 15) {

  # Getting daily throughput for each clinic
  national %>%
    filter(year(date_reported) > 2013, year(date_reported) < 2018, !is.na(distcode), !is.na(id_ctar)) %>%
    mutate(date_reported = ymd(date_reported)) -> national

  national %>%
    group_by(date_reported, id_ctar) %>%
    summarise(no_patients = n()) %>%
    ungroup() %>%
    complete(
      date_reported = seq(min(date_reported), max(date_reported), by = "day"), id_ctar,
      fill = list(no_patients = 0)
    ) -> throughput

  # rle.days = Helper function for getting which days to include (moved to functions from data_functions.R)
  throughput %>%
    group_by(id_ctar) %>%
    arrange(date_reported, .by_group = TRUE) %>%
    mutate(
      include_day = rle.days(no_patients, threshold = tput_thresh),
      mean_throughput = mean(no_patients[include_day == 1]),
      sd_throughput = sd(no_patients[include_day == 1]),
      year = year(date_reported)
    ) -> throughput

  # yearly reporting
  # sum the total # of days included over # of days in year (365)
  throughput %>%
    group_by(year, id_ctar) %>%
    summarize(reporting = sum(include_day) / 365) -> reporting

  # Left join with throughput to get exclusion criteria
  national %>%
    filter(
      distcode %in% mada_districts$distcode,
      id_ctar %in% ctar_metadata$id_ctar[!is.na(ctar_metadata$id_ctar)],
      type == "new"
    ) -> national

  national %>%
    left_join(select(throughput, date_reported, id_ctar, include_day, year)) -> bites

  # Getting district level exclusion criteria
  # if submitted less than 10 forms total
  national %>%
    group_by(id_ctar) %>%
    summarize(total_forms = n()) %>%
    complete(id_ctar = ctar_metadata$id_ctar, fill = list(total_forms = 0)) %>%
    right_join(ctar_metadata) %>%
    mutate(
      total_forms = ifelse(is.na(total_forms), 0, total_forms),
      exclude_dist = ifelse(total_forms > 10, 0, 1)
    ) -> ctar_metadata

  # Getting bite incidence estimates for all districts
  bites %>%
    # filter based on throughput
    filter(include_day == 1) %>%
    group_by(year, distcode) %>%
    summarize(bites = n()) -> bites_district
  bites_district$CTAR <- mada_districts$catchment[match(
    bites_district$distcode,
    mada_districts$distcode
  )]
  bites_district$id_ctar <- ctar_metadata$id_ctar[match(bites_district$CTAR, ctar_metadata$CTAR)]
  bites_district %>%
    left_join(reporting) %>%
    filter(reporting > reporting_thresh) %>% # dont include any district for which catchment clinic had
    # less than 25% reporting
    # correct for reporting by year and ctar reported to
    mutate(bites = bites / reporting) %>%
    group_by(distcode) %>%
    summarize(
      avg_bites = mean(bites, na.rm = TRUE),
      sd_bites = sd(bites, na.rm = TRUE),
      min_bites = min(bites, na.rm = TRUE),
      max_bites = max(bites, na.rm = TRUE),
      nobs = n()
    ) %>%
    complete(distcode = mada_districts$distcode, fill = list(avg_bites = 0)) -> bite_ests

  # Join bites with district and commune covariates
  mada_districts %>%
    filter(exclude_dist == 0) %>%
    mutate(
      catch = as.numeric(droplevels(factor(catchment))),
      group = as.numeric(droplevels(factor(distcode)))
    ) %>%
    left_join(bite_ests) %>%
    arrange(group) %>%
    st_drop_geometry() -> district_bites

  mada_communes %>%
    filter(exclude_dist == 0) %>%
    mutate(
      catch = as.numeric(droplevels(factor(ctch_dist))),
      group = as.numeric(droplevels(factor(distcode)))
    ) %>%
    arrange(group) %>%
    st_drop_geometry() -> comm_covars

  district_bites$end <- cumsum(rle(comm_covars$group)$lengths)
  district_bites$start <- c(1, lag(district_bites$end)[-1] + 1)

  return(list(comm_covars = comm_covars, district_bites = district_bites))
}

# Moramanga data
clean_mora <- function(moramanga, mada_communes, mada_districts, district_bites) {

  # Communes
  mada_communes %>%
    left_join(select(st_drop_geometry(mada_districts), distcode, exclude_dist,
      ctch_dist = catchment
    )) -> mada_communes

  moramanga %>%
    mutate(month_date = floor_date(ymd(moramanga$date_reported), unit = "month")) %>%
    filter(
      type == "new", month_date >= "2016-10-01",
      month_date <= "2019-06-01", !is.na(commcode)
    ) %>%
    group_by(commcode, month_date) %>%
    summarize(bites = n()) %>%
    ungroup() %>%
    complete(
      month_date = seq(min(month_date), max(month_date), by = "month"),
      commcode, fill = list(bites = 0)
    ) %>%
    group_by(commcode) %>%
    summarize(
      avg_bites = mean(bites) * 12,
      nobs = n()
    ) %>% # average monthly bites x 12 to get annual avg_bites
    complete(commcode = mada_communes$commcode, fill = list(avg_bites = 0)) -> mora_bites

  mada_communes %>%
    filter(catchment == "Moramanga") %>%
    left_join(mora_bites) %>%
    st_drop_geometry() -> mora_bites
  mora_bites$catch <- district_bites$catch[district_bites$catchment == "Moramanga"][1]

  return(mora_bites)
}
